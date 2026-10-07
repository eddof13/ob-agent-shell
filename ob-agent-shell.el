;;; ob-agent-shell.el --- Org-babel backend for agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eddie Jesinsky

;; Author: Eddie Jesinsky <eddie@jesinsky.com>
;; Assisted-by: Claude Code
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.85.3") (org "9.6"))
;; Keywords: tools, convenience, outlines
;; URL: https://github.com/eddof13/ob-agent-shell
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-babel backend that sends source blocks to an existing agent-shell
;; buffer and captures the response as the block result.  Reuses your
;; configured agent-shell client rather than requiring a separate AI setup.
;;
;; Usage:
;;
;;   #+begin_src agent-shell
;;   What is the capital of France?
;;   #+end_src
;;
;;   #+RESULTS:
;;   : Paris.
;;
;; Setup:
;;
;;   (require 'ob-agent-shell)
;;   (add-to-list 'org-babel-load-languages '(agent-shell . t))
;;
;; Header args:
;;
;;   :buffer BUFFER-NAME  Use a specific agent-shell buffer by name.
;;                        Takes priority over :session.
;;
;;   :session NAME        Route all blocks sharing NAME to the same
;;                        agent-shell buffer.  On first use, binds NAME
;;                        to the currently active buffer; subsequent
;;                        blocks reuse it.  Set this file-wide via
;;                        #+PROPERTY: header-args:agent-shell :session ID
;;                        to give every block in a file its own shell.
;;
;;   :timeout N           Override `ob-agent-shell-timeout' for this block.
;;                        N is a number of seconds.  Useful for long-running
;;                        prompts (e.g. reading a full PDF) without raising
;;                        the global default.
;;
;;   :model ID-OR-NAME    Switch the session model before sending.  The change
;;                        sticks for later blocks and interactive use.
;;
;;   :thought-level ID-OR-NAME
;;                        Switch the session thought level before sending.
;;                        Sticks the same way.  Errors when the agent does
;;                        not advertise one.
;;
;;   :context TEXT        Prepend TEXT to the block body.  A single token that
;;                        names an Org element uses that element's body.
;;
;;   :results raw         Omit the leading ": " prefix on each result line.

;;; Code:

(require 'agent-shell)
(require 'ob)
(require 'org-element)
(require 'map)

(declare-function org-in-commented-heading-p "org")

;; Public agent-shell config API (0.85.3+; see xenodium/agent-shell#860).
(declare-function agent-shell-config-option "agent-shell")
(declare-function agent-shell-config-option-value "agent-shell")
(declare-function agent-shell-set-config-option-value "agent-shell")

(defgroup ob-agent-shell nil
  "Org-babel integration for `agent-shell'."
  :group 'org-babel
  :prefix "ob-agent-shell-")

(defcustom ob-agent-shell-timeout 30
  "Seconds to wait for `agent-shell' to respond before signaling an error."
  :type 'integer
  :group 'ob-agent-shell)

(defcustom ob-agent-shell-convert-markdown nil
  "When non-nil, convert markdown responses to `org-mode' format via pandoc.
Requires pandoc to be installed and available on PATH."
  :type 'boolean
  :group 'ob-agent-shell)

;;; Buffer resolution

(defvar ob-agent-shell--sessions (make-hash-table :test #'equal)
  "Registry mapping session names to their `agent-shell' buffers.
Entries are added on first use of a :session name and removed when
the associated buffer is no longer live.")

(defun ob-agent-shell--resolve-session (name)
  "Return the `agent-shell' buffer for session NAME, registering it if new.
On first call for NAME, binds NAME to the currently active `agent-shell'
buffer.  On subsequent calls, returns the same buffer as long as it
remains live.  Signals an error if no active buffer can be found."
  (let ((buf (gethash name ob-agent-shell--sessions)))
    (if (and buf (buffer-live-p buf) (process-live-p (get-buffer-process buf)))
        buf
      (when buf
        (remhash name ob-agent-shell--sessions))
      (let ((active (agent-shell-shell-buffer :no-error t)))
        (unless active
          (user-error "No agent-shell buffer found for session %S; start one with M-x agent-shell"
                      name))
        (puthash name active ob-agent-shell--sessions)
        active))))

(defun ob-agent-shell--resolve-buffer (&optional name session)
  "Return the `agent-shell' buffer to use, or signal an error.
Resolution order: :buffer NAME, then :session SESSION, then most recently used."
  (if (and session (not name))
      (ob-agent-shell--resolve-session session)
    (let ((buf (if name (get-buffer name) (agent-shell-shell-buffer :no-error t))))
      (cond
       ((null buf)
        (user-error "No agent-shell buffer%s found; start one with M-x agent-shell"
                    (if name (format " named %S" name) "")))
       ((not (buffer-live-p buf))
        (user-error "Agent-shell buffer %S is no longer live" (buffer-name buf)))
       ((not (process-live-p (get-buffer-process buf)))
        (user-error "Agent-shell buffer %S has no live process; restart with M-x agent-shell"
                    (buffer-name buf)))
       (t buf)))))

;;; Response extraction

(defun ob-agent-shell--strip-ui-fragments (text)
  "Return TEXT keeping only agent_message_chunk spans and untagged text.
Current `agent-shell' tags all buffer content with an `agent-shell-ui-state'
alist whose :qualified-id identifies the span type.  Only spans ending in
\"agent_message_chunk\" are response prose; others (tool calls, thinking
indicators, etc.) are UI-only and should be omitted."
  (let ((pos 0)
        (len (length text))
        parts)
    (while (< pos len)
      (let* ((state (get-text-property pos 'agent-shell-ui-state text))
             (qid (and state (cdr (assq :qualified-id state))))
             (keep (or (null state)
                       (and qid (string-suffix-p "agent_message_chunk" qid))))
             (next (or (next-single-property-change
                        pos 'agent-shell-ui-state text len)
                       len)))
        (when keep
          (push (substring-no-properties text pos next) parts))
        (setq pos next)))
    (string-trim (apply #'concat (nreverse parts)))))

(defun ob-agent-shell--extract-response (buf)
  "Extract plain agent response text from BUF, skipping tool-call UI fragments.

Uses `agent-shell-interaction-at-point' to obtain the response, then
strips any spans carrying the `agent-shell-ui-state' text property
\(tool-call blocks, thinking indicators, etc.)."
  (with-current-buffer buf
    (save-excursion
      (agent-shell-goto-last-interaction)
      (when-let* ((interaction (agent-shell-interaction-at-point))
                  (response (cdr (assq :response interaction))))
        (ob-agent-shell--strip-ui-fragments response)))))

;;; Markdown conversion

(defun ob-agent-shell--maybe-convert (text)
  "Convert TEXT from markdown to `org-mode' format if configured to do so.
Requires `ob-agent-shell-convert-markdown' to be non-nil and pandoc on PATH."
  (if (and ob-agent-shell-convert-markdown (executable-find "pandoc"))
      (with-temp-buffer
        (insert text)
        (shell-command-on-region (point-min) (point-max)
                                 "pandoc -f markdown -t org"
                                 (current-buffer) t)
        (string-trim (buffer-string)))
    text))

;;; Prompt and session options

(defun ob-agent-shell--element-body (element)
  "Return the text contents of Org ELEMENT, or nil."
  (pcase (org-element-type element)
    ((or 'src-block 'example-block 'verse-block 'fixed-width)
     (org-element-property :value element))
    ((or 'quote-block 'paragraph)
     (when-let* ((beg (org-element-property :contents-begin element))
                 (end (org-element-property :contents-end element)))
       (buffer-substring-no-properties beg end)))))

(defun ob-agent-shell--named-context (name)
  "Return the body of the Org element named NAME, or nil.
NAME must be a single token.  The element is not executed."
  (when (and (derived-mode-p 'org-mode)
             (string-match-p "\\`[[:alnum:]_-]+\\'" name))
    (org-with-wide-buffer
      (goto-char (point-min))
      (let ((regexp (org-babel-named-data-regexp-for-name name))
            body)
        (while (and (not body) (re-search-forward regexp nil t))
          (unless (org-in-commented-heading-p)
            (let ((element (org-element-at-point)))
              (when (equal (org-element-property :name element) name)
                (goto-char (org-element-post-affiliated element))
                (setq element (org-element-at-point))
                (setq body (ob-agent-shell--element-body element))))))
        (and body (let ((trimmed (string-trim body)))
                    (unless (string-blank-p trimmed) trimmed)))))))

(defun ob-agent-shell--context-text (context)
  "Return prompt text for CONTEXT, or nil when CONTEXT is empty.
A single token that names an Org element expands to that element's body.
Any other value is literal text."
  (when context
    (let* ((raw (if (stringp context) context (format "%s" context)))
           (named (ob-agent-shell--named-context raw))
           (text (string-trim (or named raw))))
      (unless (string-blank-p text) text))))

(defun ob-agent-shell--compose-prompt (body context)
  "Return the prompt sent for BODY with CONTEXT prepended.
CONTEXT is a header value, not yet resolved.  BODY is the source block."
  (let ((ctx (ob-agent-shell--context-text context))
        (body (or body "")))
    (cond
     ((and ctx (not (string-blank-p body)))
      (concat ctx "\n\n" body))
     (ctx ctx)
     (t body))))

(defun ob-agent-shell--matching-choices (wanted items id-fn name-fn)
  "Return ITEMS whose id or name matches WANTED.
ID-FN and NAME-FN read each item.  An exact id match is preferred over
a display name, and display names match case-insensitively."
  (let ((ids nil)
        (names nil))
    (dolist (item items)
      (when (equal wanted (funcall id-fn item))
        (push item ids))
      (when (and (funcall name-fn item)
                 (string-equal (downcase wanted)
                               (downcase (funcall name-fn item))))
        (push item names)))
    (or (nreverse ids) (nreverse names))))

(defun ob-agent-shell--resolve-choice (wanted items id-fn name-fn kind)
  "Return the id in ITEMS selected by WANTED.
ID-FN and NAME-FN read each item.  KIND labels errors, for example
\"model\".  Signal `user-error' when WANTED matches nothing or matches
more than one display name."
  (let ((matches (ob-agent-shell--matching-choices wanted items id-fn name-fn)))
    (cond
     ((null matches)
      (user-error "Unknown %s %S; choices: %s"
                  kind wanted
                  (if items
                      (mapconcat (lambda (item)
                                   (format "%s (%s)"
                                           (or (funcall name-fn item)
                                               (funcall id-fn item))
                                           (funcall id-fn item)))
                                 items ", ")
                    "none")))
     ((and (cdr matches)
           (not (equal wanted (funcall id-fn (car matches)))))
      (user-error "Ambiguous %s name %S; use an id (%s)"
                  kind wanted
                  (mapconcat id-fn matches ", ")))
     (t (funcall id-fn (car matches))))))

(defun ob-agent-shell--config-choices (category)
  "Return advertised option values for CATEGORY, or nil if none.
CATEGORY is an ACP category such as \"model\" or \"thought_level\".
Call with the shell buffer current.  Each choice is an alist with
:value and optional :name."
  (when-let* ((option (agent-shell-config-option :category category)))
    (map-elt option :options)))

(defun ob-agent-shell--resolve-model-id (model)
  "Return the session model id for MODEL, or nil when MODEL is unset.
MODEL is an id or a display name advertised by the current session.
Call with the shell buffer current."
  (when (and model (not (string-blank-p (if (stringp model) model (format "%s" model)))))
    (ob-agent-shell--resolve-choice
     (if (stringp model) model (format "%s" model))
     (ob-agent-shell--config-choices "model")
     (lambda (item) (map-elt item :value))
     (lambda (item) (map-elt item :name))
     "model")))

(defun ob-agent-shell--resolve-thought-id (thought)
  "Return the thought-level id for THOUGHT, or nil when THOUGHT is unset.
THOUGHT is an id or a display name.  Signal `user-error' when the agent
advertises no thought level.  Call with the shell buffer current."
  (when (and thought (not (string-blank-p (if (stringp thought) thought (format "%s" thought)))))
    (let ((levels (ob-agent-shell--config-choices "thought_level")))
      (unless levels
        (user-error "Agent does not advertise a thought level"))
      (ob-agent-shell--resolve-choice
       (if (stringp thought) thought (format "%s" thought))
       levels
       (lambda (item) (map-elt item :value))
       (lambda (item) (map-elt item :name))
       "thought level"))))

(defun ob-agent-shell--acp-error-message (acp-error)
  "Return a short string for ACP-ERROR."
  (cond
   ((stringp acp-error) acp-error)
   ((listp acp-error)
    (or (map-elt acp-error 'message)
        (map-elt acp-error :message)
        (format "%S" acp-error)))
   (t (format "%S" acp-error))))

(defun ob-agent-shell--set-config-value (category value kind on-success on-error)
  "Set CATEGORY config option to VALUE, then call ON-SUCCESS.
KIND labels errors, for example \"model\".  Skip the request when VALUE
is nil or already current.  ON-ERROR receives a message string.  Call
with the shell buffer current."
  (cond
   ((null value) (funcall on-success))
   ((equal value (agent-shell-config-option-value :category category))
    (funcall on-success))
   (t
    (condition-case err
        (agent-shell-set-config-option-value
         :category category
         :value value
         :on-success (lambda (_result) (funcall on-success))
         :on-failure
         (lambda (result)
           (funcall on-error
                    (format "Failed to set %s %s: %s"
                            kind value
                            (ob-agent-shell--acp-error-message
                             (map-elt result :acp-error))))))
      (error (funcall on-error (error-message-string err)))))))

(defun ob-agent-shell--set-model-id (model-id on-success on-error)
  "Set the session model to MODEL-ID, then call ON-SUCCESS.
Skip the request when MODEL-ID is nil or already current.  ON-ERROR
receives a message string.  Call with the shell buffer current."
  (ob-agent-shell--set-config-value
   "model" model-id "model" on-success on-error))

(defun ob-agent-shell--set-thought-id (thought-id on-success on-error)
  "Set the session thought level to THOUGHT-ID, then call ON-SUCCESS.
Skip the request when THOUGHT-ID is nil or already current.  ON-ERROR
receives a message string.  Call with the shell buffer current."
  (ob-agent-shell--set-config-value
   "thought_level" thought-id "thought level" on-success on-error))

(defun ob-agent-shell--apply-session-options (shell-buf model thought on-success on-error)
  "Point SHELL-BUF at MODEL and THOUGHT, then call ON-SUCCESS.
MODEL and THOUGHT are header values, or nil.  ON-ERROR receives a
message string.  Model is applied before thought level."
  (with-current-buffer shell-buf
    (condition-case err
        (let ((model-id (ob-agent-shell--resolve-model-id model))
              (thought-id (ob-agent-shell--resolve-thought-id thought)))
          (ob-agent-shell--set-model-id
           model-id
           (lambda ()
             (with-current-buffer shell-buf
               (ob-agent-shell--set-thought-id thought-id on-success on-error)))
           on-error))
      (error (funcall on-error (error-message-string err))))))

;;; Subscription cleanup

(defun ob-agent-shell--unsubscribe-all (shell-buf tokens)
  "Remove all subscription TOKENS from SHELL-BUF."
  (with-current-buffer shell-buf
    (dolist (tok tokens)
      (when tok
        (agent-shell-unsubscribe :subscription tok)))))

;;; Core execution

(defun ob-agent-shell--exit-recursive-edit-if-active ()
  "Exit the innermost recursive edit if one is active."
  (when (> (recursion-depth) 0)
    (exit-recursive-edit)))

(defun org-babel-execute:agent-shell (body params)
  "Execute BODY by sending it to the active `agent-shell' buffer.
PARAMS may include :buffer to target a specific buffer by name,
:timeout to override `ob-agent-shell-timeout' for this block, :model
and :thought-level to switch the session before sending, and :context
to prepend text to BODY."
  (let* ((prompt (ob-agent-shell--compose-prompt body (cdr (assq :context params))))
         (shell-buf (unless (string-blank-p prompt)
                      (ob-agent-shell--resolve-buffer (cdr (assq :buffer params))
                                                      (cdr (assq :session params)))))
         (timeout (or (cdr (assq :timeout params)) ob-agent-shell-timeout))
         (model (cdr (assq :model params)))
         (thought (cdr (assq :thought-level params)))
         (result nil)
         (err nil)
         (done nil)
         (options-ready nil)
         (waiting-for-permission nil)
         (tokens nil)
         (timeout-timer nil))
    (if (string-blank-p prompt)
        (org-babel-remove-result)
      (unwind-protect
          (progn
            (setq timeout-timer
                  (run-at-time timeout nil
                               (lambda ()
                                 (unless waiting-for-permission
                                   (setq err (format "ob-agent-shell: timed out after %ds"
                                                     timeout)
                                         done t)))))
            (ob-agent-shell--apply-session-options
             shell-buf model thought
             (lambda () (setq options-ready t))
             (lambda (message)
               (setq err message
                     done t
                     options-ready t)))
            (while (not (or options-ready done))
              (unless (sit-for 0.1)
                (setq err "ob-agent-shell: aborted by user input" done t)))
            (when (and options-ready (not err))
              (push (agent-shell-subscribe-to
                     :shell-buffer shell-buf
                     :event 'turn-complete
                     :on-event (lambda (_data)
                                 (let ((was-waiting waiting-for-permission))
                                   (setq waiting-for-permission nil)
                                   (if-let* ((response (ob-agent-shell--extract-response shell-buf)))
                                       (setq result (ob-agent-shell--maybe-convert response)
                                             done t)
                                     (setq err "ob-agent-shell: no response found"
                                           done t))
                                   (when was-waiting
                                     (run-at-time 0 nil #'ob-agent-shell--exit-recursive-edit-if-active)))))
                    tokens)
              (push (agent-shell-subscribe-to
                     :shell-buffer shell-buf
                     :event 'error
                     :on-event (lambda (data)
                                 (let ((was-waiting waiting-for-permission))
                                   (setq waiting-for-permission nil
                                         err (format "ob-agent-shell error [%s]: %s"
                                                     (map-elt data :code)
                                                     (map-elt data :message))
                                         done t)
                                   (when was-waiting
                                     (run-at-time 0 nil #'ob-agent-shell--exit-recursive-edit-if-active)))))
                    tokens)
              (push (agent-shell-subscribe-to
                     :shell-buffer shell-buf
                     :event 'permission-request
                     :on-event (lambda (_data)
                                 (setq waiting-for-permission t)
                                 (pop-to-buffer shell-buf)
                                 (condition-case nil
                                     (unless done
                                       (recursive-edit))
                                   (quit
                                    (setq err "ob-agent-shell: aborted by user" done t)))
                                 (setq waiting-for-permission nil)))
                    tokens)
              (save-window-excursion
                (agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer shell-buf)
                (while (not done)
                  (unless (sit-for 0.1)
                    (setq err "ob-agent-shell: aborted by user input" done t))))))
        (when timeout-timer (cancel-timer timeout-timer))
        (when shell-buf
          (ob-agent-shell--unsubscribe-all shell-buf tokens)))
      (when err (user-error err))
      result)))

;;; Org-babel boilerplate

(defvar org-babel-default-header-args:agent-shell
  '((:results . "output drawer") (:exports . "both"))
  "Default header arguments for `agent-shell' source blocks.")

(defun org-babel-prep-session:agent-shell (_session _params)
  "Use :session for buffer routing, not interactive sessions."
  (user-error "`ob-agent-shell' does not open interactive sessions; use :session to route blocks to a named buffer"))

(provide 'ob-agent-shell)
;;; ob-agent-shell.el ends here
