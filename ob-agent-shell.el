;;; ob-agent-shell.el --- Org-babel backend for agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eddie Jesinsky

;; Author: Eddie Jesinsky <eddie@jesinsky.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "20260424.1630") (org "9.6"))
;; Keywords: ai, org, babel
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/eddof13/ob-agent-shell

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
;;   :results raw         Omit the leading ": " prefix on each result line.

;;; Code:

(require 'agent-shell)
(require 'ob)
(require 'map)

(defgroup ob-agent-shell nil
  "Org-babel integration for agent-shell."
  :group 'org-babel
  :prefix "ob-agent-shell-")

(defcustom ob-agent-shell-timeout 30
  "Seconds to wait for agent-shell to respond before signaling an error."
  :type 'integer
  :group 'ob-agent-shell)

(defcustom ob-agent-shell-convert-markdown nil
  "When non-nil, convert markdown responses to org-mode format via pandoc.
Requires pandoc to be installed and available on PATH."
  :type 'boolean
  :group 'ob-agent-shell)

;;; Buffer resolution

(defvar ob-agent-shell--sessions (make-hash-table :test #'equal)
  "Registry mapping session names to their agent-shell buffers.
Entries are added on first use of a :session name and removed when
the associated buffer is no longer live.")

(defun ob-agent-shell--resolve-session (name)
  "Return the agent-shell buffer for session NAME, registering it if new.
On first call for NAME, binds NAME to the currently active agent-shell
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
  "Return the agent-shell buffer to use, or signal an error.
Resolution order: :buffer NAME, then :session SESSION, then most recently used."
  (if (and session (not name))
      (ob-agent-shell--resolve-session session)
    (let ((buf (if name (get-buffer name) (agent-shell-shell-buffer :no-error t))))
      (cond
       ((null buf)
        (user-error "No agent-shell buffer%s found; start one with M-x agent-shell"
                    (if name (format " named %S" name) "")))
       ((not (buffer-live-p buf))
        (user-error "agent-shell buffer %S is no longer live" (buffer-name buf)))
       ((not (process-live-p (get-buffer-process buf)))
        (user-error "agent-shell buffer %S has no live process; restart with M-x agent-shell"
                    (buffer-name buf)))
       (t buf)))))

;;; Response extraction

(defun ob-agent-shell--strip-ui-fragments (text)
  "Return TEXT keeping only agent_message_chunk spans and untagged text.
Current agent-shell tags all buffer content with an `agent-shell-ui-state'
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
  "Convert TEXT from markdown to org-mode format if configured to do so.
Requires `ob-agent-shell-convert-markdown' to be non-nil and pandoc on PATH."
  (if (and ob-agent-shell-convert-markdown (executable-find "pandoc"))
      (with-temp-buffer
        (insert text)
        (shell-command-on-region (point-min) (point-max)
                                 "pandoc -f markdown -t org"
                                 (current-buffer) t)
        (string-trim (buffer-string)))
    text))

;;; Subscription cleanup

(defun ob-agent-shell--unsubscribe-all (shell-buf tokens)
  "Remove all subscription TOKENS from SHELL-BUF."
  (with-current-buffer shell-buf
    (dolist (tok tokens)
      (when tok
        (agent-shell-unsubscribe :subscription tok)))))

;;; Core execution

(defun org-babel-execute:agent-shell (body params)
  "Execute BODY by sending it to the active agent-shell buffer.
PARAMS may include :buffer to target a specific buffer by name and
:timeout to override `ob-agent-shell-timeout' for this block."
  (let* ((shell-buf (ob-agent-shell--resolve-buffer (cdr (assq :buffer params))
                                                    (cdr (assq :session params))))
         (timeout (or (cdr (assq :timeout params)) ob-agent-shell-timeout))
         (result nil)
         (err nil)
         (done nil)
         (tokens nil)
         (timeout-timer nil))
    (unwind-protect
        (progn
          (setq timeout-timer
                (run-at-time timeout nil
                             (lambda ()
                               (setq err (format "ob-agent-shell: timed out after %ds"
                                                 timeout)
                                     done t))))
          (push (agent-shell-subscribe-to
                 :shell-buffer shell-buf
                 :event 'turn-complete
                 :on-event (lambda (_data)
                             (if-let ((response (ob-agent-shell--extract-response shell-buf)))
                                 (setq result (ob-agent-shell--maybe-convert response)
                                       done t)
                               (setq err "ob-agent-shell: no response found"
                                     done t))))
                tokens)
          (push (agent-shell-subscribe-to
                 :shell-buffer shell-buf
                 :event 'error
                 :on-event (lambda (data)
                             (setq err (format "ob-agent-shell error [%s]: %s"
                                               (map-elt data :code)
                                               (map-elt data :message))
                                   done t)))
                tokens)
          (push (agent-shell-subscribe-to
                 :shell-buffer shell-buf
                 :event 'permission-request
                 :on-event (lambda (_data)
                             (message "ob-agent-shell: waiting for permission in %s"
                                      (buffer-name shell-buf))))
                tokens)
          (save-window-excursion
            (agent-shell-insert :text body :submit t :no-focus t :shell-buffer shell-buf)
            (while (not done)
              (unless (sit-for 0.1)
                (setq err "ob-agent-shell: aborted by user input" done t)))))
      (when timeout-timer (cancel-timer timeout-timer))
      (ob-agent-shell--unsubscribe-all shell-buf tokens))
    (when err (user-error err))
    result))

;;; Org-babel boilerplate

(defvar org-babel-default-header-args:agent-shell
  '((:results . "output") (:exports . "both"))
  "Default header arguments for agent-shell source blocks.")

(defun org-babel-prep-session:agent-shell (_session _params)
  "ob-agent-shell uses :session for buffer routing, not interactive sessions."
  (user-error "ob-agent-shell does not open interactive sessions; use :session to route blocks to a named buffer"))

(provide 'ob-agent-shell)
;;; ob-agent-shell.el ends here
