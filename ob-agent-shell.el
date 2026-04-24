;;; ob-agent-shell.el --- Org-babel backend for agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eddie Jesinsky

;; Author: Eddie Jesinsky <eddie@jesinsky.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "20260424") (org "9.6"))
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
;;                        Defaults to the most recently used shell buffer.
;;
;;   :results raw         Omit the leading ": " prefix on each result line.

;;; Code:

(require 'agent-shell)
(require 'ob)
(require 'seq)
(require 'map)

(defgroup ob-agent-shell nil
  "Org-babel integration for agent-shell."
  :group 'org-babel
  :prefix "ob-agent-shell-")

(defcustom ob-agent-shell-timeout 30
  "Seconds to wait for agent-shell to respond before signaling an error."
  :type 'integer
  :group 'ob-agent-shell)

;;; Buffer resolution

(defun ob-agent-shell--resolve-buffer (&optional name)
  "Return the agent-shell buffer to use, or signal an error.
If NAME is non-nil, find a live buffer by that name.
Otherwise return the most recently used agent-shell buffer."
  (let ((buf (if name (get-buffer name) (seq-first (agent-shell-buffers)))))
    (cond
     ((null buf)
      (user-error "No agent-shell buffer%s found; start one with M-x agent-shell"
                  (if name (format " named %S" name) "")))
     ((not (buffer-live-p buf))
      (user-error "agent-shell buffer %S is no longer live" (buffer-name buf)))
     ((not (process-live-p (get-buffer-process buf)))
      (user-error "agent-shell buffer %S has no live process; restart with M-x agent-shell"
                  (buffer-name buf)))
     (t buf))))

;;; Response extraction

(defun ob-agent-shell--extract-response (buf)
  "Extract plain agent response text from BUF, skipping tool-call UI fragments.

Positions to the last interaction, finds the response region, then walks
it collecting only spans without the `agent-shell-ui-state' property —
the property agent-shell sets on every tool-call block."
  (with-current-buffer buf
    (save-excursion
      (agent-shell-goto-last-interaction)
      (when (search-forward "<shell-maker-end-of-prompt>" nil t)
        (let* ((response-start (point))
               (prompt-re (map-elt (agent-shell-get-config buf) :shell-prompt-regexp))
               (response-end (if (and prompt-re
                                      (re-search-forward prompt-re nil t))
                                 (match-beginning 0)
                               (point-max)))
               (pos response-start)
               parts)
          (while (< pos response-end)
            (if (get-text-property pos 'agent-shell-ui-state)
                (setq pos (or (next-single-property-change
                               pos 'agent-shell-ui-state nil response-end)
                              response-end))
              (let ((next (or (next-single-property-change
                               pos 'agent-shell-ui-state nil response-end)
                              response-end)))
                (push (buffer-substring-no-properties pos next) parts)
                (setq pos next))))
          (string-trim (apply #'concat (nreverse parts))))))))

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
PARAMS may include :buffer to target a specific buffer by name."
  (let* ((shell-buf (ob-agent-shell--resolve-buffer (cdr (assq :buffer params))))
         (result nil)
         (err nil)
         (done nil)
         (tokens nil)
         (timeout-timer nil))
    (unwind-protect
        (progn
          (setq timeout-timer
                (run-at-time ob-agent-shell-timeout nil
                             (lambda ()
                               (setq err (format "ob-agent-shell: timed out after %ds"
                                                 ob-agent-shell-timeout)
                                     done t))))
          (push (agent-shell-subscribe-to
                 :shell-buffer shell-buf
                 :event 'turn-complete
                 :on-event (lambda (_data)
                             (if-let ((response (ob-agent-shell--extract-response shell-buf)))
                                 (setq result response done t)
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
          (agent-shell-insert :text body :submit t :no-focus t :shell-buffer shell-buf)
          (while (not done)
            (unless (sit-for 0.1)
              (setq err "ob-agent-shell: aborted by user input" done t))))
      (when timeout-timer (cancel-timer timeout-timer))
      (ob-agent-shell--unsubscribe-all shell-buf tokens))
    (when err (user-error err))
    result))

;;; Org-babel boilerplate

(defvar org-babel-default-header-args:agent-shell
  '((:results . "output") (:exports . "both"))
  "Default header arguments for agent-shell source blocks.")

(defun org-babel-prep-session:agent-shell (_session _params)
  "Sessions are not supported for agent-shell blocks."
  (user-error "ob-agent-shell does not support sessions"))

(provide 'ob-agent-shell)
;;; ob-agent-shell.el ends here
