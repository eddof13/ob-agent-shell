;;; ob-agent-shell.el --- Org-babel backend for agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eddie Jesinsky

;; Author: Eddie Jesinsky <eddie@jesinsky.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1") (org "9.6"))
;; Keywords: ai, org, babel
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/eddof13/ob-agent-shell

;;; Commentary:

;; Org-babel backend that sends source blocks to an existing agent-shell
;; buffer and captures the response as the block result.  Reuses your
;; configured agent-shell client rather than requiring a separate AI setup.
;;
;; This version requires agent-shell-query and agent-shell-shell-buffer
;; from https://github.com/xenodium/agent-shell/pull/NNN.
;; For a version with no upstream dependency, see the main branch.
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

(defgroup ob-agent-shell nil
  "Org-babel integration for agent-shell."
  :group 'org-babel
  :prefix "ob-agent-shell-")

(defcustom ob-agent-shell-timeout 120
  "Seconds to wait for agent-shell to respond before signaling an error."
  :type 'integer
  :group 'ob-agent-shell)

;;; Buffer resolution

(defun ob-agent-shell--resolve-buffer (&optional name)
  "Return the agent-shell buffer to use, or signal an error.
If NAME is non-nil, find a live buffer by that name.
Otherwise return the most recently used agent-shell buffer."
  (let ((buf (if name
                 (get-buffer name)
               (agent-shell-shell-buffer :no-error t))))
    (cond
     ((null buf)
      (user-error "No agent-shell buffer%s found; start one with M-x agent-shell"
                  (if name (format " named %S" name) "")))
     ((not (buffer-live-p buf))
      (user-error "agent-shell buffer %S is no longer live" (buffer-name buf)))
     (t buf))))

;;; Core execution

(defun org-babel-execute:agent-shell (body params)
  "Execute BODY by sending it to the active agent-shell buffer.
PARAMS may include :buffer to target a specific buffer by name."
  (let* ((shell-buf (ob-agent-shell--resolve-buffer (cdr (assq :buffer params))))
         (result nil)
         (err nil)
         (done nil))
    (agent-shell-query
     :text body
     :shell-buffer shell-buf
     :timeout ob-agent-shell-timeout
     :on-complete (lambda (response) (setq result response done t))
     :on-error    (lambda (msg)      (setq err msg done t)))
    (while (not done)
      (unless (sit-for 0.1)
        (setq err "ob-agent-shell: aborted by user input" done t)))
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
