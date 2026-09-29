;;; ob-agent-shell-tests.el --- Tests for ob-agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eddie Jesinsky

;; Author: Eddie Jesinsky <eddie@jesinsky.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run from the repository root:
;;
;;   emacs -Q --batch -L . -l ob-agent-shell-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(dolist (dir (and (file-directory-p (expand-file-name "~/.emacs.d/elpa"))
                  (directory-files (expand-file-name "~/.emacs.d/elpa") t)))
  (when (and (file-directory-p dir)
             (string-match-p "/\\(agent-shell$\\|acp-\\|shell-maker-\\)" dir))
    (add-to-list 'load-path dir)))

(require 'ob-agent-shell)

(defvar ob-agent-shell-tests-shell nil)
(defvar ob-agent-shell-tests-calls nil)
(defvar ob-agent-shell-tests-models nil)
(defvar ob-agent-shell-tests-thoughts nil)
(defvar ob-agent-shell-tests-current-model nil)
(defvar ob-agent-shell-tests-current-thought nil)
(defvar ob-agent-shell-tests-defer-model nil)
(defvar ob-agent-shell-tests-fail-model nil)
(defvar ob-agent-shell-tests-on-complete nil)

(defmacro ob-agent-shell-tests-with-agent (&rest body)
  "Evaluate BODY with agent-shell I/O replaced by recorders.
`ob-agent-shell-tests-shell' is the buffer blocks are routed to.
Calls are pushed onto `ob-agent-shell-tests-calls'."
  (declare (indent 0) (debug t))
  `(let ((ob-agent-shell-tests-shell (generate-new-buffer " *ob-agent-shell-test*"))
         (ob-agent-shell-tests-calls nil)
         (ob-agent-shell-tests-on-complete nil)
         (ob-agent-shell-tests-defer-model nil)
         (ob-agent-shell-tests-fail-model nil))
     (unwind-protect
         (cl-letf (((symbol-function 'ob-agent-shell--resolve-buffer)
                    (lambda (&rest _) ob-agent-shell-tests-shell))
                   ((symbol-function 'agent-shell--state)
                    (lambda () '((:session . ((:id . "session"))))))
                   ((symbol-function 'agent-shell--get-available-models)
                    (lambda (_) ob-agent-shell-tests-models))
                   ((symbol-function 'agent-shell--get-available-thought-levels)
                    (lambda (_) ob-agent-shell-tests-thoughts))
                   ((symbol-function 'agent-shell--current-model-id)
                    (lambda (_) ob-agent-shell-tests-current-model))
                   ((symbol-function 'agent-shell--current-thought-level-id)
                    (lambda (_) ob-agent-shell-tests-current-thought))
                   ((symbol-function 'agent-shell--config-option-set-model-id)
                    (lambda (&rest args)
                      (let ((model-id (plist-get args :model-id))
                            (on-success (plist-get args :on-success))
                            (on-failure (plist-get args :on-failure)))
                        (unless (eq (current-buffer) ob-agent-shell-tests-shell)
                          (error "Model setter ran outside the shell buffer"))
                        (push (list 'model model-id) ob-agent-shell-tests-calls)
                        (cond
                         (ob-agent-shell-tests-fail-model
                          (funcall on-failure '((message . "refused")) nil))
                         (ob-agent-shell-tests-defer-model
                          (setq ob-agent-shell-tests-defer-model on-success))
                         (t (funcall on-success))))))
                   ((symbol-function 'agent-shell--config-option-set-thought-level-id)
                    (lambda (&rest args)
                      (let ((thought-level-id (plist-get args :thought-level-id))
                            (on-success (plist-get args :on-success)))
                        (unless (eq (current-buffer) ob-agent-shell-tests-shell)
                          (error "Thought setter ran outside the shell buffer"))
                        (push (list 'thought thought-level-id) ob-agent-shell-tests-calls)
                        (funcall on-success))))
                   ((symbol-function 'agent-shell-subscribe-to)
                    (lambda (&rest args)
                      (let ((event (plist-get args :event))
                            (on-event (plist-get args :on-event)))
                        (push (list 'subscribe event) ob-agent-shell-tests-calls)
                        (when (eq event 'turn-complete)
                          (setq ob-agent-shell-tests-on-complete on-event))
                        'token)))
                   ((symbol-function 'agent-shell-insert)
                    (lambda (&rest args)
                      (push (list 'insert
                                  (plist-get args :text)
                                  (plist-get args :submit)
                                  (plist-get args :no-focus)
                                  (plist-get args :shell-buffer))
                            ob-agent-shell-tests-calls)
                      (when ob-agent-shell-tests-on-complete
                        (funcall ob-agent-shell-tests-on-complete nil))))
                   ((symbol-function 'agent-shell-unsubscribe)
                    (lambda (&rest _) nil))
                   ((symbol-function 'ob-agent-shell--extract-response)
                    (lambda (_) "Paris.")))
           ,@body)
       (when (buffer-live-p ob-agent-shell-tests-shell)
         (kill-buffer ob-agent-shell-tests-shell)))))

(defun ob-agent-shell-tests-call (kind)
  "Return recorded calls of KIND, oldest first."
  (seq-filter (lambda (call) (eq (car call) kind))
              (reverse ob-agent-shell-tests-calls)))

(ert-deftest ob-agent-shell-resolve-choice-prefers-exact-id ()
  (let ((items '(((:model-id . "Sonnet") (:name . "Claude"))
                 ((:model-id . "x") (:name . "Sonnet")))))
    (should (equal "Sonnet"
                   (ob-agent-shell--resolve-choice
                    "Sonnet" items
                    (lambda (item) (map-elt item :model-id))
                    (lambda (item) (map-elt item :name))
                    "model")))))

(ert-deftest ob-agent-shell-resolve-choice-matches-name-case-insensitively ()
  (let ((items '(((:model-id . "claude-sonnet-4-5") (:name . "Sonnet")))))
    (should (equal "claude-sonnet-4-5"
                   (ob-agent-shell--resolve-choice
                    "sonnet" items
                    (lambda (item) (map-elt item :model-id))
                    (lambda (item) (map-elt item :name))
                    "model")))))

(ert-deftest ob-agent-shell-resolve-choice-rejects-unknown-and-ambiguous-names ()
  (let ((items '(((:value . "a") (:name . "High"))
                 ((:value . "b") (:name . "High")))))
    (should-error (ob-agent-shell--resolve-choice
                   "High" items
                   (lambda (item) (map-elt item :value))
                   (lambda (item) (map-elt item :name))
                   "thought level")
                  :type 'user-error)
    (should-error (ob-agent-shell--resolve-choice
                   "low" items
                   (lambda (item) (map-elt item :value))
                   (lambda (item) (map-elt item :name))
                   "thought level")
                  :type 'user-error)))

(ert-deftest ob-agent-shell-compose-prompt-prepends-literal-context ()
  (should (equal "Be brief.\n\nWhat is the capital of France?"
                 (ob-agent-shell--compose-prompt
                  "What is the capital of France?"
                  "Be brief.")))
  (should (equal "Just the body"
                 (ob-agent-shell--compose-prompt "Just the body" "  ")))
  (should (equal "Only context"
                 (ob-agent-shell--compose-prompt "  \n" "Only context"))))

(ert-deftest ob-agent-shell-compose-prompt-uses-named-element-body ()
  (with-temp-buffer
    (org-mode)
    (insert "#+name: voice\n#+begin_example\nAnswer in one sentence.\n#+end_example\n")
    (should (equal "Answer in one sentence.\n\nWhat is the capital of France?"
                   (ob-agent-shell--compose-prompt
                    "What is the capital of France?"
                    "voice")))
    (should (equal "not-a-name\n\nBody"
                   (ob-agent-shell--compose-prompt "Body" "not-a-name")))))

(ert-deftest ob-agent-shell-compose-prompt-reads-named-source-without-executing ()
  (with-temp-buffer
    (org-mode)
    (insert "#+name: rules\n#+begin_src text\nUse short sentences.\n#+end_src\n")
    (should (equal "Use short sentences."
                   (ob-agent-shell--compose-prompt "" "rules")))))

(ert-deftest ob-agent-shell-blank-body-removes-result-without-routing ()
  (let ((removed nil)
        (routed nil))
    (cl-letf (((symbol-function 'org-babel-remove-result)
               (lambda (&rest _) (setq removed t)))
              ((symbol-function 'ob-agent-shell--resolve-buffer)
               (lambda (&rest _) (setq routed t) nil)))
      (org-babel-execute:agent-shell " \n\t" nil)
      (should removed)
      (should-not routed))))

(ert-deftest ob-agent-shell-execute-sets-model-then-thought-then-sends-context ()
  (ob-agent-shell-tests-with-agent
    (setq ob-agent-shell-tests-models
          '(((:model-id . "claude-sonnet-4-5") (:name . "Sonnet")))
          ob-agent-shell-tests-thoughts
          '(((:value . "high") (:name . "High")))
          ob-agent-shell-tests-current-model "other"
          ob-agent-shell-tests-current-thought "low")
    (should (equal "Paris."
                   (org-babel-execute:agent-shell
                    "What is the capital of France?"
                    '((:model . "Sonnet")
                      (:thought-level . "high")
                      (:context . "Be brief.")))))
    (should (equal '((model "claude-sonnet-4-5")
                     (thought "high"))
                   (append (ob-agent-shell-tests-call 'model)
                           (ob-agent-shell-tests-call 'thought))))
    (should (equal (list 'insert
                         "Be brief.\n\nWhat is the capital of France?"
                         t t ob-agent-shell-tests-shell)
                   (car (ob-agent-shell-tests-call 'insert))))))

(ert-deftest ob-agent-shell-execute-skips-setters-when-already-current ()
  (ob-agent-shell-tests-with-agent
    (setq ob-agent-shell-tests-models
          '(((:model-id . "claude-sonnet-4-5") (:name . "Sonnet")))
          ob-agent-shell-tests-thoughts
          '(((:value . "high") (:name . "High")))
          ob-agent-shell-tests-current-model "claude-sonnet-4-5"
          ob-agent-shell-tests-current-thought "high")
    (org-babel-execute:agent-shell "Hello" '((:model . "Sonnet")
                                             (:thought-level . "High")))
    (should-not (ob-agent-shell-tests-call 'model))
    (should-not (ob-agent-shell-tests-call 'thought))
    (should (equal "Hello" (nth 1 (car (ob-agent-shell-tests-call 'insert)))))))

(ert-deftest ob-agent-shell-execute-does-not-send-when-model-is-unknown ()
  (ob-agent-shell-tests-with-agent
    (setq ob-agent-shell-tests-models
          '(((:model-id . "claude-sonnet-4-5") (:name . "Sonnet"))))
    (let ((err (should-error
                (org-babel-execute:agent-shell "Hello" '((:model . "Opus")))
                :type 'user-error)))
      (should (string-match-p "unknown model" (cadr err))))
    (should-not (ob-agent-shell-tests-call 'model))
    (should-not (ob-agent-shell-tests-call 'insert))
    (should-not (ob-agent-shell-tests-call 'subscribe))))

(ert-deftest ob-agent-shell-execute-does-not-send-when-thought-level-is-missing ()
  (ob-agent-shell-tests-with-agent
    (setq ob-agent-shell-tests-thoughts nil)
    (let ((err (should-error
                (org-babel-execute:agent-shell "Hello" '((:thought-level . "high")))
                :type 'user-error)))
      (should (string-match-p "does not advertise a thought level" (cadr err))))
    (should-not (ob-agent-shell-tests-call 'insert))))

(ert-deftest ob-agent-shell-execute-does-not-send-when-model-setter-fails ()
  (ob-agent-shell-tests-with-agent
    (setq ob-agent-shell-tests-models
          '(((:model-id . "claude-sonnet-4-5") (:name . "Sonnet")))
          ob-agent-shell-tests-thoughts
          '(((:value . "high") (:name . "High")))
          ob-agent-shell-tests-current-model "other"
          ob-agent-shell-tests-current-thought "low"
          ob-agent-shell-tests-fail-model t)
    (let ((err (should-error
                (org-babel-execute:agent-shell "Hello" '((:model . "Sonnet")
                                                        (:thought-level . "high")))
                :type 'user-error)))
      (should (string-match-p "failed to set model" (cadr err))))
    (should (ob-agent-shell-tests-call 'model))
    (should-not (ob-agent-shell-tests-call 'thought))
    (should-not (ob-agent-shell-tests-call 'insert))))

(ert-deftest ob-agent-shell-execute-waits-for-model-before-thought-and-send ()
  (ob-agent-shell-tests-with-agent
    (setq ob-agent-shell-tests-models
          '(((:model-id . "sonnet") (:name . "Sonnet")))
          ob-agent-shell-tests-thoughts
          '(((:value . "high") (:name . "High")))
          ob-agent-shell-tests-current-model "other"
          ob-agent-shell-tests-current-thought "low"
          ob-agent-shell-tests-defer-model t)
    (let ((release nil))
      (cl-letf (((symbol-function 'agent-shell--config-option-set-model-id)
                 (lambda (&rest args)
                   (push (list 'model (plist-get args :model-id))
                         ob-agent-shell-tests-calls)
                   (setq release (plist-get args :on-success)))))
        (run-with-timer 0.15 nil
                        (lambda ()
                          (should release)
                          (should-not (ob-agent-shell-tests-call 'thought))
                          (should-not (ob-agent-shell-tests-call 'insert))
                          (funcall release)))
        (should (equal "Paris."
                       (org-babel-execute:agent-shell
                        "Hello"
                        '((:model . "sonnet") (:thought-level . "high"))))))
      (should (equal '(model thought insert)
                     (mapcar #'car
                             (seq-filter
                              (lambda (call)
                                (memq (car call) '(model thought insert)))
                              (reverse ob-agent-shell-tests-calls))))))))

(ert-deftest ob-agent-shell-execute-times-out-while-waiting-for-model ()
  (ob-agent-shell-tests-with-agent
    (setq ob-agent-shell-tests-models
          '(((:model-id . "sonnet") (:name . "Sonnet")))
          ob-agent-shell-tests-current-model "other"
          ob-agent-shell-tests-defer-model t)
    (let ((err (should-error
                (org-babel-execute:agent-shell
                 "Hello"
                 '((:model . "sonnet") (:timeout . 0.4)))
                :type 'user-error)))
      (should (string-match-p "timed out" (cadr err))))
    (should-not (ob-agent-shell-tests-call 'insert))
    (should-not (ob-agent-shell-tests-call 'subscribe))))

(provide 'ob-agent-shell-tests)
;;; ob-agent-shell-tests.el ends here
