;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

(defmacro test (&rest body)
  `(let ((buffer (generate-new-buffer "*temp*")))
     (unwind-protect
         (progn
           (switch-to-buffer buffer)
           (transient-mark-mode)
           (esc-mode)
           ,@body)
       (kill-buffer buffer)
       (setq
        esc--test-command-override nil
        unread-command-events nil
        this-command nil))))

(ert-deftest should-still-be-able-to-use-meta-commands ()
  (test
   (push ?f unread-command-events)
   (esc--terminal-keyboard-quit)
   (should (eq 'forward-word (key-binding [?\e ?f])))
   (should
    (equal
     (list (event-apply-modifier ?f 'meta 27 "M-"))
     unread-command-events))))

(ert-deftest should-still-be-able-to-use-describe-key-command ()
  (test
   (setq this-command 'forward-word)
   (esc--terminal-pre-command-handler)
   (should (eq 'esc--terminal-keyboard-quit (key-binding [?\e])))

   (setq this-command 'describe-key)
   (esc--terminal-pre-command-handler)
   (should-not (eq 'esc--terminal-keyboard-quit (key-binding [?\e])))))

(ert-deftest should-be-able-to-cancel-active-region ()
  (test
   (setq esc--test-command-override 'deactivate-mark)
   (insert "abc")
   (push-mark (point-min) t)
   (push-mark (point-max) t t)
   (should (region-active-p))
   (esc--terminal-keyboard-quit)
   (should-not (region-active-p))))

(ert-deftest should-overriding-terminal-local-map ()
  (test
   (should (eq 'ESC-prefix (key-binding [?\e])))
   (setq
    overriding-terminal-local-map
    (let ((map (make-sparse-keymap)))
      (define-key map [?\e] 'forward-char)
      map))
   (should (eq 'forward-char (esc--lookup-key [?\e])))

   (setq esc--test-command-override 'deactivate-mark)
   (insert "abc")
   (push-mark (point-min) t)
   (push-mark (point-max) t t)
   (should (region-active-p))
   (esc--terminal-keyboard-quit)
   (should-not (region-active-p))))

(ert-deftest should-be-able-to-cancel-partially-types-prefix-keymap ()
  (test
   (should (eq search-map (key-binding [?\e ?s])))
   (should (lookup-key search-map [?\e]))

   (push ?s unread-command-events)
   (esc--terminal-keyboard-quit)
   (should (eq 'esc--terminal-keyboard-quit (lookup-key search-map [?\e])))))
