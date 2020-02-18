;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

(defmacro test (&rest body)
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (generate-new-buffer "*temp*")))
       (unwind-protect
           (progn
             (switch-to-buffer ,buffer)
             (transient-mark-mode)
             ,@body)
         (esc-mode -1)
         (kill-buffer ,buffer)
         (setq
          overriding-terminal-local-map nil
          esc--test-command-override nil
          unread-command-events nil
          this-command nil)))))

(ert-deftest should-still-be-able-to-use-meta-commands ()
  (test
   (esc-mode)
   (push ?f unread-command-events)
   (esc--terminal-keyboard-quit)
   (should (eq 'forward-word (key-binding [?\e ?f])))
   (should
    (equal
     (list (event-apply-modifier ?f 'meta 27 "M-"))
     unread-command-events))))

(ert-deftest should-still-be-able-to-use-describe-key-command ()
  (test
   (esc-mode)
   (setq this-command 'forward-word)
   (esc--terminal-init-key)
   (should (eq 'esc--terminal-keyboard-quit (key-binding [?\e])))

   (setq this-command 'describe-key)
   (esc--terminal-init-key)
   (should-not (eq 'esc--terminal-keyboard-quit (key-binding [?\e])))))

(ert-deftest should-be-able-to-cancel-active-region ()
  (test
   (esc-mode)
   (setq esc--test-command-override 'deactivate-mark)
   (insert "abc")
   (push-mark (point-min) t)
   (push-mark (point-max) t t)
   (should (region-active-p))
   (esc--terminal-keyboard-quit)
   (should-not (region-active-p))))

(ert-deftest should-overriding-terminal-local-map ()
  (test
   (esc-mode)
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

(ert-deftest should-bind-esc-key-on-init ()
  (test
   (esc-mode -1)
   (should-not
    (eq 'esc--terminal-keyboard-quit
        (esc--lookup-key-in-active-maps [?\e])))

   (esc-mode)
   (should
    (eq 'esc--terminal-keyboard-quit
        (esc--lookup-key-in-active-maps [?\e])))))

(ert-deftest should-be-able-to-cancel-partially-types-prefix-keymap ()
  (test
   (let ((original-esc-command (lookup-key search-map [?\e])))
     (should (eq search-map (esc--lookup-key-in-active-maps [?\e ?s])))
     (should original-esc-command)
     (should-not (eq original-esc-command 'esc--terminal-keyboard-quit))

     (esc-mode)
     (push ?s unread-command-events)
     (esc--terminal-keyboard-quit)
     (should (eq 'esc--terminal-keyboard-quit (lookup-key search-map [?\e])))

     (esc-mode -1)
     (should (eq original-esc-command (lookup-key search-map [?\e]))))))

(ert-deftest should-restore-modified-prefix-keymaps ()
  (test

   (setq
    overriding-terminal-local-map
    (let ((map (make-sparse-keymap)))
      (define-key map [?\e] 'forward-char)
      map))

   (let ((original-esc-search-command (lookup-key search-map [?\e]))
         (original-esc-goto-command (lookup-key goto-map [?\e])))

     (should (eq search-map (esc--lookup-key-in-active-maps [?\e ?s])))
     (should (eq goto-map (esc--lookup-key-in-active-maps [?\e ?g])))

     (should original-esc-search-command)
     (should original-esc-goto-command)

     (should-not (eq original-esc-search-command 'esc--terminal-keyboard-quit))
     (should-not (eq original-esc-goto-command 'esc--terminal-keyboard-quit))

     (esc-mode)
     (should
      (eq 'esc--terminal-keyboard-quit
          (lookup-key overriding-terminal-local-map [?\e])))

     (push ?s unread-command-events)
     (esc--terminal-keyboard-quit)
     (should
      (eq 'esc--terminal-keyboard-quit
          (lookup-key search-map [?\e])))

     (push ?g unread-command-events)
     (esc--terminal-keyboard-quit)
     (should
      (eq 'esc--terminal-keyboard-quit
          (lookup-key goto-map [?\e])))

     (esc-mode -1)
     (should (eq original-esc-search-command (lookup-key search-map [?\e])))
     (should (eq original-esc-goto-command (lookup-key goto-map [?\e])))
     (should (eq 'forward-char
                 (lookup-key overriding-terminal-local-map [?\e]))))))
