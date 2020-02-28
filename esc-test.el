;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

(defmacro test (&rest body)
  (let ((keymap (make-symbol "keymap")))
    `(let ((,keymap (copy-tree (lookup-key input-decode-map [?\e]))))
       (unwind-protect
           (progn
             ,@body)
         (esc-mode -1)
         (define-key input-decode-map [?\e] ,keymap)
         (setq
          unread-command-events nil
          esc--test-command-override nil)))))

(ert-deftest should-be-able-to-decode-single-event ()
  (test
   (define-key input-decode-map [?\e ?1] [up])
   (esc-mode)
   (setq unread-command-events (list ?1))
   (should (equal [up] (esc--decode nil)))))

(ert-deftest should-be-able-to-decode-multiple-events ()
  (test
   (define-key input-decode-map "\e[1;5C" [C-right])
   (esc-mode)
   (setq unread-command-events (listify-key-sequence "[1;5C"))
   (should (equal [C-right] (esc--decode nil)))))

(ert-deftest should-be-able-to-partially-match-multiple-events ()
  (test
   (define-key input-decode-map "\e[1" (lambda (prompt) [up]))
   (esc-mode)
   (setq unread-command-events (listify-key-sequence "[1;5C"))
   (should (equal [up] (esc--decode nil)))
   (should (equal (listify-key-sequence ";5C") unread-command-events))))

(ert-deftest should-be-able-to-decode-using-function ()
  (test
   (define-key input-decode-map [?\e ?3] (lambda (prompt) [left]))
   (esc-mode)
   (setq unread-command-events (list ?3))
   (should (equal [left] (esc--decode nil)))))

(ert-deftest should-pass-through-single-event-when-not-in-map ()
  (test
   (define-key input-decode-map [?\e ?i] nil)
   (should-not (lookup-key input-decode-map [?\e ?i]))
   (esc-mode)
   (setq unread-command-events (list ?i))
   (should (equal [?\e ?i] (esc--decode nil)))))

(ert-deftest should-pass-through-multiple-events-when-not-in-map ()
  (test
   (define-key input-decode-map "\e[1;5C" nil)
   (should
    (let ((def (lookup-key input-decode-map "\e[1;5C")))
      (or (not def)
          (numberp def))))
   (esc-mode)
   (setq unread-command-events (listify-key-sequence "[1;5C"))
   (should
    (equal
     (vconcat (listify-key-sequence "\e[1;5C"))
     (esc--decode nil)))))

(ert-deftest should-execute-quit-command-esc-has-associated-event ()
  (test
   (setq esc--test-command-override (lambda () [right]))
   (esc-mode)
   (should (equal [right] (esc--decode nil)))))

(ert-deftest should-restore-original-keymap ()
  (test
   (let ((test-keymap (let ((map (make-sparse-keymap)))
                        (define-key map [?a] 'forward-char)
                        map)))

     (define-key input-decode-map [?\e] (copy-tree test-keymap))

     (esc-mode)
     (should
      (equal
       'esc--decode
       (lookup-key input-decode-map [?\e])))

     (esc-mode -1)
     (should
      (equal
       test-keymap
       (lookup-key input-decode-map [?\e]))))))