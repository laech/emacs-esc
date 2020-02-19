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
   (should (equal [up] (esc--terminal-decode nil)))))

(ert-deftest should-be-able-to-decode-multiple-events ()
  (test
   (define-key input-decode-map [?\e ?1 ?2] [down])
   (esc-mode)
   (setq unread-command-events (list ?1 ?2))
   (should (equal [down] (esc--terminal-decode nil)))))

(ert-deftest should-be-able-to-decode-using-function ()
  (test
   (define-key input-decode-map [?\e ?3] (lambda (prompt) [left]))
   (esc-mode)
   (setq unread-command-events (list ?3))
   (should (equal [left] (esc--terminal-decode nil)))))

(ert-deftest should-pass-through-single-event-when-not-in-map ()
  (test
   (define-key input-decode-map [?\e ?i] nil)
   (should-not (lookup-key input-decode-map [?\e ?i]))
   (esc-mode)
   (setq unread-command-events (list ?i))
   (should
    (equal
     (vector (event-apply-modifier ?i 'meta ?\e "M-"))
     (esc--terminal-decode nil)))))

(ert-deftest should-pass-through-multiple-events-when-not-in-map ()
  (test
   (define-key input-decode-map [?\e ?i ?j] nil)
   (should-not (lookup-key input-decode-map [?\e ?i ?j]))
   (esc-mode)
   (setq unread-command-events (list ?i ?j))
   (should
    (equal
     (vector ?\e ?i ?j)
     (esc--terminal-decode nil)))))

(ert-deftest should-execute-quit-command-esc-has-associated-event ()
  (test
   (setq esc--test-command-override (lambda () [right]))
   (esc-mode)
   (should (equal [right] (esc--terminal-decode nil)))))

(ert-deftest should-restore-original-keymap ()
  (test
   (let ((test-keymap (let ((map (make-sparse-keymap)))
                        (define-key map [?a] 'forward-char)
                        map)))

     (define-key input-decode-map [?\e] (copy-tree test-keymap))

     (esc-mode)
     (should
      (equal
       'esc--terminal-decode
       (lookup-key input-decode-map [?\e])))

     (esc-mode -1)
     (should
      (equal
       test-keymap
       (lookup-key input-decode-map [?\e]))))))