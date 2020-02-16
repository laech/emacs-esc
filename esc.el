
(defvar esc--test-command-override nil)

(defun esc--lookup-key (key)
  (cond
   (esc--test-command-override)
   ((key-binding key))))

(defun esc--keyboard-quit ()
  (interactive)
  (apply
   (cond
    ((esc--lookup-key (kbd "C-g")))
    (t '(keyboard-quit)))
   nil))

(defvar esc--gui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape] 'esc--keyboard-quit)
    map))

(define-minor-mode esc--gui-mode
  "Toggle one esc mode in GUI Emacs."
  :global t)


(defun esc--terminal-keyboard-quit ()
  (interactive)
  (let ((event (read-event nil nil 0.01)))
    (if (not event)
        (esc--keyboard-quit)

      (let* ((new-event (event-apply-modifier event 'meta 27 "M-"))
             (def (esc--lookup-key (vector new-event))))
        (push new-event unread-command-events)
        (when (keymapp def)
          (define-key def (kbd "ESC") 'esc--terminal-keyboard-quit))
        (when overriding-terminal-local-map
          (define-key overriding-terminal-local-map (kbd "ESC") nil))))))

(defun esc--terminal-pre-command-handler ()
  (if (eq this-command 'describe-key)
      (when overriding-terminal-local-map
        (define-key overriding-terminal-local-map (kbd "ESC") nil))
    (unless overriding-terminal-local-map
      (setq overriding-terminal-local-map (make-sparse-keymap)))
    (define-key overriding-terminal-local-map
      (kbd "ESC") 'esc--terminal-keyboard-quit)))

(define-minor-mode esc--terminal-mode
  "Toggle one esc mode in terminal Emacs."
  :global t
  (cond
   (esc--terminal-mode
    (add-hook 'pre-command-hook 'esc--terminal-pre-command-handler))
   (t
    (remove-hook 'pre-command-hook 'esc--terminal-pre-command-handler))))


(define-minor-mode esc-mode
  "Toggle one esc mode."
  :global t
  (cond
   (esc-mode
    (if (display-graphic-p)
        (esc--gui-mode)
      (esc--terminal-mode)))
   (t
    (if (display-graphic-p)
        (esc--gui-mode -1)
      (esc--terminal-mode -1)))))

(provide 'esc)
