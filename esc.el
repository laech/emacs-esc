
(defvar esc--test-command-override nil)

(defun esc--lookup-key-in (keymaps key)
  (if keymaps
      (let ((def (lookup-key (car keymaps) key)))
        (if (or (not def) (numberp def))
            (esc--lookup-key-in (cdr keymaps) key)
          def))
    nil))

(defun esc--lookup-key-in-active-maps (key)
  (esc--lookup-key-in (current-active-maps t (point)) key))

(defun esc--lookup-key (key)
  (cond
   (esc--test-command-override)
   ((esc--lookup-key-in-active-maps key))))

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


(defvar esc--terminal-last-keymap nil)
(defvar esc--terminal-last-keymap-esc-command nil)

(defun esc--terminal-set-last-keymap (keymap)
  (when esc--terminal-last-keymap
    (define-key esc--terminal-last-keymap
      [?\e] esc--terminal-last-keymap-esc-command))

  (setq esc--terminal-last-keymap nil)
  (setq esc--terminal-last-keymap-esc-command nil)

  (when keymap
    (setq esc--terminal-last-keymap keymap)
    (setq esc--terminal-last-keymap-esc-command (lookup-key keymap [?\e]))
    (define-key keymap [?\e] 'esc--terminal-keyboard-quit)))

(defun esc--terminal-keyboard-quit ()
  (interactive)
  (let ((event (read-event nil nil 0.01)))
    (if (not event)
        (esc--keyboard-quit)

      (let* ((new-event (event-apply-modifier event 'meta 27 "M-"))
             (def (esc--lookup-key-in-active-maps (vector new-event))))
        (push new-event unread-command-events)
        (when (keymapp def)
          (esc--terminal-set-last-keymap def))
        (when overriding-terminal-local-map
          (define-key overriding-terminal-local-map (kbd "ESC") nil))))))

(defun esc--terminal-init-key ()
  (esc--terminal-set-last-keymap nil)
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
    (add-hook 'pre-command-hook 'esc--terminal-init-key)
    (esc--terminal-init-key))
   (t
    (remove-hook 'pre-command-hook 'esc--terminal-init-key)
    (esc--terminal-set-last-keymap nil)
    (when overriding-terminal-local-map
      (define-key overriding-terminal-local-map [?\e] nil)))))


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
