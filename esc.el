
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

(defun esc--read-events (events)
  (let ((event (read-event nil nil 0.01)))
    (if event
        (esc--read-events (cons event events))
      (reverse events))))

(defun esc--terminal-decode (prompt)
  (let ((events (esc--read-events nil)))
    (if (not events)
        (esc--keyboard-quit)
      (let ((def (if esc--meta-decode-map
                     (lookup-key esc--meta-decode-map (vconcat events))
                   nil)))
        (cond
         ((functionp def)
          (apply def (list prompt)))
         (def)
         ((= 1 (length events))
          (vector (event-apply-modifier (car events) 'meta 27 "M-")))
         (t
          (push 27 events)
          (vconcat events)))))))

(defvar esc--meta-decode-map nil)

(define-minor-mode esc--terminal-mode
  "Toggle one esc mode in terminal Emacs."
  :global t
  (cond
   (esc--terminal-mode
    (setq esc--meta-decode-map (lookup-key input-decode-map [?\e]))
    (define-key input-decode-map [?\e] 'esc--terminal-decode))
   (t
    (define-key input-decode-map [?\e] esc--meta-decode-map)
    (setq esc--meta-decode-map nil))))


(define-minor-mode esc-mode
  "Toggle esc mode."
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
