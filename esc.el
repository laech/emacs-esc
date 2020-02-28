;;; esc.el --- Normal Escape -*- lexical-binding: t; -*-

;; Homepage: https://gitlab.com/lae/emacs-esc
;; Package-Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools

;;; Commentary:

;; Provides a global minor mode `esc-mode', when the escape key is
;; pressed and released on its own, perform a quit action (like C-g),
;; instead of acting like a prefix key.

;;; Code:

(defcustom esc-quit-function 'esc--quit
  "Name of function to call when escape is pressed on its own.
Default is to delegate to whatever action that is under C-g at
the time of invocation."
  :group 'esc
  :type 'text)

(defvar esc--test-command-override nil)

(defun esc--lookup-key-in (keymaps key)
  (if keymaps
      (let ((def (lookup-key (car keymaps) key)))
        (if (or (not def) (numberp def))
            (esc--lookup-key-in (cdr keymaps) key)
          def))
    nil))

(defun esc--lookup-key (key)
  (cond
   (esc--test-command-override)
   ((esc--lookup-key-in (current-active-maps t (point)) key))))

(defun esc--quit ()
  (apply
   (cond
    ((esc--lookup-key  [?\C-g]))
    (t 'keyboard-quit))
   nil))

(defvar esc--decode-map nil)

(defun esc--read-events (events)
  (let ((event (read-event nil nil 0.01)))
    (if event
        (esc--read-events (cons event events))
      (reverse events))))

(defun esc--process (prompt keymap previous-events)
  (let ((event (read-event nil nil 0.01)))
    (if (and event keymap)
        (let* ((def (lookup-key keymap (vector event)))
               (events (cons event previous-events)))
          (cond
           ((vectorp def) def)
           ((keymapp def) (esc--process prompt def events))
           ((functionp def) (apply def (list prompt)))
           ((vconcat [27] (esc--read-events events)))))
      (if previous-events
          (vconcat [27] (reverse previous-events))
        (apply esc-quit-function '())))))

(defun esc--decode (prompt)
  (esc--process prompt esc--decode-map nil))

;;;###autoload
(define-minor-mode esc-mode
  "Toggle esc mode."
  :global t
  (cond
   (esc-mode
    (setq esc--decode-map (lookup-key input-decode-map [?\e]))
    (define-key input-decode-map [?\e] 'esc--decode)
    (define-key input-decode-map [escape] 'esc--decode))
   (t
    (define-key input-decode-map [?\e] esc--decode-map)
    (define-key input-decode-map [escape] nil)
    (setq esc--decode-map nil))))

(provide 'esc)

;;; esc.el ends here
