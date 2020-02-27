all: compile test

compile:
	emacs -Q --batch -l esc.el

test:
	./test.sh

debug-gui:
	emacs -Q -l esc.el --eval "(esc-mode)"

debug-term:
	emacs -Q -l esc.el --eval "(progn (xterm-mouse-mode) (esc-mode))" -nw

