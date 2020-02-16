all: compile test

compile:
	emacs -Q --batch -l esc.el

test:
	./test.sh

debug:
	emacs -Q -l esc.el --eval "(esc-mode)"
