FROM alpine:3.5

RUN apk --update add emacs-nox make

WORKDIR /tmp/esc

COPY esc.el esc-test.el makefile test.sh ./

RUN make
