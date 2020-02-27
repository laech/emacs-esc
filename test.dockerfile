FROM alpine:3.5

RUN apk --update add bash emacs-nox make

WORKDIR /tmp/esc

COPY . .

RUN make
