FROM alpine:latest

RUN apk add chicken openssl-dev

ADD . /app

RUN cd /app && chicken-install

ENV PORT 8080

CMD csi -e '(import (chicken process-context) wiki) (run-server (string->number (get-environment-variable "PORT")))'