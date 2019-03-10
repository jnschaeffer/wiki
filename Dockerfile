FROM alpine:latest

RUN apk add chicken openssl-dev

ADD . /app

RUN cd /app && chicken-install

EXPOSE 80

CMD csi -e '(import wiki) (run-server 80)'