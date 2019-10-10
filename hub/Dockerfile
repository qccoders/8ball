FROM ubuntu

RUN apt-get update -y && \
    apt-get install ca-certificates -y

WORKDIR /app

COPY public /app/public
COPY config /app/config
COPY hub /app/hub

ENTRYPOINT ["./hub"]