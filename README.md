# Dev

``` shell
docker run -it --rm -e "DOCKER_VERNEMQ_PLUGINS__VMQ_WEBHOOKS=on" -e "DOCKER_VERNEMQ_VMQ_WEBHOOKS__WHLABO__HOOK=auth_on_register" -e "DOCKER_VERNEMQ_VMQ_WEBHOOKS__WHLABO__ENDPOINT=http://wh.px.io/auth" -p 1883:1883 erlio/docker-vernemq
```

``` shell
ngrok http 8080 --hostname=wh.px.io
```

``` shel
mosquitto_pub -u 88mph -P 88mph -t test -m ok -d
```

## Static build

``` shell
docker run -it --rm -v $(pwd):/app -w /app -u $(id -u):$(id -g) rust:musl cargo build --target x86_64-unknown-linux-musl --release
```
