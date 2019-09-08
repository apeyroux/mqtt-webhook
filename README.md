# Dev

``` shell
docker run -it --rm -e "DOCKER_VERNEMQ_PLUGINS__VMQ_WEBHOOKS=on" -e "DOCKER_VERNEMQ_VMQ_WEBHOOKS__WHLABO__HOOK=auth_on_register" -e "DOCKER_VERNEMQ_VMQ_WEBHOOKS__WHLABO__ENDPOINT=http://wh.px.io/auth" -p 1883:1883 erlio/docker-vernemq
```

# Static bin

``` shell
env CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -a -ldflags "-s -w"
```
