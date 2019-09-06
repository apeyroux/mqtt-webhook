docker run -it --rm -e "DOCKER_VERNEMQ_PLUGINS__VMQ_WEBHOOKS=on" -e "DOCKER_VERNEMQ_VMQ_WEBHOOKS__WHLABO__HOOK=auth_on_register" -e "DOCKER_VERNEMQ_VMQ_WEBHOOKS__WHLABO__ENDPOINT=http://wh.px.io/auth" -p 1883:1883 erlio/docker-vernemq

go build -ldflags "-s -w"
