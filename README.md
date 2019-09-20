# Dev

``` shell
docker-compose up
```

``` shell
ngrok http 8080 --hostname=wh.px.io
```

``` shel
mosquitto_pub -u 88mph -P 88mph -t test -m ok -d
```

## Static build


``` shell
docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder:nightly-2019-09-05 cargo build --release
```
