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
nix-shell
PKG_CONFIG_ALLOW_CROSS=1 cargo build --target x86_64-unknown-linux-musl --release
```
