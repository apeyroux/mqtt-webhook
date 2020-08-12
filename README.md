# mqtt-webhook

## Exemple

``` shell
curl -H "vernemq-hook: auth_on_register" -H "Content-Type: application/json" -v -d '{"username": "token:imei:idetel:uid:ok", "client_id": "XXXXX1"}' -XPOST http://127.0.0.1:8080/auth

curl -H "vernemq-hook: auth_on_subscribe" -H "Content-Type: application/json" -d '{"username":"alex", "client_id": "ok", "mountpoint": "ok", "topics":[{"topic": "a/b", "qos": 1}]}' -v -XPOST http://127.0.0.1:8080/auth

curl -H "vernemq-hook: auth_on_publish" -H "Content-Type: application/json" -v -d '{ "username": "username", "client_id": "clientid", "mountpoint": "", "qos": 1, "topic": "a/b", "payload": "hello", "retain": false }' -XPOST http://127.0.0.1:8080/auth
```

## Build with Nix & Proxy

``` bash
NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz stack build --nix --no-nix-pure
```

## web-hook podam

``` shell
ssh ubuntu@10.227.193.18
```

