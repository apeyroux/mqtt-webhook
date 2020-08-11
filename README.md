# mqtt-webhook

## Exemple

``` shell
curl -H "vernemq-hook: auth_on_register" -H "Content-Type: application/json" -v -d '{"username": "alexandre.px", "client_id": "XXXXX1"}' -XPOST http://10.227.193.18/ifup

curl -H "vernemq-hook: auth_on_register" -H "Content-Type: application/json" -v -d '{"username": "token:imei:idetel:uid:token", "client_id
": "XXXXX1"}' -XPOST http://127.0.0.1:8080/auth
```

## Build with Nix & Proxy

``` bash
NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz stack build --nix --no-nix-pure
```

## web-hook podam

``` shell
ssh ubuntu@10.227.193.18
```

