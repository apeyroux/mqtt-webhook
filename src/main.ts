import express = require('express')
import winston = require('winston')
import expressWinston = require('express-winston')
import fetch = require('node-fetch')
import req = require('request')
import yargs = require('yargs')
import bcrypt = require('bcrypt')
import fs = require('fs');
import qs = require('querystring')

const app: express.Application = express()

const enum AuthType {
    LOGIN = "login",
    AUTH = "auth",
    IDENT = "ident"
}

type Configuration = {
    users: User[],
    urlneotoken: string,
    wipman: string
}

type User = {
    login: String,
    password: String
}

type Authentification = {
    type: AuthType,
    login: String,
    idtel?: String,
}

type AuthOnRegister = {
    peer_addr: String,
    peer_port: Number,
    username: String,
    password: String,
    mountpoint: String,
    client_id: String,
    clean_session: Boolean
}

function load_users(htpasswd: string): User[] {
    try {
        return fs.readFileSync(htpasswd, "utf-8").split('\n').filter(l => l.split(':').length == 2).map(l => {
            const [login, password] = l.split(':')
            return { login: login, password: password }
        })
    } catch (err) {
        console.log(err.message)
        return []
    }
}

async function is_auth(auth: AuthOnRegister, cfg: Configuration): Promise<Authentification> {
    const tauth = auth.username.split(':')
    const err: string = "Impossible de vous identifier"
    switch (tauth.length) {
        case 0:
            return Promise.reject(err)
        case 1:
            const [login] = tauth
            const fusers = cfg.users.filter(u => u.login == login)
            switch (fusers.length) {
                case 1:
                    return bcrypt.compare(auth.password, fusers[0].password.replace(/^\$2y/, "$2a")).then(res => {
                        if (res) {
                            return Promise.resolve({ type: AuthType.LOGIN, login: login })
                        } else {
                            return Promise.reject(err)
                        }
                    })
                default:
                    return Promise.reject(err)
            }
        case 3:
            const [type, imei, idtel] = tauth
            switch (type) {
                case AuthType.AUTH:
                    let wsurl = new URL(cfg.urlneotoken)
                    wsurl.searchParams.append('uid', 'ok')
                    wsurl.searchParams.append('token', 'ok')
                    fetch.default(wsurl.toString(), { method: "get" }).then(r => {
                        if (r.status != 200) {
                            return Promise.reject(err)
                        } else {
                            return Promise.resolve({ tag: "right", value: { type: AuthType.AUTH, login: imei, idtel: idtel } })
                        }
                    })
                case AuthType.IDENT:
                    return Promise.resolve({ type: AuthType.IDENT, login: imei, idtel: idtel })
                default:
                    return Promise.reject(err)
            }
        default:
            return Promise.reject(err)
    }
}

// start main()
const argv = yargs.scriptName("mqtt-webhook").options({
    p: { type: 'number', alias: 'port', default: 3000 },
    u: { type: 'string', alias: 'htpasswd', default: "htpasswd" },
    c: { type: 'string', alias: 'config' }
}).help().argv

const users: User[] = load_users(argv.u)
const cfg: Configuration = {
    users: users,
    urlneotoken: "http://neotoken.gendarmerie.fr/token/check/",
    wipman: "88mph"
}

app.use(express.json())

app.post('/auth', function(req, res) {
    const ok = { "result": "ok" }
    const ko = {
        "result": {
            "error": "not_allowed"
        }
    }
    if (req.headers['vernemq-hook']) {
        if (req.headers['vernemq-hook'] == "auth_on_register") {
            if (req.body.username && req.body.password) {
                const payload = is_auth(req.body, cfg).then(p => {
                    res.send(ok)
                }).catch(err => {
                    console.log(err)
                    res.status(401).send(ko)
                })
            }
        }
    }
})

app.listen(argv.p, function() {
    console.log(`mqtt-webhook app listening on port ${argv.p}!`)
})
