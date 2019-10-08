import express = require('express')
import winston = require('winston')
import expressWinston = require('express-winston')
import fetch = require('node-fetch')
import yargs = require('yargs')
import bcrypt = require('bcrypt')
import fs = require('fs');
import { resolve } from 'url';

const app: express.Application = express()

const enum AuthType {
    LOGIN = "login",
    AUTH = "auth",
    IDENT = "ident"
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

type Left<T> = { tag: "left", value: T }
type Right<T> = { tag: "right", value: T }
type Either<L, R> = Left<L> | Right<R>


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

async function is_auth(auth: AuthOnRegister, users: User[]): Promise<Authentification> {
    const tauth = auth.username.split(':')
    const err: string = "Impossible de vous identifier"
    switch (tauth.length) {
        case 0:
            return Promise.reject(err)
        case 1:
            const [login] = tauth
            const fusers = users.filter(u => u.login == login)
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
                    fetch.default("https://google.fr", { method: "post" }).then(r => {
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

app.use(express.json())

app.post('/auth', function (req, res) {
    const ok = { "result": "ok" }
    const ko = {
        "result": {
            "error": "not_allowed"
        }
    }
    if (req.headers['vernemq-hook']) {
        if (req.headers['vernemq-hook'] == "auth_on_register") {
            if (req.body.username && req.body.password) {
                const payload = is_auth(req.body, users).then(p => {
                    res.send(ok)
                }).catch(err => {
                    console.log(err)
                    res.status(401).send(ko)
                })
            }
        }
    }
})



bcrypt.compare("andre", "$2y$05$LLCSCd59BG.lRMSOmh3qgeH6ElWTivQNOSKBeJkZ7Z3ygWKFatoAK".replace(/^\$2y/, "$2a")).then(res => {
    console.log(res)
});

app.listen(argv.p, function () {
    console.log(`mqtt-webhook app listening on port ${argv.p}!`)
})
