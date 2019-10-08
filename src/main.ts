import express = require("express")
import winston = require('winston')
import expressWinston = require('express-winston')
import fetch = require('node-fetch')
import yargs = require('yargs')
import bcrypt = require('bcrypt')

const app: express.Application = express()

const enum AuthType {
	LOGIN = "login",
	AUTH = "auth",
	IDENT = "ident"
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

function is_auth(auth: AuthOnRegister): Either<String, Authentification> {
	const tauth = auth.username.split(':')
	const err: Left<String> = { tag: "left", value: "Impossible de vous identifier" }
	switch (tauth.length) {
		case 0:
			return err
		case 1:
			const [login] = tauth
			return { tag: "right", value: { type: AuthType.LOGIN, login: login } }
		case 3:
			const [type, imei, idtel] = tauth
			switch (type) {
				case AuthType.AUTH:
					fetch.default("https://google.fr", { method: "post" }).then(r => {
						if (r.status != 200) {
							return err
						} else {
							return { tag: "right", value: { type: AuthType.AUTH, login: imei, idtel: idtel } }
						}
					})
				case AuthType.IDENT:
					return { tag: "right", value: { type: AuthType.IDENT, login: imei, idtel: idtel } }
				default:
					return err
			}
		default:
			return { tag: "left", value: "Erreur de login" }
	}
}

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
				const payload = is_auth(req.body)
				switch (payload.tag) {
					case "right":
						res.send(ok)
						break
					case "left":
						res.status(401).send(ko)
						break
					default:
						res.status(401).send(ko)
				}
			}
		}
	}
})

// start main()
const argv = yargs.scriptName("mqtt-webhook").options({
	p: { type: 'number', alias: 'port', default: 3000 },
	c: { type: 'string', alias: 'config' }
}).help().argv

bcrypt.compare("88mph", "$2y$05$k0vF/IyN8sPZf4AOFlJRieOO49rfitdtequtuTqjXN9ipdUL4b5Qy", function(err, res) {
    console.log(res)
});

bcrypt.compare("88mpa", "$2y$05$k0vF/IyN8sPZf4AOFlJRieOO49rfitdtequtuTqjXN9ipdUL4b5Qy", function(err, res) {
    console.log(res)
});

app.listen(argv.p, function () {
	console.log(`mqtt-webhook app listening on port ${argv.p}!`)
})