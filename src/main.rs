extern crate clap;
extern crate serde;
extern crate serde_derive;
use actix_web::{web, App as WebApp, HttpRequest, HttpResponse, HttpServer};
use clap::Arg;
use serde::{Deserialize, Serialize};
#[macro_use]
extern crate log;
extern crate simplelog;
use simplelog::*;
use std::fs::File;
extern crate reqwest;

type IMEI = String;

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
struct WebHookAuthPayload {
    peer_addr: String,
    peer_port: i32,
    username: Option<String>,
    password: Option<String>,
    mountpoint: String,
    client_id: String,
    clean_session: bool,
}

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
struct WebHookAuthSubPayload {
    username: String,
    mountpoint: String,
    client_id: String,
    topics: Vec<Topic>,
}

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
struct WebHookAuthPubPayload {
    username: String,
    mountpoint: String,
    client_id: String,
    qos: i32,
    topic: String,
    payload: String,
    retain: bool,
}

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
struct Topic {
    topic: String,
    qos: i32,
}

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
enum Authentication {
    Token {
        imei: IMEI,
        idtel: String,
        uid: String,
        token: String,
    },
    Ident {
        imei: IMEI,
        idtel: String,
    },
    Login {
        username: String,
        password: String,
    },
}

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag = "result")]
#[serde(rename_all = "lowercase")]
enum WebHookResult {
    Ok,
    Failed,
    Next,
}

fn login_to_auth(username: String, password: String) -> Result<Authentication, String> {
    // unimplemented!();
    let vu: Vec<&str> = username.split(':').collect();
    match *vu {
        [] => Err(String::from("Comprend pas")),
        [login] => Ok(Authentication::Login {
            username: login.to_string(),
            password: password.to_string(),
        }),
        [auth_type, imei, idtel] if auth_type == "ident" => Ok(Authentication::Ident {
            imei: imei.to_string(),
            idtel: idtel.to_string(),
        }),
        [auth_type, imei, idtel] if auth_type == "auth" => {
            let vt: Vec<&str> = password.split(':').collect();
            match *vt {
                [uid, token] => Ok(Authentication::Token {
                    uid: uid.to_string(),
                    token: token.to_string(),
                    imei: imei.to_string(),
                    idtel: idtel.to_string(),
                }),
                _ => Err(String::from(format!(
                    "this imei {} is trying to use a password {} that is not a token.",
                    imei, password
                ))),
            }
        }
        _ => Err(String::from("I don't understand the username chain.")),
    }
}

fn auth_is_ok(auth: &Authentication) -> Result<WebHookResult, WebHookResult> {
    match auth {
        Authentication::Token { uid, token, .. } => {
            // todo: kk faire un param
            let request_url = format!(
                "http://neotoken.gendarmerie.fr/token/check/?uid={uid}&token={token}",
                uid = uid,
                token = token
            );
            let response = reqwest::get(&request_url);
            match response {
                Ok(response) if response.status().as_u16() == 200 => Ok(WebHookResult::Ok),
                Ok(_) => Err(WebHookResult::Failed),
                Err(e) => {
                    error!("to bad {:?}", e);
                    Err(WebHookResult::Failed)
                }
            }
        }
        Authentication::Ident { .. } => Ok(WebHookResult::Ok),
        Authentication::Login { username, password }
            if username == "88mph" && password == "88mph" =>
        {
            Ok(WebHookResult::Ok)
        }
        _ => Err(WebHookResult::Next),
    }
}

fn ws_auth_sub(_client: web::Json<WebHookAuthSubPayload>, _req: HttpRequest) -> HttpResponse {
    HttpResponse::Ok().json(WebHookResult::Ok)
}

fn ws_auth_pub(client: web::Json<WebHookAuthPubPayload>, req: HttpRequest) -> HttpResponse {
    match req.headers().get("vernemq-hook") {
        Some(hv) if hv.to_str().unwrap() == "auth_on_publish" => match client.into_inner() {
            WebHookAuthPubPayload {
                username, topic, ..
            } => {
                if topic.contains("wip") && username != "88mph" {
                    HttpResponse::Ok().json(WebHookResult::Ok)
                } else {
                    HttpResponse::Ok().json(WebHookResult::Ok)
                }
            }
        },
        _ => HttpResponse::Ok().json(WebHookResult::Ok), // mettre un 200 car vernemq connait pas le 401 sur le auth_sub
    }
}

fn ws_auth(client: web::Json<WebHookAuthPayload>, req: HttpRequest) -> HttpResponse {
    match req.headers().get("vernemq-hook") {
        // auth_on_register
        Some(hv) if hv.to_str().unwrap() == "auth_on_register" => match client.into_inner() {
            WebHookAuthPayload {
                peer_addr: _,
                peer_port: _,
                username: Some(username),
                password: Some(password),
                mountpoint: _,
                client_id: _,
                clean_session: _,
            } => match login_to_auth(username, password) {
                Ok(auth) => {
                    if auth_is_ok(&auth) == Ok(WebHookResult::Ok) {
                        info!("ok {:?}", auth);
                        HttpResponse::Ok().json(WebHookResult::Ok)
                    } else {
                        info!("ko {:?}", auth);
                        HttpResponse::Ok().json(WebHookResult::Failed)
                    }
                }
                Err(err) => {
                    info!("ko {}", err);
                    HttpResponse::Ok().json(WebHookResult::Failed)
                }
            },
            _ => HttpResponse::Ok().json(WebHookResult::Failed),
        },
        // todo: mettre le auth_on_publish ici
        _ => HttpResponse::Ok().json(WebHookResult::Failed), // <- send response
    }
}

//
// MAIN
//
fn main() {
    let matches = clap::App::new("mqtt-webhook")
        .about("MQTT-WEBHOOK")
        .version("0.1")
        .arg(
            Arg::with_name("listen")
                .short("l")
                .long("listen")
                .takes_value(true)
                .default_value("127.0.0.1:8080")
                .env("LISTEN_WS"),
        )
        .arg(
            Arg::with_name("logfile")
                .long("log")
                .takes_value(true)
                .default_value("mqtt-webhook.log")
                .env("MQTT_WEBHOO_LOG"),
        )
        .get_matches();

    let listen = matches.value_of("listen").unwrap();
    let logfile = matches.value_of("logfile").unwrap();

    CombinedLogger::init(vec![
        TermLogger::new(LevelFilter::Info, Config::default(), TerminalMode::Mixed).unwrap(),
        WriteLogger::new(
            LevelFilter::Info,
            Config::default(),
            File::create(logfile).unwrap(),
        ),
    ])
    .unwrap();

    let _ =HttpServer::new(|| {
        WebApp::new()
            .data(web::JsonConfig::default().limit(4096))
            .service(web::resource("/auth").route(web::post().to(ws_auth)))
            .service(web::resource("/auth/pub").route(web::post().to(ws_auth_pub)))
            .service(web::resource("/auth/sub").route(web::post().to(ws_auth_sub)))
    })
    .bind(listen)
    .unwrap()
    .run();
}

//
// TEST
//
#[test]
fn test_check_auth_token() {
    let t = login_to_auth("auth:323232:3232k".to_string(), "uid:token".to_string()).unwrap();
    assert_eq!(WebHookResult::Failed, auth_is_ok(&t))
}

#[test]
fn test_check_auth_login_failed() {
    let t = login_to_auth("test".to_string(), "test".to_string()).unwrap();
    assert_eq!(WebHookResult::Failed, auth_is_ok(&t))
}

#[test]
fn test_check_auth_login_ok() {
    let t = login_to_auth("88mph".to_string(), "88mph".to_string()).unwrap();
    assert_eq!(WebHookResult::Ok, auth_is_ok(&t))
}
