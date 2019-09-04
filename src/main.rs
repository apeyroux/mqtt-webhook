#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;

use rocket::Outcome;
use rocket::http::Status;
use rocket::request::{self, Request, FromRequest};
use rocket::response::content;

fn checkIdent(i: &str) -> bool {
    // i == "0000"
    true
}

fn is_valid_auth(key: &str) -> bool {
    let k: Vec<&str> = key.split(":").collect();
    match &k[..] {
        ["neoparc", _, _] => true,
        ["ident", i, _] => checkIdent(i),
        ["auth", _, _] => true,
        _ => false
    }
}

#[derive(Debug)]
enum HookError {
    BadCount,
    Missing,
    Invalid,
}

#[derive(Debug)]
struct VernemqHook (String);

impl<'a, 'r> FromRequest<'a, 'r> for VernemqHook {
    type Error = HookError;
    fn from_request(request: &'a Request<'r>) -> request::Outcome<Self, Self::Error> {
        let hooks: Vec<_> = request.headers().get("vernemq-hook").collect();
        match hooks.len() {
            0 => Outcome::Failure((Status::BadRequest, HookError::Missing)),
            1 if is_valid_auth(hooks[0]) => Outcome::Success(VernemqHook(hooks[0].to_string())),
            1 => Outcome::Failure((Status::Unauthorized, HookError::Invalid)),
            _ => Outcome::Failure((Status::Unauthorized, HookError::BadCount)),
        }
    }
}

#[get("/")]
fn auth(hook: VernemqHook) -> content::Json<&'static str> {
    content::Json("{'result': 'ok'}")
}

fn main() {
    rocket::ignite().mount("/auth", routes![auth]).launch();
}
