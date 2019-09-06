package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"strings"
)

type webookPayload struct {
	PeerAddr     string `json:"peer_addr"`
	PeerPort     int    `json:"peer_port"`
	UserName     string `json:"username"`
	Password     string `json:"password"`
	Mountpoint   string `json:"mountpoint"`
	ClientID     string `json:"client_id"`
	CleanSession bool   `json:"clean_session"`
}

func checkAuthType(userName string, password string) int {
	switch userName {
	case "ident":
		return 1
	case "auth":
		sPassword := strings.Split(password, ":")
		// check si j'ai bien uid et token
		if len(sPassword) != 2 {
			return 0
		}
		// todo: caca
		req, err := http.Get(fmt.Sprintf("http://neotoken.gendarmerie.fr/token/check/?uid=%s&token=%s", sPassword[0], sPassword[1]))
		if err != nil {
			log.Printf("ERROR: %s", err)
			return 0
		}
		if req.Request.Response.StatusCode != 200 {
			return 0
		}
		return 2
	case "neoparc":
		return 3
	default:
		return 0
	}
}

func webhookHandler(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "POST":
		d := json.NewDecoder(r.Body)
		p := &webookPayload{}
		err := d.Decode(p)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
		sUserName := strings.Split(p.UserName, ":")
		if len(sUserName) == 0 {
			w.WriteHeader(http.StatusMethodNotAllowed)
		}
		if checkAuthType(sUserName[0], p.Password) != 0 {
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprintf(w, "{'result': 'ok'}")
			log.Printf("ok 😍 id:%s user:%s password:%s", p.ClientID, p.UserName, p.Password)
		} else {
			w.WriteHeader(http.StatusForbidden)
			log.Printf("ko ⛔ id:%s user:%s password:%s", p.ClientID, p.UserName, p.Password)
		}
	default:
		w.WriteHeader(http.StatusMethodNotAllowed)
		fmt.Fprintf(w, "Pas le droit ☹")
	}
}

func main() {
	pListen := flag.String("listen", ":8080", "Listen")
	flag.Parse()
	http.HandleFunc("/auth", webhookHandler)
	http.ListenAndServe(*pListen, nil)
}
