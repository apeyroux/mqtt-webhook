package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"net/url"
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
		if len(sPassword) != 2 {
			return 0
		}
		req, err := http.PostForm("http://neotoken.gendarmerie.fr/token/check/",
			url.Values{"uid": {sPassword[0]}, "token": {sPassword[1]}})
		if err != nil {
			log.Printf("ERROR: %s", err)
			return 0
		}
		if req.StatusCode != 200 {
			return 0
		}
		return 2
	case "neoparc":
		return 3
	default:
		return 0
	}
}

// Auth du Node qui permet du pub/sub
func check88mph(login string, password string) bool {
	// faire un fichier de conf
	if(login == "88mph" && password == "88mph") {
		return true;
	} else {
		return false;
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
		if len(sUserName) == 0 && false == check88mph(p.UserName, p.Password){
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
	pListen := flag.String("listen", ":8080", "")
	pNeoTokenWS := flag.String("neotoken-url",
		"http://neotoken.gendarmerie.fr/token/check/",
		"URL du point d'entrée du WS NeoToken")
	flag.Parse()
	log.Printf("Démarage du service")
	log.Printf("Ecoute sur %s", *pListen)
	log.Printf("Utilisation de %s comme ws neotoken", *pNeoTokenWS)
	http.HandleFunc("/auth", webhookHandler)
	http.ListenAndServe(*pListen, nil)
}
