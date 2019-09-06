package main

import (
	"encoding/json"
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

func checkAuthType(userName string) int {
	switch userName {
	case "ident":
		return 1
	case "auth":
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
		if checkAuthType(sUserName[0]) != 0 {
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprintf(w, "{'result': 'ok'}")
			log.Printf("😍 id:%s user:%s password:%s", p.ClientID, p.UserName, p.Password)
		} else {
			w.WriteHeader(http.StatusForbidden)
			log.Printf("⛔ id:%s user:%s password:%s", p.ClientID, p.UserName, p.Password)
		}
	default:
		w.WriteHeader(http.StatusMethodNotAllowed)
		fmt.Fprintf(w, "Pas le droit ☹")
	}
}

func main() {
	http.HandleFunc("/auth", webhookHandler)
	http.ListenAndServe(":80", nil)
}
