package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	model "./model"
	"github.com/julienschmidt/httprouter"
)

const errorsBeforeDeregistration = 3
const sharedKey = "Bearer b0486c92-f2ef-487b-9cdf-8550f2177fb5"

var agents map[string]int8

func main() {
	agents = map[string]int8{}

	router := httprouter.New()
	router.GET("/answer", answer)
	router.PUT("/webhooks/:url", register)
	router.DELETE("/webhooks/:url", deregister)

	static := httprouter.New()
	static.ServeFiles("/*filepath", http.Dir("public"))
	router.NotFound = static

	log.Println("Listening on port 8080")
	log.Fatal(http.ListenAndServe(":8080", router))
}

func register(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	key := r.Header.Get("Authorization")

	if key != sharedKey {
		http.Error(w, http.StatusText(http.StatusForbidden), http.StatusForbidden)
		return
	}

	url := ps.ByName("url")

	if url == "" {
		http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
	}

	agents[url] = errorsBeforeDeregistration
	w.WriteHeader(http.StatusNoContent)
}

func deregister(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	key := r.Header.Get("Authorization")

	if key != sharedKey {
		http.Error(w, http.StatusText(http.StatusForbidden), http.StatusForbidden)
		return
	}

	url := ps.ByName("url")

	if url == "" {
		http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
	}

	delete(agents, url)
	w.WriteHeader(http.StatusNoContent)
}

func answer(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	queryValues := r.URL.Query()
	agents, _ := json.Marshal(agents)

	response := model.Response{
		Name:     "foo",
		Response: 1,
		Delay:    50,
	}

	responses := []model.Response{response}

	res, _ := json.Marshal(responses)
	fmt.Fprintf(w, "question: %s, TTL: %s\nAgents: %s\nResponse: %s\n", queryValues.Get("q"), queryValues.Get("ttl"), agents, res)
}
