package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strconv"
	"sync"
	"time"

	model "./model"
	"github.com/julienschmidt/httprouter"
)

const errorsBeforeDeregistration = 3
const defaultAgentResponseTTL = 1000
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

func authorize(r *http.Request, w http.ResponseWriter) error {
	key := r.Header.Get("Authorization")

	if key != sharedKey {
		http.Error(w, http.StatusText(http.StatusForbidden), http.StatusForbidden)
		log.Printf("Unauthorized de/registration from %s with key '%s'", r.Host, key)
		return errors.New("Invalid auth token")
	}

	return nil
}

func answer(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	// todo: rate limiting
	// todo: ip ban check

	question := r.URL.Query().Get("q")

	ttl, err := strconv.Atoi(r.URL.Query().Get("ttl"))
	if err != nil {
		ttl = defaultAgentResponseTTL
	}

	log.Printf("TTL: %d", ttl)

	agents, _ := json.Marshal(agents)

	var wg sync.WaitGroup

	request := model.Request{
		Host:      r.Host,
		Question:  question,
		TTL:       ttl,
		WaitGroup: &wg,
		Responses: &[]model.Response{},
	}

	for i := 0; i < 3; i++ {
		wg.Add(1)
		go getAgentAnswer("https://4f9gs2dqw3.execute-api.us-east-1.amazonaws.com/prod/answer?q=does%20this%20work", request)
	}

	wg.Wait()

	res, _ := json.Marshal(request.Responses)
	fmt.Fprintf(w, "question: %s, TTL: %d\nAgents: %s\nResponse: %s\n", question, ttl, agents, res)
	log.Printf("Answered question '%s' for client %s", question, r.Host)
}

func getAgentAnswer(agentURL string, request model.Request) {
	defer request.WaitGroup.Done()

	client := http.Client{Timeout: time.Duration(request.TTL) * time.Millisecond}

	req, err := http.NewRequest("GET", agentURL, nil)
	if err != nil {
		return
	}

	req.Header.Add("User-Agent", "Distributed Magic 8-Ball (https://github.com/qccoders/8ball)")

	resp, err := client.Do(req)
	if err != nil {
		log.Printf("Error: %s", err)
		return
	}

	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		bodyBytes, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			return
		}

		var res = model.Response{}
		err = json.Unmarshal(bodyBytes, &res)

		if err == nil {
			*request.Responses = append(*request.Responses, res)
		} else {
			// todo: report the error
		}
	}
}

func register(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	if err := authorize(r, w); err == nil {
		url := ps.ByName("url")

		if url == "" {
			http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
		}

		agents[url] = errorsBeforeDeregistration
		log.Printf("Registered agent at %s\n", url)
		w.WriteHeader(http.StatusNoContent)
	}
}

func deregister(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	if err := authorize(r, w); err == nil {
		url := ps.ByName("url")

		if url == "" {
			http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
		}

		delete(agents, url)
		log.Printf("Deregistered agent at %s\n", url)
		w.WriteHeader(http.StatusNoContent)
	}
}
