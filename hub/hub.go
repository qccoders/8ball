package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strconv"
	"strings"
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

	// todo: iterate over registered agents
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

	httpClient := http.Client{Timeout: time.Duration(request.TTL) * time.Millisecond}

	httpRequest, err := http.NewRequest("GET", agentURL, nil)
	if err != nil {
		return
	}

	httpRequest.Header.Add("User-Agent", "Distributed Magic 8-Ball (https://github.com/qccoders/8ball)")
	httpRequest.Header.Add("X-Forwarded-For", request.Host)

	var response = model.Response{}

	var reportResult = func(res model.Response, err *model.Error) {
		if err != nil {
			response.Response = -1
			response.Error = err
		}

		*request.Responses = append(*request.Responses, response)
	}

	httpResponse, err := httpClient.Do(httpRequest)
	if err != nil {
		reportResult(response, makeError(500, nil, err))
		return
	}

	body, err := getResponseBody(httpResponse)

	if httpResponse.StatusCode != http.StatusOK {
		reportResult(response, makeError(httpResponse.StatusCode, body, err))
		return
	}

	err = json.Unmarshal(body, &response)

	if err == nil {
		reportResult(response, nil)
	} else {
		reportResult(response, makeError(500, body, err))
	}
}

func makeError(code int, body []byte, err error) *model.Error {
	var message string

	if err != nil {
		if strings.Contains(err.Error(), "Client.Timeout exceeded") {
			code = 408
			message = "Timed out"
		} else {
			message = err.Error()
		}
	} else {
		message = string(body)
	}

	return &model.Error{
		StatusCode: code,
		Message:    message,
	}
}

func getResponseBody(r *http.Response) (body []byte, err error) {
	if r.Body != nil {
		defer r.Body.Close()
		bodyBytes, err := ioutil.ReadAll(r.Body)
		if err != nil {
			return nil, err
		}

		return bodyBytes, nil
	}

	empty := []byte("N/A")
	return empty, nil
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
