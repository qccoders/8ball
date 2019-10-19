package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	model "./model"
	"github.com/julienschmidt/httprouter"
	"gopkg.in/yaml.v2"
)

const defaultAgentResponseTTL = 1000
const configFile = "config/agents.yml"

var agents []model.Agent

func main() {
	log.Println("Distributed Magic-8 Ball starting...")

	agents = loadAgentsFromYaml(configFile)
	logRegisteredAgents()

	router := httprouter.New()

	router.GlobalOPTIONS = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Header.Get("Access-Control-Request-Method") != "" {
			header := w.Header()
			header.Set("Access-Control-Allow-Methods", r.Header.Get("Allow"))
			header.Set("Access-Control-Allow-Origin", "*")
		}

		w.WriteHeader(http.StatusNoContent)
	})

	router.GET("/answer", answer)

	static := httprouter.New()
	static.ServeFiles("/*filepath", http.Dir("public"))
	router.NotFound = static

	log.Println("Listening on port 8080")
	log.Fatal(http.ListenAndServe(":8080", router))
}

func answer(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	// todo: rate limiting
	// todo: ip ban check

	question := r.URL.Query().Get("q")

	ttl, err := strconv.Atoi(r.URL.Query().Get("ttl"))
	if err != nil {
		ttl = defaultAgentResponseTTL
	}

	hubResponse := model.HubResponse{
		Question: question,
		TTL:      ttl,
	}

	var wg sync.WaitGroup

	request := model.Request{
		Host:      r.Host,
		Question:  question,
		TTL:       ttl,
		WaitGroup: &wg,
		Responses: &[]model.Response{},
	}

	start := time.Now()

	// todo: iterate over registered agents
	for _, agent := range agents {
		wg.Add(1)
		go getAgentAnswer(agent, request)
	}

	wg.Wait()

	hubResponse.Delay = int(time.Since(start) / time.Millisecond)
	hubResponse.Responses = *request.Responses

	res, _ := json.Marshal(hubResponse)

	fmt.Fprintf(w, "%s", res)
	log.Printf("Answered question '%s' for client %s", question, r.Host)
}

func getAgentAnswer(agent model.Agent, request model.Request) {
	defer request.WaitGroup.Done()

	httpClient := http.Client{Timeout: time.Duration(request.TTL) * time.Millisecond}

	requestURL := fmt.Sprintf("%s/answer?q=%s&ttl=%d", agent.Webhook, url.QueryEscape(request.Question), request.TTL)
	httpRequest, err := http.NewRequest("GET", requestURL, nil)
	if err != nil {
		return
	}

	httpRequest.Header.Add("User-Agent", "Distributed Magic 8-Ball (https://github.com/qccoders/8ball)")
	httpRequest.Header.Add("X-Forwarded-For", request.Host)

	var response = model.Response{}
	start := time.Now()

	var reportResult = func(res model.Response, err *model.Error) {
		if err != nil {
			response.Response = -1
			response.Error = err
		}

		// the response contains a name, but it may differ from what's in our local config.
		// swap it out, so we can keep track.
		response.Name = agent.Name
		response.Delay = int(time.Since(start) / time.Millisecond)
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

func logRegisteredAgents() {
	log.Println()
	log.Printf("%-15s\t%-15s\t%s", "Agent", "Language", "Webhook")
	log.Printf("%-15s\t%-15s\t%s", "-----", "--------", "-------")

	for _, agent := range agents {
		log.Printf("%-15s\t%-15s\t%s", agent.Name, agent.Language, agent.Webhook)
	}

	log.Println()
}

func loadAgentsFromYaml(file string) (agents []model.Agent) {
	file = filepath.FromSlash(file) // force cross-platform separator char conversion

	log.Printf("Loading agents from %s...", file)

	yamlFile, err := ioutil.ReadFile(file)
	if err != nil {
		log.Fatalf("Failed to read file %s: %s", file, err)
	}

	agents = []model.Agent{}

	err = yaml.Unmarshal(yamlFile, &agents)
	if err != nil {
		log.Fatalf("Failed to unmarshal yaml contents: %s", err)
	}

	return agents
}
