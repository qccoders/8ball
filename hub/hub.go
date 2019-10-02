package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"

	"github.com/julienschmidt/httprouter"
)

func question(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	queryValues := r.URL.Query()
	fmt.Fprintf(w, "question: %s, TTL: %s\n", queryValues.Get("q"), queryValues.Get("ttl"))
}

func register(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	body, err := ioutil.ReadAll(io.LimitReader(r.Body, 1048576))
	if err != nil {
		panic(err)
	}
	if err := r.Body.Close(); err != nil {
		panic(err)
	}

	fmt.Fprintf(w, "register; auth: %s, body: %s\n", r.Header.Get("Authorization"), body)
}

func main() {
	router := httprouter.New()
	router.GET("/question", question)
	router.POST("/register", register)

	// if not found look for a static file
	static := httprouter.New()
	static.ServeFiles("/*filepath", http.Dir("public"))
	router.NotFound = static

	log.Fatal(http.ListenAndServe(":8080", router))
}
