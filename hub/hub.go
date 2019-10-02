package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/julienschmidt/httprouter"
)

func question(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	queryValues := r.URL.Query()
	fmt.Fprintf(w, "question: %s, TTL: %s\n", queryValues.Get("q"), queryValues.Get("ttl"))
}

func main() {
	router := httprouter.New()
	router.GET("/question", question)

	// if not found look for a static file
	static := httprouter.New()
	static.ServeFiles("/*filepath", http.Dir("public"))
	router.NotFound = static

	log.Fatal(http.ListenAndServe(":8080", router))
}
