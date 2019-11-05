package main

import (
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"net/http"
	"time"

	"github.com/julienschmidt/httprouter"
	"github.com/rs/cors"
)

func main() {
	router := httprouter.New()
	router.GET("/answer", answer)

	handler := cors.Default().Handler(router)

	log.Println("Listening on port 5004")
	log.Fatal(http.ListenAndServe(":5004", handler))

	rand.Seed(time.Now().UnixNano())
}

func answer(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	answer := response{
		Name:     "Go",
		Response: rand.Intn(20),
		Children: []response{
			{
				Name:     "lang",
				Response: rand.Intn(20),
			},
		},
	}

	res, _ := json.Marshal(answer)

	fmt.Fprintf(w, "%s", res)
}

type response struct {
	Name     string     `json:"name,required"`
	Response int        `json:"response,required"`
	Children []response `json:"children"`
}
