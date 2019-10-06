package model

import "sync"

// Request is an agent request for an answer.
type Request struct {
	Host      string `json:"host,required"`
	Question  string `json:"question,required"`
	TTL       int    `json:"ttl,omitempty"`
	WaitGroup *sync.WaitGroup
	Responses *[]Response
}
