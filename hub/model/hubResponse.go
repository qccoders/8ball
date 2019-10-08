package model

// HubResponse is the hub response to a question.
type HubResponse struct {
	Question  string     `json:"question,required"`
	TTL       int        `json:"ttl,required"`
	Delay     int        `json:"delay,omitempty"`
	Responses []Response `json:"responses,omitempty"`
}
