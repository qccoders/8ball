package model

// Agent is a registered agent.
type Agent struct {
	Name     string `json:"name,required"`
	Webhook  string `json:"webhook,required"`
	Language string `json:"language,omitempty"`
}
