package model

// Response is an agent response to a question.
type Response struct {
	Name     string     `json:"name,required"`
	Response int        `json:"response,required"`
	Error    *Error     `json:"error,omitempty"`
	Delay    int        `json:"delay,omitempty"`
	Children []Response `json:"children,omitempty"`
}
