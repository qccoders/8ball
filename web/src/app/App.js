import React, { Component } from "react";

import { Segment, Input, Button, Label } from 'semantic-ui-react';

import "./App.css";
import Icosahedron from "../icosahedron/Icosahedron";

class App extends Component {
    state = { refreshing: false, response: 0, question: '', askedQuestion: '' }

    refresh = (e) => {
        if (this.state.question === '') {
            document.getElementById('question-input').focus();
            document.getElementById('question-input-container').classList.add('shake')
            setTimeout(() => document.getElementById('question-input-container').classList.remove('shake'), 250)
            return;
        }

        this.setState({ refreshing: true, question: '', askedQuestion: this.state.question }, () => {
            this.setState({ response: this.state.response + 1 }, () => {
                this.setState({ refreshing: false })
            })
        })
    }

    handleInput = (e) => {
        this.setState({ question: e.target.value })
    }

    render() {
        var { refreshing, response, question, askedQuestion } = this.state;

        askedQuestion = (askedQuestion !== '' && !askedQuestion.endsWith('?')) ? askedQuestion + '?' : askedQuestion;
        
        return (
            <div className="app">
                <div className="window">
                    <div id='question-input-container' className="input-container">
                        <Segment inverted>
                            <Input 
                                id='question-input'
                                inverted 
                                value={question}
                                placeholder='Ask a Question...' 
                                onChange={this.handleInput}
                                action={
                                    <Button 
                                        color='green'
                                        onClick={this.refresh}
                                    >
                                        Get an Answer!
                                    </Button>
                                }
                            />
                        </Segment>
                    </div>
                    <div className="question-container">
                        {askedQuestion !== '' && <Label className="question" inverted size='big'>
                                {askedQuestion}
                        </Label>}
                    </div>
                    <Icosahedron refreshing={refreshing} response={response}/>
                </div>
            </div>
        );
    }        
}

export default App;
