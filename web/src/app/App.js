import React, { Component } from "react";
import axios from 'axios';

import { Segment, Input, Button, Label, Icon } from 'semantic-ui-react';

import { hubURL } from '../constants';
import strategies from '../strategies';
import "./App.css";
import Icosahedron from "../icosahedron/Icosahedron";

class App extends Component {
    state = { refreshing: false, response: 0, question: '', askedQuestion: '', responses: [] }

    handleClick = (e) => {
        if (this.state.question === '') {
            this.rejectClick('question-input', 'question-input-container');
        } else {
            this.setState({ 
                refreshing: true, 
                question: '', 
                askedQuestion: this.state.question,
                responses: []
            }, () => {
                axios.get(`${hubURL}?q=${this.state.askedQuestion}`)
                .then(response => {
                    let responses = response.data.responses;

                    this.setState({ 
                        responses: responses,
                        response: this.calculateResponse(responses),
                        refreshing: false
                    });
                });
            })
        }
    }

    rejectClick = (inputId, containerId) => {
        document.getElementById(inputId).focus();
        document.getElementById(containerId).classList.add('shake')
        setTimeout(() => document.getElementById(containerId).classList.remove('shake'), 250)
    }

    handleInput = (e) => {
        this.setState({ question: e.target.value })
    }

    calculateResponse = (responses) => {
        let strategy = strategies[Math.floor(Math.random() * strategies.length)];
        
        var result = strategy.compute([...responses]);
        
        console.log(strategy.name, result, responses);
        return result;
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
                                        onClick={this.handleClick}
                                    >
                                        Get an Answer!
                                    </Button>
                                }
                            />
                        </Segment>
                    </div>
                    <div className="question-container">
                        {askedQuestion !== '' && <Label className="question" size='big'>
                                {askedQuestion}
                        </Label>}
                    </div>
                    <Icosahedron refreshing={refreshing} response={response}/>
                    <Icon name='info circle' size='big' className='icon'/>
                </div>
            </div>
        );
    }        
}

export default App;
