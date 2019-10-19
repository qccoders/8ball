import React, { Component } from "react";
import axios from 'axios';

import { Segment, Input, Button, Label, Icon } from 'semantic-ui-react';

import { hubURL } from '../constants';
import strategies from '../strategies';
import "./App.css";
import Icosahedron from "../icosahedron/Icosahedron";
import QuestionInput from "../questionInput/QuestionInput";
import QuestionDisplay from "../questionDisplay/QuestionDisplay";

class App extends Component {
    state = { 
        refreshing: false, 
        response: 0, 
        question: '', 
        askedQuestion: '', 
        responses: [],
        shakeInput: false,
    };

    handleClick = (e) => {
        if (this.state.question === '') {
            this.rejectClick();
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

    rejectClick = () => {
        document.getElementById('question-input').focus();

        this.setState({ shakeInput: true }, () => {
            setTimeout(() => this.setState({ shakeInput: false }), 250)
        })
    }

    handleInput = (e) => {
        this.setState({ question: e.target.value })
    }

    calculateResponse = (responses) => {
        responses = [...responses.filter(r => r.response >= 0)];

        let strategy = strategies[Math.floor(Math.random() * strategies.length)];        
        
        var result = strategy.compute(responses);

        console.log(strategy.name, result, responses);
        return result;
    }

    render() {
        var { refreshing, response, question, askedQuestion, shakeInput } = this.state;

        askedQuestion = (askedQuestion !== '' && !askedQuestion.endsWith('?')) ? askedQuestion + '?' : askedQuestion;
        
        return (
            <div className="app">
                <div className="window">
                    <QuestionInput
                        id="question-input"
                        shake={shakeInput}
                        value={question} 
                        onChange={this.handleInput} 
                        onClick={this.handleClick}
                    />
                    <QuestionDisplay question={askedQuestion}/>
                    <Icosahedron refreshing={refreshing} response={response}/>
                    <Icon name='info circle' size='big' className='icon'/>
                </div>
            </div>
        );
    }        
}

export default App;
