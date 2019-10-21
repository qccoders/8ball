import React, { Component } from "react";
import axios from 'axios';

import { hubURL } from '../constants';
import strategies from '../strategies';
import "./App.css";
import Icosahedron from "../icosahedron/Icosahedron";
import QuestionInput from "../questionInput/QuestionInput";
import QuestionDisplay from "../questionDisplay/QuestionDisplay";
import Details from "../details/Details";

class App extends Component {
    state = { 
        refreshing: false,
        initialized: false,
        response: 0, 
        question: '', 
        askedQuestion: '', 
        responses: [],
        shakeInput: false,
        showDetails: false,
        strategy: '',
        delay: 0,
    };

    handleClick = (e) => {
        if (this.state.question === '') {
            this.rejectClick();
        } else {
            this.setState({ 
                refreshing: true,
                initialized: true,
                question: '', 
                askedQuestion: this.state.question,
                responses: [],
                strategy: '',
                showDetails: false,
            }, () => {
                axios.get(`${hubURL}?q=${this.state.askedQuestion}`)
                .then(response => {
                    let responses = response.data.responses;
                    let { strategy, result } = this.calculateResponse(responses);

                    this.setState({ 
                        responses: responses,
                        response: result,
                        strategy: strategy,
                        delay: response.data.delay,
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
        return { strategy: strategy.name, result };
    }

    render() {
        var { refreshing, initialized, response, responses, question, askedQuestion, shakeInput, showDetails, strategy, delay } = this.state;

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
                    <Icosahedron 
                        initialized={initialized} 
                        refreshing={refreshing} 
                        response={response}
                        onClick={() => this.setState({ showDetails: !showDetails })}
                    />
                    <Details 
                        show={showDetails}
                        strategy={strategy}
                        delay={delay}
                        response={response}
                        responses={responses}
                    />
                </div>
            </div>
        );
    }        
}

export default App;
