import React, { Component } from "react";
import "./QuestionDisplay.css";

import { Label } from 'semantic-ui-react';

class QuestionDisplay extends Component {
    render() {
        var { question } = this.props;

        return (
            <div className="question-container">
            {question !== '' && <Label className="question" size='big'>
                    {question}
            </Label>}
        </div>
        );
    }
}

export default QuestionDisplay;
