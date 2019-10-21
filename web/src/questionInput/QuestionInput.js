import React, { Component } from "react";
import "./QuestionInput.css";

import { Segment, Input, Button } from 'semantic-ui-react';

class QuestionInput extends Component {
    render() {
        let classNames = "input-container" + (this.props.shake ? " shake" : "");

        return (
            <div id='question-input-container' className={classNames}>
                <Segment inverted>
                    <Input 
                        className='input-box'
                        id={this.props.id}
                        inverted 
                        value={this.props.value}
                        placeholder='Ask a Question...' 
                        onChange={this.props.onChange}
                        action={
                            <Button 
                                color='green'
                                onClick={this.props.onClick}
                            >
                                Get an Answer!
                            </Button>
                        }
                    />
                </Segment>
            </div>
        );
    }
}

export default QuestionInput;
