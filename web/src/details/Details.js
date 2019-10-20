import React, { Component } from "react";
import "./Details.css";

import { Segment } from 'semantic-ui-react';

class Details extends Component {
    render() {
        let { show } = this.props;

        return (
            <div>
                {show && <div id='details-container'>
                    <Segment inverted>
                        foo
                    </Segment>
                </div>}
            </div>
        );
    }
}

export default Details;
