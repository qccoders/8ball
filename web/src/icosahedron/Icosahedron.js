import React, { Component } from "react";
import "./Icosahedron.css";
import responses from "../responses";
import { Icon } from 'semantic-ui-react';

class Icosahedron extends Component {
    render() {
        let { initialized, refreshing, onClick } = this.props;

        return (
            <div className="icosahedron-container" onClick={(initialized && !refreshing) ? onClick : ''}>
                {!initialized || (!refreshing ? <div className="icosahedron fadeIn">
                    <div className="text">
                        {responses[this.props.response]}
                    </div>
                </div> : <Icon className='loading-icon' loading name='circle notch' size='big' />)}
            </div>
        );
    }
}

export default Icosahedron;
