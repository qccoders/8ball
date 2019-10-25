import React, { Component } from "react";
import "./Details.css";
import responses from '../responses';
import ResponseList from './ResponseList';
import ResponseTable from './ResponseTable';

import { Tab, Divider } from 'semantic-ui-react';
import NetworkGraph from "./NetworkGraph";

class Details extends Component {
    panes = () => [
        {
            menuItem: 'Details', 
            render: () => <Tab.Pane inverted className='details-tab'>
                <ResponseTable response={this.props.response} strategy={this.props.strategy} delay={this.props.delay} responses={responses}/>
                <Divider/>
                {this.props.responses.map((r, i) => <ResponseList key={i} index={i} response={r} responses={responses} />)}
            </Tab.Pane> 
        },
        { 
            menuItem: 'Network Graph', 
            render: () => <Tab.Pane inverted className='details-tab'>
                <div id='network' className='network-container'>
                    <NetworkGraph agentResponses={this.props.responses} responses={responses}/>
                </div>
            </Tab.Pane> 
        },
    ];
      
    render() {
        let { show } = this.props;

        return (
            show && <Tab 
                className='details-container'
                menu={{ color: 'black', inverted: true, attached: true }}
                panes={this.panes()}
            />            
        );
    }
}

export default Details;
