import React, { Component } from "react";
import "./Details.css";

import { Segment, Tab } from 'semantic-ui-react';

class Details extends Component {
    panes = () => [
        {
            menuItem: 'Details', 
            render: () => <Tab.Pane inverted className='details-tab'>Details</Tab.Pane> 
        },
        { 
            menuItem: 'Network Graph', 
            render: () => <Tab.Pane inverted className='details-tab'>Graph</Tab.Pane> 
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
