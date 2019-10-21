import React, { Component } from "react";
import "./Details.css";
import responses from '../responses';

import { Tab, List, Divider, Table } from 'semantic-ui-react';

class Details extends Component {
    responseList = (response, index = 0) => {
        let details = `${responses[response.response]} (${response.response}${response.delay ? ` , ${response.delay}ms` : ''})`;

        return (
            <List inverted key={index}>
                <List.Item>
                    <List.Icon name={response.children ? 'share alternate' : 'circle'}/>
                    <List.Content>
                        <List.Header>{response.name}</List.Header>
                        <List.Description>{details}</List.Description>
                        {response.children && <List.List>
                            {response.children.map((c, i) => this.responseList(c, i))}
                        </List.List>}
                    </List.Content>
                </List.Item>
            </List>
        )
    }

    responseTable = () => (
        <Table celled inverted className='response-table'>
            <Table.Body>
                <Table.Row>
                    <Table.Cell className='response-table-label'>Response</Table.Cell>
                    <Table.Cell>{`${responses[this.props.response]} (${this.props.response})`}</Table.Cell>
                </Table.Row>
                <Table.Row>
                    <Table.Cell className='response-table-label'>Aggregation Strategy</Table.Cell>
                    <Table.Cell>{this.props.strategy}</Table.Cell>
                </Table.Row>
                <Table.Row>
                    <Table.Cell className='response-table-label'>Total Latency</Table.Cell>
                    <Table.Cell>{this.props.delay}</Table.Cell>
                </Table.Row>
            </Table.Body>
        </Table>
    )
    
    panes = () => [
        {
            menuItem: 'Details', 
            render: () => <Tab.Pane inverted className='details-tab'>
                {this.responseTable()}
                <Divider/>
                {this.props.responses.map((r, i) => this.responseList(r, i))}
            </Tab.Pane> 
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
