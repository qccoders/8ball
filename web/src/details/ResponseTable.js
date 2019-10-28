import React from "react";
import { Table } from "semantic-ui-react";

const ResponseTable = (props) => {
    let { response, strategy, delay, responses } = props;

    return (
        <Table celled inverted className='response-table'>
            <Table.Body>
                <Table.Row>
                    <Table.Cell className='response-table-label'>Response</Table.Cell>
                    <Table.Cell>{`${responses[response]} (${response})`}</Table.Cell>
                </Table.Row>
                <Table.Row>
                    <Table.Cell className='response-table-label'>Aggregation Strategy</Table.Cell>
                    <Table.Cell>{strategy}</Table.Cell>
                </Table.Row>
                <Table.Row>
                    <Table.Cell className='response-table-label'>Total Latency</Table.Cell>
                    <Table.Cell>{delay}</Table.Cell>
                </Table.Row>
            </Table.Body>
        </Table>
    )
};

export default ResponseTable;
