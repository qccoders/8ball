import React from "react";
import { List } from "semantic-ui-react";

const ResponseList = (props) => {
    let { index, response, responses } = props;

    let details = `${responses[response.response]} (${response.response}${
        response.delay ? ` , ${response.delay}ms` : ""
    })`;

    return (
        <List inverted key={index}>
            <List.Item>
                <List.Icon
                    name={response.children ? "share alternate" : "circle"}
                />
                <List.Content>
                    <List.Header>{response.name}</List.Header>
                    <List.Description>{details}</List.Description>
                    {response.children && (
                        <List.List>
                            {response.children.map((c, i) =>
                                <ResponseList key={i} index={i} response={c} responses={responses} />
                            )}
                        </List.List>
                    )}
                </List.Content>
            </List.Item>
        </List>
    );
};

export default ResponseList;
