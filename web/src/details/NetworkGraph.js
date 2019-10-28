import { Component } from "react";
import { DataSet, Network } from "vis-network";

class NetworkGraph extends Component {
    componentDidMount = () => {
        let res = { 
            name: 'Hub',
            children: this.props.agentResponses 
        };

        let nodesAndEdges = [];
        this.makeNodes(res, 0, nodesAndEdges);

        let nodes = new DataSet(nodesAndEdges.map(n => n.node));
        let edges = new DataSet(nodesAndEdges.map(n => n.edge).filter(n => n.from !== n.to));

        var container = document.getElementById("network");

        var data = {
            nodes: nodes,
            edges: edges
        };

        var options = {
            nodes: {
                shape: "circle"
            }
        };

        new Network(container, data, options);
    };

    makeNodes = (rootNode, parentId, nodesAndEdges) => {
        let nextId = nodesAndEdges.length;

        nodesAndEdges.push(this.makeNode(rootNode, parentId, nextId));

        if (rootNode.children) {
            rootNode.children.map((c, i) => this.makeNodes(c, nextId, nodesAndEdges));
        }
    }

    makeNode = (node, parentId, id) => ({
        node: { 
            id: id, 
            label: node.name, 
            shape: id === 0 ? 'triangleDown' : 'circle', 
            color: id === 0 ? '#255fff' : '#FFF',
            size: id === 0 ? 50 : 25,
        },
        edge: { from: parentId, to: id, color: { color: 'grey' }} 
    })

    render = () => ''
}

export default NetworkGraph;
