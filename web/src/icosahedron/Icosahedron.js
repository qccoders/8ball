import React, { Component } from "react";
import "./Icosahedron.css";
import responses from "../responses"

class Icosahedron extends Component {
    state = { hidden: true }

    componentDidUpdate(prevProps) {
        if (this.props.refreshing !== prevProps.refreshing) {
            this.setState({ hidden: this.props.refreshing })
        }
    }

    render() {
        return (
            <div className="icosahedron-container">
                {this.state.hidden || <div className="icosahedron fadeIn" style={{ opacity: this.state.opacity }}>
                    <div className="text">
                        {responses[this.props.response]}
                    </div>
                </div>}
            </div>
        );
    }
}

export default Icosahedron;
