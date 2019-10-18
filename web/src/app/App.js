import React, { Component } from "react";
import "./App.css";
import Icosahedron from "../icosahedron/Icosahedron";

class App extends Component {
    state = { refreshing: false, response: 0 }

    refresh = (e) => {
        this.setState({ refreshing: true}, () => {
            this.setState({ response: this.state.response + 1 }, () => {
                this.setState({ refreshing: false })
            })
        })
    }

    render() {
        return (
            <div className="app">
                <span className="window">
                    <button onClick={this.refresh}>Next Response</button>
                    <br/>
                    <Icosahedron refreshing={this.state.refreshing} response={this.state.response}/>
                </span>
            </div>
        );
    }        
}

export default App;
