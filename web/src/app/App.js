import React from "react";
import "./App.css";
import Icosahedron from "../icosahedron/Icosahedron";

function App() {
    return (
        <div className="app">
            <span className="window">
                <Icosahedron result={0}/>
            </span>
        </div>
    );
}

export default App;
