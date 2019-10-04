
const awsServerlessExpress = require('aws-serverless-express');  
const awsServerlessExpressMiddleware = require('aws-serverless-express/middleware');

const express = require('express');  
const cors = require('cors');

const app = express();

app.use(awsServerlessExpressMiddleware.eventContext());
app.use(cors());

app.use('/answer', (req, res) => {
    // do nothing with inputs for now
    let question = req.query.q;
    let ttl = req.query.ttl;

    res.status(200)
    res.send(getResponse())
})

const getResponse = () => {
    var nums = [0,0,0].map(n => Math.floor(Math.random() * 19));

    return {
        name: "Stooges",
        response: Math.floor((nums[0] + nums[1] + nums[2]) / 3),
        children: [
            { name: "Larry", response: nums[0] },
            { name: "Curly", response: nums[1] },
            { name: "Moe", response: nums[2] },
        ]
    }
}

app.listen(3005, () => console.log('Listening on port 3000.')); // ignored by lambda

const server = awsServerlessExpress.createServer(app);
exports.handler = (event, context) => awsServerlessExpress.proxy(server, event, context);