export default [
    {
        name: 'average',
        compute: (responses) => {
            let sum = responses.reduce((acc, r) => acc + r.response, 0)
            return Math.ceil(sum / responses.length);
        }
    },
    {
        name: 'first',
        compute: (responses) => {
            return responses[0].response;
        }
    },
    {
        name: 'fastest',
        compute: (responses) => {
            return responses.sort((a, b) => a.delay - b.delay)[0].response;
        }
    },
    {
        name: 'slowest',
        compute: (responses) => {
            return responses.sort((a, b) => b.delay - a.delay)[0].response;
        }
    }
]