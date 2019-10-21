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
    },
    {
        name: 'sentiment',
        compute: (responses) => {
            let positive = 0, neutral = 0, negative = 0;
            responses.forEach(response => {
                switch (Math.floor(response.response / 5)) {
                    // 0-9
                    case 0:
                    case 1:
                        positive++;
                        break;
                    // 10-14
                    case 2:
                        neutral++;
                        break;
                    // 15-19
                    case 3:
                        negative++;
                        break;
                    default:
                        throw new Error('should never happen')
                }
            });

            if (positive > neutral && positive > negative) {
                return Math.floor(Math.random() * 10);
            }
            else if (neutral > negative) {
                return Math.floor(Math.random() * 5) + 10;
            }
            else {
                return Math.floor(Math.random() * 5) + 15;
            }
        }
    }
]