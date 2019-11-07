import random
from flask import Flask
from flask import jsonify

app = Flask(__name__)

@app.route('/answer')
def answer():
    return jsonify(
        name='Python+Flask',
        response=random.randint(0,20)
    )

if __name__ == '__main__':
    app.run(app.run(host='0.0.0.0', port=5001))