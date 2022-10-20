import joblib
from flask import Flask, request
import json
import numpy as np

app = Flask(__name__)

model = joblib.load('randomForestClassifier')

@app.route('/predict', methods=['POST'])

def predict():
    event = json.loads(request.data)
    values = event['values']
    values = list(values)
    pre = np.array(values)
    pre = pre.reshape(1, -1)
    res = model.predict(pre)
    print(res)
    return str(res[0])


if __name__ == '__main__':
    app.run(debug = True)
