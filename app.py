from flask import Flask, jsonify, request
from flask_restful import Api, Resource, reqparse
import pickle
import numpy as np
import json


app = Flask(__name__)

@app.route("/material", methods=["POST"])
# Define how the api will respond to the post requests
def MaterialCharacterizer():
    payload = request.json
    X = np.array(payload['data'])
    prediction = model.predict(X)
    return jsonify(prediction.tolist())
    
    
if __name__ == '__main__':
    # Load model
    with open('model.pickle', 'rb') as f:
        model = pickle.load(f)

    app.run(debug=True)