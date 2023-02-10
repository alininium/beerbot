import spacy
import json

from flask import Flask, request
from datetime import datetime  

app = Flask(__name__)

nlp = spacy.load("ru_core_news_sm")

def flatten(s):
    return [item for sublist in s for item in sublist]

@app.route('/analyze', methods=['GET'])
def analyze():
    print('---- NEW MESSAGE ----')
    current_time = datetime.now()
    time_stamp = current_time.timestamp()
    date_time = datetime.fromtimestamp(time_stamp)
    print(date_time)
    try: 
        data = request.json["string"]
        print('converted to json')
        print(data)
        analysis = nlp(data)
        analysis = [token for token in analysis if token.pos_ in ["VERB"]]

        plurals = flatten([token.morph.get("Number") for token in analysis]).count("Plur")
        genders = flatten([t.morph.get("Gender") for t in analysis])

        masc = genders.count("Masc")
        fem = genders.count("Fem")
        print(masc, fem, plurals)
        return json.dumps({
            "response" : {
                "fem": fem,
                "masc": masc,
                "plur": plurals
            }
        })

    except Exception as e:
        print(e)
        return "Unknown exception", 500

if __name__ == '__main__':
    app.run(host='127.0.0.1', port='8000')
