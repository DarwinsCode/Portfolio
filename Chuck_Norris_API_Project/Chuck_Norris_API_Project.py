
"""Uses an API to retrieve a Super Sweet Chuck Norris joke!"""
#Author: Nicolas Saenz
#11/02/2024

import requests
import json


def main():

    # Take user input for agreeing to hear a joke
    try:
        var = input("Welcome! Would you like to hear a Chuck Norris joke? Type Y for yes or N for No")
        var = var.upper()
    except:
        var = input("Please enter Y to hear a Chuck Norris or N to move on.")
        var = var.upper()


    Line = "Here is your joke : "

    # location for science joke API request
    url = "https://api.chucknorris.io/jokes/random?category=science"

    payload = ""
    headers = {
        "cache-control" : "no-cache"
    }

    # request call for joke from url. Save response in payload var
    response = requests.request("GET", url, headers=headers, data=payload)

    # User response check, and printing only the joke from json dump in a prettyish way.
    if var == "Y":
        parsed = json.loads(response.text)
        print(Line + json.dumps(parsed["value"], indent=4, sort_keys=False))

if __name__ == "__main__":
    main()
