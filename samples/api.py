from urllib import parse

import requests

def post_auth(data, headervernemqhook):
    """
    POST "auth"


    Returns: 
        JSON response from the endpoint
    """
    url = "http://localhost:8000/auth"

    headers = {"vernemq-hook": headervernemqhook}
    resp = requests.post(url,
                         headers=headers,
                         json=data)

    resp.raise_for_status()
    return resp.json()

