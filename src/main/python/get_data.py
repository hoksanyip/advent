from lxml import html
import requests
import sys
from . import load_env as env


def download_data():
    # Get response from URL
    response = requests.get(env.input_url, cookies={"session": env.session_id})
    # Write to output folder
    with open(env.path_input_data, "w") as f:
        f.write(response.text)


if __name__ == "__main__":
    download_data()
