import pandas as pd
import requests
from datetime import datetime
from . import load_env as env


def request_stats():
    # Get response from URL
    response = requests.get(env.stats_url, cookies={"session": env.session_id})
    # Parse to JSON
    data = response.json()
    # Format json to dataframe
    data_stats = pd.DataFrame([
        {
            "day": int(day),
            "part": int(part),
            "timestamp": datetime.fromtimestamp(star["get_star_ts"]),
            "name": member["name"],
            "stars": member["stars"],
            "score": member["local_score"]
        }
        for _, member in data["members"].items()
        for day, parts in member["completion_day_level"].items()
        for part, star in parts.items()
    ])
    # Sort records by last part first.
    data_stats = data_stats.sort_values(
        ["day", "part", "timestamp", "score"],
        ascending=[False, False, True, False]
    )
    # Write output
    data_stats.to_csv(env.path_stats, index=False)


if __name__ == "__main__":
    request_stats()
