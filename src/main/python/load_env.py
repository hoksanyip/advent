import os
from datetime import datetime
from dotenv import load_dotenv

load_dotenv()

year = os.getenv("YEAR")
board_id = os.getenv("BOARD_ID")
session_id = os.getenv("SESSION_ID")
day = datetime.now().day

stats_url = "https://adventofcode.com/{year}/leaderboard/private/view/{board_id}.json".format(board_id=board_id, year=year)
input_url = "https://adventofcode.com/{year}/day/{day}/input".format(year=year, day=day)

path_stats = "src/main/resources/stats.csv"
path_input_data = "{year}/src/main/resources/day{day}.txt".format(year=year, day=day)
