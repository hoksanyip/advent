from src.main.python.get_data import download_data
from src.main.python.update_stats import request_stats


if __name__ == "__main__":
    # Check if new data is available.
    print("Updating data from today...")
    download_data()
    # Update stats
    print("Updating leaderboard stats...")
    request_stats()
    # Finished
    print("Done")
