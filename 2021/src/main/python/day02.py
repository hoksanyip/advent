import pandas as pd
import numpy as np

#################################################
# Import
#################################################
data = pd.read_table("src/main/resources/day02.txt", sep=" ", header=None)
data.columns = ["action", "amount"]

#################################################
# Prepare
#################################################
data["aim"] = 0
data.loc[data.action == "up", "aim"] = -data.loc[data.action == "up", "amount"]
data.loc[data.action == "down", "aim"] = data.loc[data.action == "down", "amount"]
data["aim"] = data["aim"].cumsum()

#################################################
# Process
#################################################
data_actions = data.loc[data.action == "forward"]
x = np.sum(data_actions["amount"])
y = np.sum(data_actions["aim"] * data_actions["amount"])

#################################################
# Output
#################################################
print(f"{x * y = }")
