import sys
import json
import pandas as pd

if __name__=="__main__":

    filename = sys.argv[1]
    dataframe = pd.read_csv(filename)

    print(dataframe.head())

    dataframe.to_csv("electronic_charging_station_in_gwangyang.csv", index=None)
