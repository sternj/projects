import os
import csv
dictionary = dict()
def main():
    for filename in os.listdir('.'):
        if filename.endswith(".csv"):
            readFirstLine(filename)
    for key in sorted(dictionary.keys()):
        print(key + "   " + dictionary[key])
def readFirstLine(filename):
    with open(filename) as f:
        reader = csv.reader(f)
        headers = next(f)
    for str in headers:
        if str in dict:
            dictionary[str] += 1
        else:
            dictionary[str] = 1



