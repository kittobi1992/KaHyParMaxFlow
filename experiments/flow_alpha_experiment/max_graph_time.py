import csv
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("graph", type=str)
parser.add_argument("k", type=int)

args = parser.parse_args()

graph = args.graph
k = args.k

############## Convert Full Benchmark Set to JSON ##############

csvfile = open('effectiveness_table.csv', 'r')

fieldnames = ["","graph","k","min_time","max_time"]
reader = csv.DictReader( csvfile, fieldnames)

max_time = {}
for row in reader:
    if row['graph'] not in max_time.keys():
        max_time[row['graph']] = {}
    max_time[row['graph']][int(row['k'])] = 3*float(row['max_time'])

print(int(round(max_time[graph][k])))