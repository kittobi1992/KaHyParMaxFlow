import csv
import json

csvfile = open('benchmark_set_stats.csv', 'r')
jsonfile = open('benchmark_set_stats.json', 'w')

fieldnames = ["graph","HNs","HEs","pins","avgHEsize","sdHEsize","minHEsize","heSize90thPercentile","Q1HEsize","medHEsize","Q3HEsize","maxHEsize",
              "avgHNdegree", "sdHNdegree","minHNdegree","hnDegree90thPercentile","maxHNdegree","Q1HNdegree","medHNdegree","Q3HNdegree","density"]
reader = csv.DictReader( csvfile, fieldnames)
out = json.dumps( [ row for row in reader ] )
jsonfile.write(out)