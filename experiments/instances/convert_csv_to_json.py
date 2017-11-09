import csv
import json

############## Convert Full Benchmark Set to JSON ##############

csvfile = open('benchmark_set_stats.csv', 'r')
jsonfile = open('full_benchmark_set_stats.json', 'w')

fieldnames = ["graph","HNs","HEs","pins","avgHEsize","sdHEsize","minHEsize","heSize90thPercentile","Q1HEsize","medHEsize","Q3HEsize","maxHEsize",
              "avgHNdegree", "sdHNdegree","minHNdegree","hnDegree90thPercentile","maxHNdegree","Q1HNdegree","medHNdegree","Q3HNdegree","density"]
reader = csv.DictReader( csvfile, fieldnames)

full_benchmark_set = []
for row in reader:
    for name in fieldnames:
        if name != "graph":
            row[name] = float(row[name])
    full_benchmark_set = full_benchmark_set + [row]
out = json.dumps( full_benchmark_set )
jsonfile.write(out)
jsonfile.close()

############## Convert Benchmark Subset to JSON ##############

subsetfile = open('benchmark_subset.txt', 'r')
jsonfile = open('benchmark_subset_stats.json', 'w')

subset = set([x.strip() for x in subsetfile.readlines()])
benchmark_subset = [x for x in full_benchmark_set if x['graph'] in subset]

out = json.dumps( benchmark_subset )
jsonfile.write(out)
jsonfile.close()

############## Convert Parameter Tunning Benchmark Set to JSON ##############

parameterfile = open('parameter_tunning_subset.txt', 'r')
jsonfile = open('parameter_tunning_subset_stats.json', 'w')

subset = set([x.strip() for x in parameterfile.readlines()])
parameter_subset = [x for x in full_benchmark_set if x['graph'] in subset]

out = json.dumps( parameter_subset )
jsonfile.write(out)
jsonfile.close()