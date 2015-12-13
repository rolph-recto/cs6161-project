#!/usr/bin/env python
import csv

def main():
    results = {}
    with open('experiments.csv','r') as csvfile:
        experiments = csv.reader(csvfile)
        for row in experiments:
            desc = row[0]
            total = int(row[1])
            sat = int(row[2])
            if not row[0] in results:
                results[desc] = {}
                results[desc]['total'] = total
                results[desc]['sat'] = sat
            else:
                results[desc]['total'] += total
                results[desc]['sat'] += sat


    for experiment,stats in sorted(results.items()):
        score = float(stats['sat']) / float(stats['total'])
        print '{:30} : {:.4f}'.format(experiment, score)

if __name__ == '__main__':
    main()
