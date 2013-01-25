#!/usr/bin/python

import glob
import re

print 'Run Type,AMR Levels,Cores,Measure Index,Memory Used (KB)'

for filename in glob.glob('*_memory_*.log'):
    name_matches = re.findall(r'^(.*)_([0-9])_([0-9]*)\.log$', filename)[0]
    runtype = name_matches[0]
    amrlevel = int(name_matches[1])
    cores = int(name_matches[2])

    filehandle = open(filename, 'r')
    measureIndex = 0
    for logline in filehandle.readlines():
        measurement = max(map(int, re.findall(r'[0-9]+', logline)))
        print '%s,%d,%d,%d,%s' \
            % (runtype, amrlevel, cores, measureIndex, measurement)
        measureIndex += 1
