# -*- coding: utf-8 -*-
"""
extract_ms_vec.py

Created on Fri Jul 13 13:45:15 2018

@author: mthornton
"""


import io
import csv

with open('pc166.csv', 'rb') as dfile:
    reader = csv.reader(dfile)
    header = reader.next()
    states = []
    for row in reader:
        states.append(row[0])


def load_vectors(fname, whitelist):
    fin = io.open(fname, 'r', encoding='utf-8', newline='\n', errors='ignore')
    n, d = map(int, fin.readline().split())
    data = {}
    for line in fin:
        tokens = line.rstrip().split(' ')
        if (tokens[0] in whitelist):
            data[tokens[0]] = map(float, tokens[1:])
    fin.close()
    return data

data = load_vectors('./crawl-300d-2M.vec/crawl-300d-2M.vec', states)
with open('state_vectors.csv', 'wb') as dfile:
    writer = csv.writer(dfile)
    for s in states:
        writer.writerow([s] + data[s])
