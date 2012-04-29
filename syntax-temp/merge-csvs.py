#!/usr/bin/env python

new_keywords = {}
for line in open('syntax-new.csv', 'r'):
    line = line.strip().split(',')
    new_keywords[line[0]] = line

for line in open('syntax-old.csv', 'r'):
    line = line.strip()

    if ',' in line:
        keyword, indent = line.split(',')
        if keyword in new_keywords:
            new_line = new_keywords[keyword]
            new_line[2] = indent
        else:
            new_line = [keyword, 'procedure', indent, '']
        print ','.join(new_line)
    else:
        print line
