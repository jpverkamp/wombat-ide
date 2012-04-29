#!/usr/bin/env python

from __future__ import print_function

import re
import sys
       
re_ref = re.compile('_([^_]+)_')

if len(sys.argv) == 1:
    print('Usage: generate-html [definition files]')
    sys.exit(0)

def output(data, fout = sys.stdout):
    if not 'name' in data or not data['name'] or not 'forms' in data or not data['forms']:
        return

    print('<div>', file = fout)

    print('<a name="%s"></a>' % data['name'], file = fout)
    if 'alias' in data and data['alias']:
        print('\n'.join(['<a name="%s"></a>' % name for name in data['alias']]), file = fout)

    for form in data['forms']:
        args = ' '.join(['<i>%s</i>' % arg for arg in form.split()])
        if args: args = ' ' + args
        print('<code>(%s%s)</code><br />' % (data['name'], args), file = fout)

    if 'return' in data and data['return']:
        print('=> %s<br />' % data['return'], file = fout)
    else:
        print('=> unspecified<br />', file = fout)

    if 'text' in data and data['text']:
        print('<br />', file = fout)
        print(re_ref.sub(lambda match: '<code>%s</code>' % match.groups(), data['text'].strip()).replace('\n', '<br />\n'), file = fout)
        print('</div>', file = fout)

    if 'alias' in data and data['alias']:
        print('<br />\naliases: %s\n<br />' % ' '.join(['<code>%s</code>' % name for name in data['alias']]), file = fout)

    print('<hr />', file = fout)

for input_file in sys.argv[1:]:
    with open(input_file, 'r') as f:
        datas = {}
        data = {}

        for line in f:
            line = line.strip()
            
            if line == '---':
                if 'name' in data:
                    datas[data['name']] = data
                data = {}

            elif line.startswith('name:'): 
                data['name'] = line[5:].strip()
            
            elif line.startswith('form:'): 
                if not 'forms' in data: data['forms'] = []
                data['forms'].append(line[5:].strip())

            elif line.startswith('return:'):
                data['return'] = line[7:].strip()

            elif line.startswith('alias:'):
                if not 'alias' in data: data['alias'] = []
                data['alias'].append(line[6:].strip())

            else: 
                if not 'text' in data: data['text'] = ''
                data['text'] += line + '\n'
        
        if 'name' in data:
            datas[data['name']] = data

        with open(input_file.replace('.txt', '.htm'), 'w') as fout:
            for name in sorted(datas):
                output(datas[name], fout)
            datas = {}
