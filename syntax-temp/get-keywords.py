#!/usr/bin/env python

import posixpath
import re
import urllib2

url = 'http://www.scheme.com/csug8/summary.html'

re_line = re.compile('<tr><td nowrap>(.*)</td><td>(.*)</td><td align=right><a href="([^"]+)">.*</a></td></tr>')

f = urllib2.urlopen(url, 'r')
data = f.read()
f.close()

def getname(call):
    for tosplat in ['<tt>', '</tt>', '<i>', '</i>']:
        call = call.replace(tosplat, '')

    for fromme, tome in [('&lt;', '<'), ('&gt;', '>'), ('&nbsp;', ' '), ('&amp;', '&')]:
        call = call.replace(fromme, tome)

    return call.strip('()').split(' ')[0]

def fixurl(url):
    return posixpath.normpath('http://www.scheme.com/csug8/' + url).replace('http:/', 'http://')

keywords = [(getname(call), kind, 2, fixurl(link)) for (call, kind, link) in re_line.findall(data)]

for keyword in keywords:
    print '%s,%s,%s,%s' % keyword
