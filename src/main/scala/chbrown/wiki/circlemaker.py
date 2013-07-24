import re
import math
from bottle import route, run, response

distances = []
for line in open('circles.csv'):
    m = re.match('(\d+)-(\d+),([.0-9]+)', line)
    if m:
        year1, year2, distance = m.groups()
        distances += [(float(year1), float(year2), math.log(float(distance)))]

min_distance = min(distance for year1, year2, distance in distances)
max_distance = max(distance for year1, year2, distance in distances)


def mean(*args):
    return sum(args) / float(len(args))


def renormalize(x, domain_min=0, domain_max=1, range_min=0, range_max=1):
    '''Rearrange a variable in the 0-1 domain to a new range'''
    domain_diff = domain_max - domain_min
    range_diff = range_max - range_min
    return (((x - domain_min) / domain_diff) * range_diff) + range_min


@route('/favicon.ico')
def favicon():
    return ''


@route('/')
def index():
    response.content_type = 'image/svg+xml'
    px_per_year = 6

    width = 100 * px_per_year
    contents = '<line x1="0" y1="0" x2="%0.5f" y2="0" style="stroke:rgb(255,0,0); stroke-width:1" />' % width

    circles = []
    for year1, year2, distance in distances:
        year_diff = year2 - year1
        year_mean = mean(year1, year2) - 1800
        # 1800 - 1900
        cx = year_mean * px_per_year
        r = year_diff * px_per_year / 2.0

        stroke_width = renormalize(distance, min_distance, max_distance, 3.0, 0.01)
        opacity = renormalize(distance, min_distance, max_distance, 0.5, 0.01)
        # green = renormalize(distance, min_distance, max_distance, 0.0, 100.0)
        # color = 'rgba(100%%, %0.1f%%, 0%%, %0.3f)' % (green, opacity)
        style = 'fill:none; stroke: black; stroke-width: %0.3f; opacity: %0.3f' % (stroke_width, opacity)
        circles += ['<circle cx="%0.5f" cy="0" r="%0.5f" style="%s" />' % (cx, r, style)]

    contents += '\n'.join(circles)
    return '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 -300 600 600">%s</svg>' % contents
    # <svg:svg width="4cm" height="8cm" version="1.1" baseProfile="tiny" >

run(host='localhost', port=1445, debug=True, reloader=True)
