# ---------------------------------------------------------------------------
# Age of every president, senator and representative, by CALENDAR YEAR.
#
# Companion to the lab's main Voteview series, not a replacement. Voteview's
# HSall_members.csv carries `born` as a YEAR and is organized by Congress;
# this file uses exact birthDATES and is organized by calendar year, so ages
# are exact to the day and a year is a year rather than a two-year Congress.
#
# FETCHED 2026-09-15. Sources (@unitedstates/congress-legislators, CC0):
#
#   https://unitedstates.github.io/congress-legislators/legislators-current.json
#     HTTP 200 - 1,468,926 bytes - 539 members
#   https://unitedstates.github.io/congress-legislators/legislators-historical.json
#     HTTP 200 - 13,483,039 bytes - 12,231 members
#   https://unitedstates.github.io/congress-legislators/executive.json
#     HTTP 200 - 46,163 bytes - 80 presidents and vice presidents
#
# METHOD
#   Anyone holding a seat at any point in year Y is counted in Y, aged as of
#   1 July Y - or, if their service that year ended before then, as of their
#   last day. Someone who switches chamber counts once in each. House counts
#   include non-voting delegates, so they exceed 435.
#
# KNOWN GAPS (see README.md)
#   542 members, nearly all antebellum, have no birthdate and are excluded
#   from the statistics but counted in `nb`. Six records implying an age
#   under 22 at swearing-in are impossible and dropped.
#
# OUTPUT
#   ages.json   year -> chamber ("rep"/"sen"/"prez") -> {n, min, c, med, mean,
#               yng, old, nb}, where c[i] is the number of members aged min+i;
#               "prez" also carries "all", every president that year in the
#               order they held office
# ---------------------------------------------------------------------------

import json, os, statistics, urllib.request
from datetime import date

def pd_(s):
    y,m,d = s.split('-'); return date(int(y),int(m),int(d))

# raw/ holds the sources as they arrive; it is gitignored repo-wide, and this
# script re-fetches anything missing.
BASE = 'https://unitedstates.github.io/congress-legislators/'
RAW = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'data', 'raw')
os.makedirs(RAW, exist_ok=True)
def raw(f):
    p = os.path.join(RAW, f)
    if not os.path.exists(p):
        print('fetching', f)
        urllib.request.urlretrieve(BASE + f, p)
    return p

leg = json.load(open(raw('legislators-historical.json'))) + json.load(open(raw('legislators-current.json')))
ex  = json.load(open(raw('executive.json')))

Y0, Y1 = 1789, 2026
CH = ['prez','sen','rep']
# year -> chamber -> {person_key: (age, name)}
data = {y:{c:{} for c in CH} for y in range(Y0,Y1+1)}
missing = {y:{c:set() for c in CH} for y in range(Y0,Y1+1)}
took_office = {y:{} for y in range(Y0,Y1+1)}   # prez only: order them by term, not age
missing_people = set()

def fullname(p):
    n=p['name']
    return (n.get('official_full') or (n.get('first','')+' '+n.get('last',''))).strip()

def add(people, wanted):
    for p in people:
        bd = p['bio'].get('birthday')
        key = p['id'].get('bioguide') or fullname(p)
        nm  = fullname(p)
        for t in p['terms']:
            c = t['type']
            if c not in wanted: continue
            s = pd_(t['start']); e = pd_(t['end'])
            for y in range(max(Y0,s.year), min(Y1,e.year)+1):
                lo = max(s, date(y,1,1)); hi = min(e, date(y,12,31))
                if lo > hi: continue
                if not bd:
                    missing[y][c].add(key); missing_people.add(key); continue
                ref = min(max(date(y,7,1), lo), hi)
                b = pd_(bd)
                age = ref.year - b.year - ((ref.month, ref.day) < (b.month, b.day))
                if not (22 <= age <= 105): continue  # below 22 = impossible, bad birthdate in source
                prev = data[y][c].get(key)
                if prev is None or age > prev[0]:
                    data[y][c][key] = (age, nm)
                if c == 'prez':
                    took_office[y][key] = min(took_office[y].get(key, lo), lo)

add(leg, {'sen','rep'})
add(ex,  {'prez'})

out = {}
for y in range(Y0, Y1+1):
    ent = {}
    for c in CH:
        vals = sorted(data[y][c].values())
        if not vals: continue
        ages = [a for a,_ in vals]
        lo = min(ages); hi = max(ages)
        counts = [0]*(hi-lo+1)
        for a in ages: counts[a-lo]+=1
        ent[c] = {
            'n': len(ages), 'min': lo, 'c': counts,
            'med': round(statistics.median(ages),1),
            'mean': round(statistics.fmean(ages),1),
            'yng': [vals[0][1], vals[0][0]],
            'old': [vals[-1][1], vals[-1][0]],
            'nb': len(missing[y][c]),
        }
        # Three presidents held office in 1841 and again in 1881, so the
        # youngest/oldest pair does not name them all: carry the full list.
        if c == 'prez':
            order = sorted(data[y][c], key=lambda k: took_office[y][k])
            ent[c]['all'] = [[data[y][c][k][1], data[y][c][k][0]] for k in order]
    if ent: out[y] = ent

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'ages.json')
json.dump(out, open(OUT,'w'), separators=(',',':'))
print('years', len(out), 'bytes', len(open(OUT).read()))
print('people missing bdays appearing in data:', len(missing_people))
for y in (1789,1800,1850,1900,1950,2000,2025,2026):
    e=out.get(y,{})
    print(y, {c:(e[c]['n'],e[c]['med'],e[c]['nb']) for c in e})
