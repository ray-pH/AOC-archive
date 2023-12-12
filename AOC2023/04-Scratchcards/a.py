# just testing how do i solve this if i have to do it in python
# definitely simpler that prolog

def parse_line(line):
    lstr, rstr = line.split(': ')[1].split(' | ')
    lnums = {int(x) for x in lstr.split(' ') if x != ''}
    rnums = {int(x) for x in rstr.split(' ') if x != ''}
    return lnums, rnums

def count_matching(card):
    l, r = card
    same = l.intersection(r)
    return len(same)

def point(c): return 2 ** (c-1) if c > 0 else 0

# ------------ main ------------

with open('input.txt', 'r') as f:
    lines = f.read().splitlines()

cards = [parse_line(line) for line in lines]
samecount = [count_matching(card) for card in cards]
points = [point(c) for c in samecount]
print(sum(points))
