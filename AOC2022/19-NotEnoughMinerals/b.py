
def filter_number_and_space_only(line):
    return ''.join([c for c in line if c.isdigit() or c.isspace()])
def parse_blueprint(line):
    numbers = filter_number_and_space_only(line).split()
    x = [int(n) for n in numbers]
    # [geode, obsidian, clay, ore]
    orecost = [0, 0, 0, x[1]]
    clycost = [0, 0, 0, x[2]]
    obscost = [0, 0, x[4], x[3]]
    geocost = [0, x[6], 0, x[5]]
    return [geocost, obscost, clycost, orecost]

def is_buildable(cost, res):
    return all([r >= c for r, c in zip(res, cost)])

# branch : (robots, res)
def branch_single(bp, branch):
    robots, res = branch
    nextres0 = [r + rob for rob, r in zip(robots, res)]
    canbuild = [is_buildable(cost, res) for cost in bp]

    if canbuild[0]:
        nextres = [r - c for r, c in zip(nextres0, bp[0])]
        nextrobots = robots.copy()
        nextrobots[0] += 1
        return [(nextrobots, nextres)]
    # if canbuild[1]:
    #     nextres = [r - c for r, c in zip(nextres0, bp[1])]
    #     nextrobots = robots.copy()
    #     nextrobots[1] += 1
    #     return [(nextrobots, nextres)]

    next_branches = [(robots, nextres0)]
    for i, can in enumerate(canbuild):
        if can:
            nextrobots = robots.copy()
            nextrobots[i] += 1
            nextres = [r - c for r, c in zip(nextres0, bp[i])]
            next_branches.append((nextrobots, nextres))
    return next_branches

def branch(bp, branches):
    return sum([branch_single(bp, branch) for branch in branches], [])

def prune_branch(branches):
    branches = sorted(branches, reverse=True)
    maxrob = branches[0][0]
    maxgeo, maxobs, _, _ = maxrob
    branches = [b for b in branches if b[0][0] >= maxgeo - 1 and b[0][1] >= maxobs - 3]
    # branches = [b for b in branches if b[0][0] >= maxgeo - 1]
    # branches = [b for b in branches if b[0][0] == maxgeo]

    pruned = [branches[0]]
    for i in range(1,len(branches)):
        prevrob, prevres = pruned[-1]
        rob, res = branches[i]
        if rob != prevrob:
            pruned.append(branches[i])
        else:
            prev_is_better = all([p >= r for p, r in zip(prevres, res)])
            if not prev_is_better:
                pruned.append(branches[i])
            # if not (prevres[0] > res[0]):
            #     pruned.append(branches[i])
            #     prevb = branches[i]
    return pruned


def iter(id,bp, branches, n):
    for i in range(n):
        branches = branch(bp, branches)
        # branches = prune_branch(branches)
        branches = prune_branch(branches)[:5000]
        print(id, " : ", i+1,len(branches), branches[0])
        # for b in branches[:10]: print(b)
        # print("------------")
    return branches

def test_bp(id, bp):
    res = [0, 0, 0, 0]
    rob = [0, 0, 0, 1]
    branches = [(rob, res)]
    branches = iter(id,bp, branches, 32)
    res = [b[1] for b in branches]
    geos = [r[0] for r in res]
    maxgeo = max(geos)
    return maxgeo

# ---------------------------------

# with open('inpex.txt', 'r') as f:
with open('input.txt', 'r') as f:
# with open('inptest.txt', 'r') as f:
    lines = f.read().splitlines()[:3]

maxgeos = [test_bp(i+1, parse_blueprint(line)) for i, line in enumerate(lines)]

p = 1
for m in maxgeos: p *= m

print(maxgeos)
print(p)
