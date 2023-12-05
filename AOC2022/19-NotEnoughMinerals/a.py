
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

# branch : (res, robots)
def branch_single(bp, branch):
    robots, res = branch
    nextres0 = [r + rob for r, rob in zip(res, robots)]
    canbuild = [is_buildable(cost, res) for cost in bp]

    # if canbuild[0]:
    #     nextres = [r - c for r, c in zip(nextres0, bp[0])]
    #     nextrobots = robots.copy()
    #     nextrobots[0] += 1
    #     return [(nextrobots, nextres)]
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
    branches = [b for b in branches if b[0][0] >= maxgeo and b[0][1] >= maxobs - 2]
    # branches = [b for b in branches if b[0][0] == maxgeo]

    pruned = [branches[0]]
    prevb = branches[0]
    for i in range(1,len(branches)):
        prevrob, prevres = prevb
        rob, res = branches[i]
        if rob != prevrob:
            pruned.append(branches[i])
            prevb = branches[i]
        else:
            prev_is_better = all([r >= p for r, p in zip(prevres, res)])
            if not prev_is_better:
                pruned.append(branches[i])
                prevb = branches[i]
            # if not (prevres[0] > res[0]):
            #     pruned.append(branches[i])
            #     prevb = branches[i]
    return pruned


def iter(id,bp, branches, n):
    for i in range(n):
        branches = branch(bp, branches)
        branches = prune_branch(branches)
        print(id, " : ", i+1,len(branches), branches[0])
        # for b in branches: print(b)
    return branches

def test_bp(id, bp):
    res = [0, 0, 0, 0]
    rob = [0, 0, 0, 1]
    branches = [(rob, res)]
    branches = iter(id,bp, branches, 24)
    maxgeo = branches[0][1][0]
    return maxgeo

# ---------------------------------

# with open('inpex.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()

maxgeos = [test_bp(i+1, parse_blueprint(line)) for i, line in enumerate(lines)]
quality = [g * (i+1) for i,g in enumerate(maxgeos)]
print(maxgeos)
print(sum(quality))
# for l in lines:
#     bp = parse_blueprint(lines[1])
#     max = test_bp(bp)
