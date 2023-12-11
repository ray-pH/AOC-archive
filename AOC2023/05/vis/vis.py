from collections import namedtuple
import matplotlib.pyplot as plt
import matplotlib.patches as patches

Section = namedtuple('Section',[ 
    'input', 'split', 'mapped', 'sorted', 'combined'
])
def parse_group(groupstr : str):
    arr = [eval(s) for s in groupstr.splitlines()]
    return Section(*arr)

with open('vis.txt', 'r') as f:
# with open('visex.txt', 'r') as f:
    rawtext = f.read()
groupsstr, singlestr = rawtext.split('::')
groups = groupsstr.split('\n\n')[:-1]
groups = [parse_group(s) for s in groups]

singlenums = singlestr.strip()[:-1]
singlenums = eval(f'[{singlenums}]')
singlenums = list(reversed(singlenums))

sectiongap = 5
dx_section = 95

names = ["seed",
    "soil",
    "fertilizer",
    "water",
    "light",
    "temp",
    "humidity",
    "location",
 ]


miny = 9999999999999999999999999
maxy = 0
for i,g in enumerate(groups):
    print(g)
    print('-----------')
    xleft = sectiongap + i*(sectiongap + dx_section)
    xright = xleft + dx_section
    for [l0,u0],[l1,u1] in zip(g.split, g.mapped):
        miny = min(miny,l0,l1)
        maxy = max(maxy,u0,u1)
        x = [xleft, xleft, xright, xright]
        y = [u0+1, l0, l1, u1+1]
        plt.gca().add_patch(patches.Polygon(
            xy=list(zip(x,y)),
            fill=True,
            alpha=0.5,
        ))

    n0,n1 = singlenums[i]
    x = [xleft, xleft, xright, xright]
    y = [n0+1, n0, n1, n1+1]
    plt.gca().add_patch(patches.Polygon(
        xy=list(zip(x,y)),
        fill=True,
        color='#d62728',
    ))

plt.ylim(miny, maxy)
plt.xlim(0, sectiongap + len(groups)*(sectiongap + dx_section))
plt.gca().set_xticklabels(names)
plt.savefig('vis05.png')
# plt.show()
