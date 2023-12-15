from collections import namedtuple
import matplotlib.pyplot as plt
import matplotlib.patches as patches
plt.style.use('dark_background')

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

bgcolor = "#0f0f23"
fig, ax = plt.subplots(facecolor=bgcolor) #type: ignore
ax : plt.Axes = ax
ax.set_facecolor(bgcolor)

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
        ax.add_patch(patches.Polygon(
            xy=list(zip(x,y)),
            # fill=True,
            alpha=0.5,
            facecolor='#009900',
            edgecolor=None,
        ))

    n0,n1 = singlenums[i]
    x = [xleft, xleft, xright, xright]
    y = [n0+1, n0, n1, n1+1]
    ax.add_patch(patches.Polygon(
        xy=list(zip(x,y)),
        fill=True,
        color='#ffff66',
        # color='#229922',
    ))

ax.set_ylim(miny, maxy)
ax.set_xlim(0, sectiongap + len(groups)*(sectiongap + dx_section))
ax.set_xticklabels(names) #type: ignore
plt.savefig('vis05.png', facecolor=bgcolor, dpi=200)
# plt.savefig('vis05e.png', facecolor=bgcolor, dpi=200)
# plt.show()
