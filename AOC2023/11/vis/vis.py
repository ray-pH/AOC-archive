import matplotlib.pyplot as plt
import matplotlib.animation as animation
plt.style.use('dark_background')

def lerp(a, b, t): return a + (b - a) * t

# with open('visex.txt', 'r') as f:
with open('vis.txt', 'r') as f:
    lines = f.read().splitlines()

stars : list[list[tuple[int,int]]] = [eval(l) for l in lines]
center_id = len(stars[0]) // 2
nexp = len(stars)

frame = 300
fig, ax = plt.subplots() # type: ignore
ax : plt.Axes = ax

# axis = plt.axes()
x0data, y0data = zip(*stars[1])
scat = ax.scatter(x0data, y0data, s=100)

def animate(i):
    expansionlevel = i / frame * (nexp-1)
    id, t = divmod(expansionlevel, 1)
    id = int(id)

    x0data, y0data = zip(*stars[id])
    x1data, y1data = zip(*stars[id+1])
    x = [lerp(x0, x1, t) for x0, x1 in zip(x0data, x1data)]
    y = [lerp(y0, y1, t) for y0, y1 in zip(y0data, y1data)]

    xc = x[center_id]
    yc = y[center_id]
    x = [x - xc for x in x]
    y = [y - yc for y in y]

    # make size absolute (1 not relative to screen size)
    minx = min(x)
    maxx = max(x)
    miny = min(y)
    maxy = max(y)

    size = 1000**2/max(maxx-minx, maxy-miny)**2

    scat.set_offsets(list(zip(x, y)))
    scat.set_sizes([size for _ in range(len(x))])

    ax.set_xlim(minx, maxx)
    ax.set_ylim(miny, maxy)
    ax.set_title(f'Expansion level: {expansionlevel+1:.2f}')
    return scat,

anim = animation.FuncAnimation(fig, animate, frames=frame, interval=24)
plt.grid(alpha=0.2)
# plt.show()
anim.save('vis.gif', fps=24)



# scat = axis.scatter([s[0] for s in stars], [s[1] for s in stars], s=1)
