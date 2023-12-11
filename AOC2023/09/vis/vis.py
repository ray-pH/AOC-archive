import matplotlib.pyplot as plt
import matplotlib.cm as cm

# with open('visex.txt', 'r') as f:
with open('vis.txt', 'r') as f:
    lines = f.read().splitlines()

orders = eval(lines[1])
numbers = eval(lines[2])
# print(sorted(orders))
# print(numbers)

cmap = cm.get_cmap('gist_rainbow')
colorcount = len(set(orders))

# get colors for each order
colors = [cmap(i/(colorcount-1)) for i in range(colorcount)]


for nums, order in zip(numbers, orders):
    color = colors[order-1]
    plt.plot(nums, color=color)
# plt.legend()
plt.show()
