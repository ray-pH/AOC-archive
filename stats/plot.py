import matplotlib.pyplot as plt


def getdata(year : int):
    with open(f"stats_{year}.txt", 'r') as f:
        data = [tuple(map(int, line.split(','))) for line in f.read().splitlines()]
    day = [line[0] for line in data if line[2] > 0]
    gold = [line[1] for line in data if line[2] > 0]
    silver = [line[2] for line in data if line[2] > 0]
    total = [line[1] + line[2] for line in data if line[2] > 0]
    return day, gold, silver, total

def plot(year : int):
    day, _, _, total = getdata(year)
    plt.plot(day, total, label=f"{year}")

def plot_relative(year : int):
    day, _, _, total = getdata(year)
    total = [t / total[-1] for t in total]
    plt.plot(day, total, label=f"{year}")

# for year in range(2015, 2023+1):
for year in range(2023, 2015-1, -1):
    plot(year)
    # plot_relative(year)
plt.legend()
plt.grid()
plt.show()

