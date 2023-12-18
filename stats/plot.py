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
    total = [t / total[-1] * 100 for t in total]
    plt.plot(day, total, label=f"{year}")

def plot_silverperc(year : int):
    day, _, silver, total = getdata(year)
    silverperc = [s/t * 100 for s,t in zip(silver,total)]
    plt.plot(day, silverperc, label=f"{year}")

def plot_bar(year : int):
    day, _, _, total = getdata(year)
    plt.bar(day, total, label=f"{year}")
    plt.xticks(day, day)
def plot_silverperc_bar(year : int):
    day, _, silver, total = getdata(year)
    silverperc = [s/t * 100 for s,t in zip(silver,total)]
    plt.bar(day, silverperc, label=f"{year}")
    plt.xticks(day, day)
def plot_droprate(year : int):
    day, _, _, total = getdata(year)
    day = day[:-1]
    droprate = [(1 - t1/t2)*100 for t1,t2 in zip(total, total[1:])]
    plt.bar(day, droprate, label=f"{year}")
    plt.xticks(day, day)
def plot_gold_droprate(year : int):
    day, gold, _, _ = getdata(year)
    day = day[:-1]
    droprate = [(1 - t1/t2)*100 for t1,t2 in zip(gold, gold[1:])]
    plt.bar(day, droprate, label=f"{year}")
    plt.xticks(day, day)

def call_plotfunction(f, years : list, ylabel : str):
    plt.clf()
    for year in years: f(year)
    plt.xlabel("Day")
    plt.ylabel(ylabel)
    plt.legend()
    # plt.grid()
    plt.show()

years = list(range(2023, 2015-1, -1))
# inverted so that the recent years are plotted first and have nicer colors

call_plotfunction(plot, years, "Solver count")
call_plotfunction(plot_relative, years, "Solver count relative to day 1 (%)")
call_plotfunction(plot_silverperc, years, "percentage of users who only solved part 1 (%)")

call_plotfunction(plot_bar, [2023], "Solver count")
call_plotfunction(plot_silverperc_bar, [2023], "percentage of users who only solved part 1 (%)")
call_plotfunction(plot_droprate, [2023], "Drop rate (%)")
call_plotfunction(plot_gold_droprate, [2023], "Gold Drop rate (%)")
