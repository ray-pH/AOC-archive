# orarr = [[131,673,234,103,18],[201,96,342,965,150],[630,803,746,422,111],[537,699,497,121,956],[805,732,524,37,331]]
f = open("input.txt", "r")
# f = open("inpex.txt", "r")
orarr = [[int(i) for i in strs[:-1]] for strs in f]
f.close()
grids = [[False for i in range(len(orarr[0]))] for j in range(len(orarr))]
targety = len(grids)-1
targetx = len(grids[0]) - 1

class expander:
	def __init__(self,y,x):
		self.x = x
		self.y = y
		self.val = grids[y][x]
		self.on = True

	def doExpand(self,j,i):
		x = self.x
		y = self.y
		if x == targetx and y == targety:
			return False
		if i<0 or j<0 or i>=len(grids[0]) or j>=len(grids):
			return False
		if grids[j][i] is False : 
			return True
		else:
			if grids[j][i] > (grids[y][x]+orarr[j][i]):
				return True
			else:
				return False
	def expand(self):
		toexpand = []
		x = self.x
		y = self.y
		self.val = grids[y][x]
		if self.doExpand(y-1,x):
			grids[y-1][x] = orarr[y-1][x] + self.val
			toexpand.append(expander(y-1,x))
		if self.doExpand(y+1,x):
			grids[y+1][x] = orarr[y+1][x] + self.val
			toexpand.append(expander(y+1,x))
		if self.doExpand(y,x-1):
			grids[y][x-1] = orarr[y][x-1] + self.val
			toexpand.append(expander(y,x-1))
		if self.doExpand(y,x+1):
			grids[y][x+1] = orarr[y][x+1] + self.val
			toexpand.append(expander(y,x+1))
		self.on = False
		return toexpand

def main():
	grids[0][0] = orarr[0][0]
	init = orarr[0][0]
	exps = [expander(0,0)]
	while len(exps) > 0:
		newexps = []
		for e in exps : newexps.extend(e.expand())
		exps.extend(newexps)
		exps = [e for e in exps if e.on]

	# for g in grids: print(g)
	print(grids[targety][targetx] - init)

if __name__ == '__main__':
	# pass
	main()
