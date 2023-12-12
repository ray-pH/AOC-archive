from PIL import Image, ImageDraw, ImageFont

def parse_insides_coord(insidesstr : str) -> list[tuple[int,int]]:
    inside_coords = []
    for ins in eval(insidesstr):
        row, cols = ins
        for c in cols:
            inside_coords.append((row, c))
    return inside_coords

def ftransform(maxrow, maxcol, imgx, imgy, pad):
    def transform(pos : tuple[int,int]) -> tuple[int,int]:
        row, col = pos
        x = int(col/(maxcol+1) * (imgx - pad) + pad/2)
        y = int(row/(maxrow+1) * (imgy - pad) + pad/2)
        return (x, y)
    return transform


# with open('visex3.txt', 'r') as f:
# with open('visex4.txt', 'r') as f:
# with open('visex5.txt', 'r') as f:
with open('vis.txt', 'r') as f:
    lines = f.read().splitlines()
# with open('../inpex3.txt', 'r') as f:
# with open('../inpex4.txt', 'r') as f:
# with open('../inpex5.txt', 'r') as f:
with open('../input.txt', 'r') as f:
    maps = f.read().splitlines()

pipe_coords = eval(lines[1])
inside_coords = parse_insides_coord(lines[2])
maxrow = max([r for r, _ in pipe_coords])
maxcol = max([c for _, c in pipe_coords])
imgsize_x = 600
imgsize_y = int(imgsize_x * (maxrow+1) / (maxcol+1))
pad = 50
gridsize_x = (imgsize_x - pad) / (maxcol+1)
gridsize_y = (imgsize_y - pad) / (maxrow+1)

pipe_coords = list(map(ftransform(maxrow, maxcol, imgsize_x, imgsize_y, pad), pipe_coords))
inside_coords = list(map(ftransform(maxrow, maxcol, imgsize_x, imgsize_y, pad), inside_coords))


bgcolor = "#0f0f23"
img = Image.new('RGB', (imgsize_x, imgsize_y), color = bgcolor)
draw = ImageDraw.Draw(img, 'RGBA')


fontpath = "/usr/share/fonts/truetype/hack/Hack-Bold.ttf"
font = ImageFont.truetype(fontpath, 30)
# for r in range(len(maps)):
#     for c in range(len(maps[r])):
#         x = pad/2 + c * gridsize_x - 4
#         y = pad/2 + r * gridsize_y - 24
#         draw.text((x, y), maps[r][c], font=font, fill=(200,200,200))

draw.polygon(pipe_coords, outline=None, fill=(0,200,0,100))

for coords in inside_coords:
    # create a rectangle gridsize_x x gridsize_y
    gz_x = gridsize_x * 0.7
    gz_y = gridsize_y * 0.7
    topleft = coords[0] - gz_x/2, coords[1] - gz_y/2
    botleft = coords[0] - gz_x/2, coords[1] + gz_y/2
    botrigh = coords[0] + gz_x/2, coords[1] + gz_y/2
    toprigh = coords[0] + gz_x/2, coords[1] - gz_y/2
    draw.polygon([topleft, botleft, botrigh, toprigh], outline=None, fill=(0,200,00,200))

draw.polygon(pipe_coords, outline='#aaa', width=2)

img.save('vis.png')
# img.show()
