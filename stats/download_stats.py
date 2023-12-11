import requests
import xml.etree.ElementTree as ET

def parse_line(line : str) -> tuple[int,int,int]:
    root = ET.fromstring(line)
    # day, gold, silver
    return (
        int(root.text.strip()),     #type: ignore
        int(root[0].text.strip()),  #type: ignore
        int(root[1].text.strip())   #type: ignore
    )

def download_year(year : int) :
    link = f"https://adventofcode.com/{year}/stats"
    f = requests.get(link)
    text = f.text
    table = text.split('<main>\n')[1].split('</main>')[0].splitlines()[1:-1]
    data = [parse_line(line) for line in table]

    with open(f"stats_{year}.txt", "w") as f:
        for line in data: f.write(f"{line[0]},{line[1]},{line[2]}\n")
    print(f"Downloaded stats_{year}.txt")

for year in range(2015, 2023+1):
    download_year(year)
