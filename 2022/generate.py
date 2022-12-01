"""Generate start program for each day"""

from pathlib import Path
import sys


BASE_DIR = Path(__file__).resolve().parent


def process_file(infile, target_dir, day):
    with open(infile, 'r') as f:
        template = f.read()
    d = f'{day:02d}'
    with open(target_dir/infile.name, 'w') as f:
        f.write(template.format(day=d))


def main():
    day = int(sys.argv[1])
    target = BASE_DIR / f's{day:02d}'
    target.mkdir()
    for infile in (BASE_DIR / 'template').iterdir():
        process_file(infile, target, day)


if __name__ == '__main__':
    main()
