"""Generate start program for each day"""

from pathlib import Path
import sys
import yaml


BASE_DIR = Path(__file__).resolve().parent


def process_file(infile, target_dir, day):
    with open(infile, 'r') as f:
        template = f.read()
    d = f'{day:02d}'
    with open(target_dir/infile.name, 'w') as f:
        f.write(template.format(day=d))


def update_stack_file(day):
    with open(BASE_DIR/"stack.yaml", 'r') as f:
        stack = yaml.safe_load(f)
    if (s := f's{day:02d}') not in stack['packages']:
        stack['packages'].append(s)
    with open(BASE_DIR/"stack.yaml", 'w') as f:
        yaml.dump(stack, f)
    

def main():
    day = int(sys.argv[1])
    target = BASE_DIR / f's{day:02d}'
    target.mkdir()
    for infile in (BASE_DIR / 'template').iterdir():
        process_file(infile, target, day)
    update_stack_file(day)


if __name__ == '__main__':
    main()
