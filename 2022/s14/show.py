from pathlib import Path
import numpy as np
from PIL import Image
vals = {
    '.': 0,
    '+': 50,
    'o': 150,
    '#': 255
}

base = Path(__file__).resolve().parent
infiles = (base / "result1.dat", base/"result2.dat")
outfiles = (base / "result1.png", base/"result2.png")

for infile, outfile in zip(infiles, outfiles):
    with open(infile, 'r') as f:
        a = np.array([[vals[x] for x in l.strip()] for l in f.readlines()],
                    dtype=np.uint8)
    img = Image.fromarray(a)
    img.save(outfile)
