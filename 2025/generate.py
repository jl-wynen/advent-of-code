import re
import shutil
import sys
from pathlib import Path

TEMPLATE_PATH = Path(__file__).resolve().parent / "template"


def existing_solutions() -> tuple[int]:
    return tuple(
        int(m[1])
        for path in Path(__file__).resolve().parent.iterdir()
        if (m := re.match(r"s(\d\d)", path.name))
    )


def next_number() -> int:
    if len(sys.argv) == 2:
        return int(sys.argv[1])
    return max(existing_solutions() or [0]) + 1


def next_name() -> str:
    return f"s{next_number():02d}"


NEXT_NAME = next_name()


def render_template(out: Path) -> None:
    shutil.copytree(TEMPLATE_PATH, out)
    template_main = out / "src" / "template.rs"
    template_main.rename(template_main.with_stem(NEXT_NAME))


def add_to_cargo_toml() -> None:
    cargo_toml_path = Path(__file__).resolve().parent / "Cargo.toml"
    with open(cargo_toml_path, "a") as f:
        f.write(
            f"""
[[bin]]
name = "{NEXT_NAME}"
path = "{NEXT_NAME}/src/{NEXT_NAME}.rs"
"""
        )


def main() -> None:
    out_path = Path(__file__).resolve().parent / NEXT_NAME
    render_template(out_path)
    add_to_cargo_toml()


if __name__ == "__main__":
    main()
