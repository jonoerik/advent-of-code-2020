from day07 import *

from pathlib import Path


def load_answer(path: Path) -> int:
    with open(path) as f:
        return int(f.read().strip())


def pytest_generate_tests(metafunc):
    data_dir = Path("data")
    sample_files = sorted([p for p in data_dir.iterdir() if p.name.startswith("sample")])
    if "answer1" in metafunc.fixturenames:
        suffix = "answer1"
    elif "answer2" in metafunc.fixturenames:
        suffix = "answer2"

    arguments = []
    for s in sample_files:
        a = s.parent / (s.name + "." + suffix)
        if a.exists():
            arguments.append((s, load_answer(a)))
    metafunc.parametrize(["input_path", suffix], arguments, ids=[str(a) for (a, _) in arguments])


def test_part1(input_path: Path, answer1: int) -> None:
    assert part1(load(input_path)) == answer1


def test_part2(input_path: Path, answer2: int) -> None:
    assert part2(load(input_path)) == answer2
