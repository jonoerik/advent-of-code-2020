import day06

from pathlib import Path

def load_answer(path: Path) -> int:
    with open(path) as f:
        return int(f.read().strip())


def pytest_generate_tests(metafunc):
    data_dir = Path("data")
    sample_files = sorted([p for p in data_dir.iterdir() if p.name.startswith("sample")])
    if "answer1" in metafunc.fixturenames:
        args = []
        for s in sample_files:
            a = s.parent / (s.name + ".answer1")
            if a.exists():
                args.append((s, load_answer(a)))
        metafunc.parametrize(["input_path", "answer1"], args, ids=[str(a) for (a, _) in args])


def test_part1(input_path: Path, answer1: int) -> None:
    assert answer1 == day06.part1(day06.load(input_path))
