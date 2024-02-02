import os
from pathlib import Path
import subprocess
from typing import Iterable, Sequence, Tuple
import pytest


def get_path_from_env(env_var_name: str) -> Path:
    path = os.environ.get(env_var_name)
    assert path is not None, f"Set the {env_var_name} environment variable"
    return Path(path)


def get_test_suite_root() -> Path:
    test_suite_root = get_path_from_env("TEST_SUITE_ROOT")
    assert (
        test_suite_root.is_dir()
    ), "The TEST_SUITE_ROOT environment variable must point to a directory"
    return test_suite_root


def get_compiler_binary() -> Path:
    binary = get_path_from_env("COMPILER_BINARY")
    assert (
        binary.is_file()
    ), "The COMPILER_BINARY environment variable must point to a file"
    return binary.absolute()


def discover_tests() -> Sequence[Tuple[Path]]:
    def get_test_files(root: Path) -> Iterable[Path]:
        paths = [root / p for p in os.listdir(root)]
        return (p for p in paths if p.is_file() and p.suffix == ".herb")

    test_suite_root = get_test_suite_root()
    assert (
        test_suite_root.is_dir()
    ), "The TEST_SUITE_ROOT environment variable must point to a directory"

    collected_tests = list(get_test_files(test_suite_root))
    assert len(collected_tests) != 0, "No tests collected"
    return collected_tests


def get_test_id(herb_file: Path) -> str:
    return str(herb_file.relative_to(get_test_suite_root()))


def run_binary(args: Sequence[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
    )


def compile(input: Path, output: Path) -> None:
    result = run_binary([str(get_compiler_binary()), str(input), "-o", str(output)])
    if result.returncode != 0:
        pytest.fail(
            f"[Compilation Error] {get_test_id(input)}\n\n----- CAPTURED OUTPUT -----\n{result.stdout}"
        )


def run_test(test_file: Path, tmpdir: Path) -> None:
    compiled_executable_path = tmpdir / "prog"
    compile(test_file, compiled_executable_path)
    result = run_binary([str(compiled_executable_path)])
    if result.returncode != 0:
        pytest.fail(
            f"[Execution Error] {get_test_id(test_file)}\n\n----- CAPTURED OUTPUT -----\n{result.stdout}"
        )


@pytest.mark.parametrize("herb_file", discover_tests(), ids=get_test_id)
def test_exec(herb_file: Path, tmpdir: Path) -> None:
    run_test(herb_file, tmpdir)
