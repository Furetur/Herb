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
        herb_files = (p for p in root.iterdir() if p.is_file() and p.suffix == ".herb")
        yield from herb_files

        subdirs = (p for p in root.iterdir() if p.is_dir())
        for subdir in subdirs:
            yield from get_test_files(subdir)

    test_suite_root = get_test_suite_root()
    assert (
        test_suite_root.is_dir()
    ), "The TEST_SUITE_ROOT environment variable must point to a directory"

    collected_tests = list(get_test_files(test_suite_root))
    assert len(collected_tests) != 0, "No tests collected"
    return collected_tests


def maybe_skip_test(test_file: Path):
    if ".skip" in test_file.suffixes:
        pytest.skip(reason="Test annotated with '.skip'")


def get_test_id(herb_file: Path) -> str:
    return str(herb_file.relative_to(get_test_suite_root()))


def run_binary(args: Sequence[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
    )


def compile(test_id: str, input: Path, output: Path) -> None:
    result = run_binary([str(get_compiler_binary()), str(input), "-o", str(output)])
    if result.returncode != 0:
        pytest.fail(
            f"[Compilation Error] {test_id}\n\n----- CAPTURED OUTPUT -----\n{result.stdout}"
        )
    if not output.exists():
        pytest.fail(
            f"[Compiled binary does not exist] {test_id}\n\n----- CAPTURED OUTPUT -----\n{result.stdout}"
        )


def execute_compiled_binary(test_id: str, exec_path: Path) -> None:
    result = run_binary([str(exec_path)])
    if result.returncode != 0:
        pytest.fail(
            f"[Execution Error] {test_id}\n\n----- CAPTURED OUTPUT -----\n{result.stdout}"
        )


def run_test(test_file: Path, tmpdir: Path) -> None:
    maybe_skip_test(test_file)
    test_id = get_test_id(test_file)
    compiled_executable_path = tmpdir / "prog"
    compile(test_id, test_file, compiled_executable_path)
    execute_compiled_binary(get_test_id(test_file), compiled_executable_path)


@pytest.mark.parametrize("herb_file", discover_tests(), ids=get_test_id)
def test_exec(herb_file: Path, tmpdir: Path) -> None:
    run_test(herb_file, tmpdir)
