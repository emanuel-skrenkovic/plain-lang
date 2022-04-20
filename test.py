#!/usr/bin/env python3

import os
import sys
import glob
import inspect
import subprocess

from dataclasses import dataclass

tests_path = "tests/"
executable = "target/debug/sage"


@dataclass
class TestStatus:
    exit_code: int
    stdout: str
    stderr: str


def test(func):
    func.is_test = True
    return func


def run_test(test_file):
    process = subprocess.run([executable, tests_path + test_file],
                             check=False,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    return TestStatus(process.returncode, str(process.stdout), str(process.stderr))


def get_tests():
    for f in inspect.getmembers(sys.modules[__name__], inspect.isfunction):
        func = f[1]
        if hasattr(func, 'is_test') and func.is_test:
            yield f


@test
def add():
    result = run_test("add.sg")
    assert result.exit_code == 0, result.stderr


@test
def let():
    result = run_test("let.sg")
    assert result.exit_code == 0, result.stderr


@test
def let_add():
    result = run_test("let_add.sg")
    assert result.exit_code == 0, result.stderr


@test
def let_redefinition():
    result = run_test("let_redefinition.sg")
    assert result.exit_code != 0, result.stderr


@test
def var_without_definition():
    result = run_test("var_without_definition.sg")
    assert result.exit_code == 0, result.stderr


@test
def var_with_definition():
    result = run_test("var_with_definition.sg")
    assert result.exit_code == 0, result.stderr


@test
def var_redefinition():
    result = run_test("var_redefinition.sg")
    assert result.exit_code == 0, result.stderr


@test
def var_postponed_definition():
    result = run_test("var_postponed_definition.sg")
    assert result.exit_code == 0, result.stderr


@test
def block_assignment():
    result = run_test("block_assignment.sg")
    assert result.exit_code == 0, result.stderr


@test
def block_scope():
    result = run_test("block_scope.sg")
    assert result.exit_code != 0, result.stderr


@test
def if_true():
    result = run_test("if_true.sg")
    assert result.exit_code == 0, result.stderr


@test
def if_false():
    result = run_test("if_false.sg")
    assert result.exit_code == 0, result.stderr


if __name__ == "__main__":
    for test in get_tests():
        test_name = test[0]
        test_func = test[1]

        failed = False

        try:
            test_result = test_func()
            print("[INFO]  \'{}\' -> \x1b[1;32;40m Success! \x1b[0m".format(test_name))
        except AssertionError as error:
            if failed != True:
                failed = True
            print("\x1b[1;31;40m[ERROR] \x1b[0m\'{}\'-> {}".format(test_name, error))

    sys.exit(failed)
