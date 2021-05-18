"""

Notice we don't follow the same test structure as in unit tests.
That's because functional tests should test the code from "outside".
So we don't need to follow any hierarchy.

"""
from pathlib import Path

import pytest
from genetwork.__main__ import cli
from typer.testing import CliRunner


class TestGenetwork:

    @pytest.fixture(autouse=True, scope="class")
    def runner(self) -> CliRunner:
        return CliRunner()

    def test_genetwork_returns_zero_for_polska(self, runner: CliRunner, resources_dir: Path):
        result = runner.invoke(cli, [str(resources_dir / 'polska.xml'), "--max-iters", "10", "--pop-size", "10"])
        assert not result.exception
        assert result.exit_code == 0

    def test_genetwork_returns_zero_for_germany50(self, runner: CliRunner, resources_dir: Path):
        result = runner.invoke(cli, [str(resources_dir / 'germany50.xml'), "--max-iters", "10", "--pop-size", "10"])
        assert not result.exception
        assert result.exit_code == 0

    def test_genetwork_prints_something(self, runner: CliRunner, resources_dir: Path):
        result = runner.invoke(cli, [str(resources_dir / 'polska.xml'), "--max-iters", "10", "--pop-size", "10"])
        assert result.output

    def test_genetwork_prints_help(self, runner: CliRunner):
        result = runner.invoke(cli, ['--help'])
        assert "Usage" in result.output
