import os
import subprocess
from pathlib import Path

import pytest

FIXTURE_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)),
    "test_files",
)


def test_biotea_installed():
    subprocess.run(["biotea", "--help"]).check_returncode()


class TestBioTEACommands:
    def test_biotea_info(self):
        # Test that all commands work and can be accessed.
        subprocess.run(["biotea", "info"]).check_returncode()


def contains_logs(dir):
    return "biotea_log.log" in [path.name for path in Path(dir).iterdir()]


def contains_output(dir):
    # Are there files in this folder?
    # It's a pretty weak assertion, yeah, but I don't have time for anything better.
    return len(list(Path(dir).iterdir())) > 2


# Some terrible integration tests, just so we have *some* testing capabilities.
@pytest.mark.datafiles(
    os.path.join(FIXTURE_DIR, "Fake_options_files/fake_expression_run.yaml"),
    os.path.join(FIXTURE_DIR, "fake_expression_matrix.csv"),
)
def test_generic_run(tmpdir, datafiles):
    res = subprocess.run(
        [
            "biotea",
            "analyze",
            "--log-name",
            "biotea_log",
            "--version",
            "bleeding",
            os.path.join(datafiles, "fake_expression_run.yaml"),
            tmpdir,
            os.path.join(datafiles, "fake_expression_matrix.csv"),
        ],
        capture_output=True,
    )
    assert res.returncode == 0, f"Command failed: {res.stderr}"
    # Test the presence of the logs
    assert contains_logs(tmpdir)


@pytest.mark.datafiles(
    os.path.join(FIXTURE_DIR, "Fake_options_files/fake_expression_run_wet.yaml"),
    os.path.join(FIXTURE_DIR, "fake_expression_matrix.csv"),
)
def test_generic_run_with_plots(tmpdir, datafiles):
    res = subprocess.run(
        [
            "biotea",
            "analyze",
            "--version",
            "bleeding",
            "--log-name",
            "biotea_log",
            os.path.join(datafiles, "fake_expression_run_wet.yaml"),
            tmpdir,
            os.path.join(datafiles, "fake_expression_matrix.csv"),
        ],
        capture_output=True,
    )
    assert res.returncode == 0, f"Command failed: {res.stderr}"
    # Test the presence of the logs and plots and outputs
    assert contains_logs(tmpdir)
    assert contains_output(tmpdir)
    assert (Path(tmpdir) / "analyze Figures").exists()
    # Did (at least one) plot get saved?
    assert len(list((Path(tmpdir) / "analyze Figures").iterdir())) > 0
    # Test that the correspondence table got saved
    assert (Path(tmpdir) / "correspondence_table.csv").exists()


@pytest.mark.datafiles(
    os.path.join(
        FIXTURE_DIR, "Fake_options_files/fake_expression_run_with_annots.yaml"
    ),
    os.path.join(FIXTURE_DIR, "fake_expression_matrix_annotatable.csv"),
)
def test_generic_run_with_annotations(tmpdir, datafiles):
    res = subprocess.run(
        [
            "biotea",
            "analyze",
            "--version",
            "bleeding",
            "--log-name",
            "biotea_log",
            os.path.join(datafiles, "fake_expression_run_with_annots.yaml"),
            tmpdir,
            os.path.join(datafiles, "fake_expression_matrix_annotatable.csv"),
        ],
        capture_output=True,
    )
    assert res.returncode == 0, f"Command failed: {res.stderr}"
    # Test the output files actually have the annotation columns
    assert contains_logs(tmpdir)
    assert contains_output(tmpdir)
    assert (Path(tmpdir) / "analyze Figures").exists()
    with (Path(tmpdir) / "Limma - DEG Table treated-control.csv").open("r") as testfile:
        header = testfile.readline()
    # The double quotes is due to csv encoding.
    assert '"SYMBOL"' in header.split(",")


@pytest.mark.datafiles(
    os.path.join(FIXTURE_DIR, "Fake_options_files/fake_counts_run.yaml"),
    os.path.join(FIXTURE_DIR, "fake_count_matrix.csv"),
)
def test_generic_counts_run(tmpdir, datafiles):
    res = subprocess.run(
        [
            "biotea",
            "analyze",
            "--version",
            "bleeding",
            "--log-name",
            "biotea_log",
            os.path.join(datafiles, "fake_counts_run.yaml"),
            tmpdir,
            os.path.join(datafiles, "fake_count_matrix.csv"),
        ],
        capture_output=True,
    )
    assert res.returncode == 0, f"Command failed: {res.stderr}"
    # Test the presence of the logs
    assert contains_logs(tmpdir)


@pytest.mark.datafiles(
    os.path.join(FIXTURE_DIR, "Fake_options_files/fake_counts_run.yaml"),
    os.path.join(FIXTURE_DIR, "fake_count_matrix_unsorted.csv"),
)
def test_generic_unsorted_cols(tmpdir, datafiles):
    res = subprocess.run(
        [
            "biotea",
            "analyze",
            "--version",
            "bleeding",
            os.path.join(datafiles, "fake_counts_run.yaml"),
            tmpdir,
            os.path.join(datafiles, "fake_count_matrix_unsorted.csv"),
        ],
        capture_output=True,
    )
    assert res.returncode == 0, f"Command failed: {res.stderr}"


@pytest.mark.datafiles(
    os.path.join(FIXTURE_DIR, "Fake_options_files/fake_batches_run.yaml"),
    os.path.join(FIXTURE_DIR, "fake_count_matrix.csv")
)
def test_batches(tmpdir, datafiles):
    res = subprocess.run(
        [
            "biotea",
            "analyze",
            "--version",
            "bleeding",
            os.path.join(datafiles, "fake_batches_run.yaml"),
            tmpdir,
            os.path.join(datafiles, "fake_count_matrix.csv")
        ]
    )
    assert res.returncode == 0, f"Command failed: {res.stderr}"

@pytest.mark.datafiles(
    os.path.join(FIXTURE_DIR, "Fake_options_files/fake_batches_run_two_contrasts.yaml"),
    os.path.join(FIXTURE_DIR, "fake_count_matrix.csv")
)
def test_batches_two_contrasts(tmpdir, datafiles):

    res = subprocess.run(
        [
            "biotea",
            "analyze",
            "--version",
            "bleeding",
            os.path.join(datafiles, "fake_batches_run_two_contrasts.yaml"),
            tmpdir,
            os.path.join(datafiles, "fake_count_matrix.csv")
        ]
    )
    assert res.returncode == 0, f"Command failed: {res.stderr}"