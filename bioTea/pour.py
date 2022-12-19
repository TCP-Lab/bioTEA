import logging
import os
import shutil
import tempfile
from pathlib import Path
from sys import exc_info
from typing import Optional, Tuple

import typer
from colorama import Fore

from bioTea import __version__
from bioTea.classes import GeoSample
from bioTea.docker_wrapper import (
    AnalyzeInterface,
    AnnotateInterface,
    PrepAffyInterface,
    PrepAgilInterface,
    get_all_versions,
    get_installed_versions,
    get_latest_version,
    pull_biotea_box_version,
    run_biotea_box,
)
from bioTea.utils.errors import ImageNotFoundError, SanityError
from bioTea.utils.strings import TEA_LOGO
from bioTea.utils.tools import (
    PathLike,
    download_ftp,
    make_geo_ftp,
    make_path_valid,
    parse_biotea_box_options,
)
from bioTea.utils.xml_parser import get_minimal_from_geo

log = logging.getLogger(__name__)


def stage_new_analysis(staging_path: PathLike) -> Tuple[Path, Path]:
    """Stage a new analysis folder, with an output and temp folder.

    Args:
        staging_path (PathLike): The path to the output folder

    Returns:
        Tuple[Path, Path]: A tuple with first element the (resolved) output path,
        and second element the path to the temporary folder.
    """
    log.info("Staging new analysis project...")
    log.debug(f"Resolving {staging_path}")
    staging_path = Path(staging_path).expanduser().resolve()
    if not staging_path.exists():
        os.makedirs(staging_path)
    log.debug(f"Staged {staging_path}.")
    log.info("Making a new temporary directory...")
    temp_dir = Path(tempfile.mkdtemp()).resolve()
    log.debug(f"Staged tempdir {temp_dir}")

    return (staging_path, temp_dir)


def retrieve_geo_data(
    output_folder: PathLike,
    geo_id: str,
    temp_dir: Optional[Path] = None,
    unpack_to_temp: bool = False,
):
    if temp_dir is None:
        staging_path, temp_dir = stage_new_analysis(output_folder)
    else:
        staging_path = make_path_valid(output_folder)

    geo_series = get_minimal_from_geo(geo_id, temp_dir)

    log.info("Retrieving sample raw data...")
    downloaded = download_ftp(
        make_geo_ftp(geo_id, "suppl"),
        staging_path / "raw_data",
        filter=lambda x: "_RAW" in x,
    )
    log.debug("Testing validity of downloaded archive...")
    # There should be just a single file
    if len(downloaded) > 1:
        log.warn("Downloaded more than one file. Using just the first one.")

    log.debug("Unpacking downloaded archive...")
    unpack_dir = temp_dir if unpack_to_temp else staging_path
    unpack_dir /= "unpacked_samples"
    log.debug(f"Unpacking dir is {unpack_dir}")
    shutil.unpack_archive(list(downloaded.values())[0], unpack_dir)
    unpacked_files = os.listdir(unpack_dir)

    log.debug(f"Unpacked {len(unpacked_files)} files: {unpacked_files}")

    log.debug("Sanity Check: Testing congruency with MINiML file...")
    miniml_samples = [
        sample.suppl_data_ftp.split("/")[-1] for sample in geo_series.samples
    ]
    if not all(miniml_sample in unpacked_files for miniml_sample in miniml_samples):
        log.error("Mismatching download and MINiML file. Aborting.")
        raise SanityError("Mismatching downloaded and MINiML files.")

    def add_realpath(sample: GeoSample, realpath: Path) -> GeoSample:
        sample.suppl_data_local_path = realpath
        return sample

    log.debug("Adding real paths to sample objects...")
    geo_series.samples = [
        add_realpath(sample, unpack_dir / sample.suppl_data_ftp.split("/")[-1])
        for sample in geo_series.samples
    ]

    log.info("Done retrieving project data.")
    return geo_series


def info_containers():
    """Get information on the downloaded and available BioTeaBox containers."""
    log.info("Getting container info...")
    local_versions = get_installed_versions()
    remote_versions = get_all_versions()

    c = lambda x: Fore.LIGHTGREEN_EX + str(x) + Fore.RESET
    col_remote_vers = [
        c(ver) if ver in local_versions else str(ver) for ver in remote_versions
    ]

    local_versions = [str(x) for x in local_versions]
    typer.echo(Fore.LIGHTBLUE_EX + "--- Container Info ---" + Fore.RESET)
    typer.echo("Locally installed: {}".format(", ".join(sorted(local_versions))))
    typer.echo("Remotely available: {}".format(", ".join(sorted(col_remote_vers))))
    typer.echo(
        Fore.LIGHTGREEN_EX
        + "Note: "
        + Fore.RESET
        + "Remote containers installed locally are highlighted in green."
    )
    typer.echo(Fore.LIGHTBLUE_EX + "----------------------" + Fore.RESET)


def info_biotea():
    """Get information on the version of bioTEA."""
    typer.echo(Fore.LIGHTBLUE_EX + "--- BioTEA Info ---" + Fore.RESET)
    typer.echo(Fore.LIGHTGREEN_EX + "Version: " + Fore.RESET + __version__)


def update_tool(yes: bool = False):
    """Check bioTEA and the container repos for updates.

    This command also updates the latest container, if needed.
    """
    # TODO: Once this is released on PyPA, implement checks for the tool update.
    log.info("Checking containers for updates...")
    try:
        if (latest := get_latest_version()) in get_installed_versions():
            log.info("Containers are up-to-date.")
            return
    except ImageNotFoundError:
        log.error(
            "No remote versions found, therefore there is no latest version. Cannot update."
        )
        return

    if not yes:
        do_update = typer.confirm(
            f"A new BioTeaBox version was found ({latest}). Update?",
        )
    else:
        log.debug("Skipped confirmation prompt")
        do_update = True
    if not do_update:
        log.debug("User abort.")
        return

    pull_biotea_box_version(latest)

    log.info("Done pulling new version.")


def retrieve(
    output_path: Path,
    geo_id: str,
):
    """Retrieve data from GEO regarding a GEO series.

    Also helps setting the options for the BioTeaBox analysis by providing a metadata file.
    """
    try:
        geo_series = retrieve_geo_data(output_folder=output_path, geo_id=geo_id)
    except Exception:
        log.error(
            "Failed to retrieve GEO data. Possibly, the MINiML file cannot be parsed correctly."
        )
        log.debug("Stack:", exc_info=exc_info())
        return

    log.info("Writing metadata...")
    with (output_path / "metadata.csv").open("w+") as fileout:
        geo_series.generate_metadata().to_csv(fileout, doublequote=True, index=False)

    log.info(f"Done retrieving data for {geo_id}.")


def prepare_agilent(
    input_dir: Path,
    output_file: Path,
    grep_pattern: str = "\.txt",
    version: str = "latest",
    log_name: Optional[str] = None,
    remove_controls: bool = False,
    plot_number: Optional[int] = None,
    plot_size: str = "12,5",
    use_png: bool = False,
    verbose: bool = False,
):
    """Prepare agilent expression data for analysis."""
    input_dir = make_path_valid(input_dir, dir=True)
    output_file = make_path_valid(output_file)

    if version == "latest":
        version = get_latest_version()

    try:
        plot_width, plot_height = [int(x) for x in plot_size.split(",")]
    except (ValueError, TypeError):
        log.error(f"Cannot parse {plot_size} as two numbers.")
        return

    if not version in ["bleeding"] and version not in get_all_versions():
        log.error(f"Invalid BioTeaBox version {version}")
        return

    if version not in get_installed_versions():
        pull_biotea_box_version(version)

    if plot_number is None:
        plot_number = 1_000_000

    args = {
        "output_file": output_file.name,
        "remove_controls": remove_controls,
        "n_plots": plot_number,
        "grep_pattern": grep_pattern,
        # Plot options
        "use_pdf": not use_png,
        "plot_width": plot_width,
        "plot_height": plot_height,
        "png_ppi": 600,
        "enumerate_plots": False,
    }

    response = run_biotea_box(
        "prepagil",
        arguments=args,
        interface=PrepAgilInterface,
        input_anchor=input_dir,
        output_anchor=output_file.parent,
        log_anchor=output_file.parent,
        version=version,
        console_level="debug" if verbose else "info",
        logfile_level="debug",
        log_name=log_name or "auto",
    )

    return response


def prepare_affymetrix(
    input_dir: Path,
    output_file: Path,
    version: str = "latest",
    log_name: Optional[str] = None,
    remove_controls: bool = False,
    plot_number: int = None,
    plot_size: str = "12,5",
    use_png: bool = False,
    verbose: bool = False,
):
    """Prepare affymetrix expression data for analysis."""
    input_dir = make_path_valid(input_dir, dir=True)
    output_file = make_path_valid(output_file)

    if version == "latest":
        version = get_latest_version()

    try:
        plot_width, plot_height = [int(x) for x in plot_size.split(",")]
    except (ValueError, TypeError):
        log.error(f"Cannot parse {plot_size} as two numbers.")
        return

    if not version in ["bleeding"] and version not in get_all_versions():
        log.error(f"Invalid BioTeaBox version {version}")
        return

    if version not in get_installed_versions():
        pull_biotea_box_version(version)

    if plot_number is None:
        plot_number = 1_000_000

    args = {
        "output.file": output_file.name,
        "remove.controls": remove_controls,
        "n_plots": plot_number,
        # Plot options
        "use_pdf": not use_png,
        "plot_width": plot_width,
        "plot_height": plot_height,
        "png_ppi": 600,
        "enumerate_plots": False,
    }

    response = run_biotea_box(
        "prepaffy",
        arguments=args,
        interface=PrepAffyInterface,
        input_anchor=input_dir,
        output_anchor=output_file.parent,
        log_anchor=output_file.parent,
        version=version,
        console_level="debug" if verbose else "info",
        logfile_level="debug",
        log_name=log_name or "auto",
    )

    return response


def run_biotea_box_analysis(
    options_path: Path,
    output_dir: Path,
    input_file: Path,
    version: str = "latest",
    log_name: Optional[str] = None,
    verbose: bool = False,
):
    """Run Differential Gene Expression with BioTeaBox."""
    print(TEA_LOGO)
    log.info(f"Biotea version {__version__}")

    options_path = make_path_valid(options_path)
    output_dir = make_path_valid(output_dir)
    input_file = make_path_valid(input_file)

    if version == "latest":
        version = get_latest_version()

    if not version in ["bleeding"] and version not in get_all_versions():
        log.error(f"Invalid BioTeaBox version {version}")
        raise ImageNotFoundError

    if version not in get_installed_versions():
        pull_biotea_box_version(version)

    log.info("Parsing options...")
    args = parse_biotea_box_options(options_path)

    log.debug("Adding input.file argument...")
    args["input.file"] = input_file.name

    response = run_biotea_box(
        "analyze",
        arguments=args,
        interface=AnalyzeInterface,
        input_anchor=input_file.parent,
        output_anchor=output_dir,
        log_anchor=output_dir,
        version=version,
        console_level="debug" if verbose else "info",
        logfile_level="debug",
        log_name=log_name or "auto",
    )

    return response


def annotate_file(
    target: Path,
    output: Path,
    annotation_database: str = "internal",
    version: str = "latest",
    log_name: Optional[str] = None,
    verbose: bool = False,
):
    """Annotate some expression data or DEA output with annotation data."""
    output = make_path_valid(output)

    if annotation_database != "internal":
        # TODO: Remove this once `annotations generate` gets implemented.
        log.warn(
            "Setting the annotation database to anything other than `internal` is currently not supported. Setting it to `internal` again. See issue *** on GitHub."
        )
        annotation_database = "internal"

    if annotation_database == "internal":
        annotation_database = True

    run_biotea_box(
        command="annotation",
        arguments={
            "expression_data_path": target.name,
            "output_path": output.name,
            "database_name": annotation_database,
        },
        interface=AnnotateInterface,
        input_anchor=target.parent,
        output_anchor=output.parent,
        log_anchor=output.parent,
        version=version,
        console_level="debug" if verbose else "info",
        logfile_level="debug",
        log_name=log_name or "auto",
    )
