import importlib.resources as pkg_resources
import logging
import shutil
from enum import Enum
from pathlib import Path
from typing import Optional

import typer
import yaml

import bioTea.pour as pour
from bioTea import resources
from bioTea.docker_wrapper import SpecialCommand, run_special_biotea_command
from bioTea.utils.strings import INTRO, TEA_LOGO
from bioTea.utils.tools import make_path_valid
from bioTea.wizard import interactive_metadata_to_biotea_box_options, wizard

log = logging.getLogger(__name__)

# CLI structure
# biotea
#   - info: Get version information and more.
#       - biotea: Get info about the biotea tool, its version (and DOI?)
#       - containers: Get available containers, as well as those locally installed.
#   - update: Check for container and tool updates.
#   - wizard: run the wizard
#   - retrieve: Retrieve (and format) data from GEO
#   - prepare
#       - affymetrix: Prep affymetrix data for analysis
#       - agilent: Prep agilent data for analysis
#   - analyze: Analyze with BioTEA box an expression file
#   - annotate: Annotate an expression matrix
#       - generate: Generate annotations for some organism.

cli_root = typer.Typer(no_args_is_help=True)
info = typer.Typer()
prepare = typer.Typer()
annotate = typer.Typer()

cli_root.add_typer(info, name="info")
cli_root.add_typer(
    prepare, name="prepare", help="Prepare raw expression data to an expression matrix"
)
cli_root.add_typer(annotate, name="annotations", help="Annotate expression data.")


@cli_root.callback()
def context_handler():
    log.debug(f"Starting bioTEA.")


@info.callback(invoke_without_command=True)
def generic_info(ctx: typer.Context):
    """Get information on the status of the tool."""
    print(TEA_LOGO)
    if ctx.invoked_subcommand:
        return

    # This is only printed if there is no subcommand invoked.
    print(INTRO)


@info.command(name="containers")
def info_containers_command():
    """Get information on the downloaded and available BioTEA box containers."""
    pour.info_containers()


@info.command(name="biotea")
def info_biotea_command():
    """Get information on the version of bioTEA."""
    # Add some free real estate
    print("\n")
    pour.info_biotea()


@info.command(name="versions")
def info_biotea_command(
    version: str = typer.Argument(
        ..., help="Version of the bioTEA box to get versions from."
    )
):
    """Get the R package versions used inside the bioTEA container"""
    run_special_biotea_command(SpecialCommand.versions, version=version)


@cli_root.command(name="update")
def update_tool_command(
    force: bool = typer.Option(
        False, help="Skip the confirmation prompt and update immediately"
    )
):
    """Check bioTEA and the container repos for updates.

    This command also updates the latest container, if needed.
    """
    pour.update_tool(yes=force)


@cli_root.command(name="wizard")
def run_wizard_command():
    """Run the bioTEA wizard.

    The wizard helps in setting up, running, and exploring a BioTEA box analysis.
    """
    wizard()


@cli_root.command(name="retrieve")
def retrieve_command(
    output_path: Path = typer.Argument(
        ..., help="Path to a folder that will contain the output"
    ),
    geo_id: str = typer.Argument(
        ..., help="GEO id that needs to be retrieved, e.g. GSE15471"
    ),
):
    """Retrieve data from GEO regarding a GEO series.

    Also helps setting the options for the BioTEA box analysis by providing a metadata file.
    """
    pour.retrieve(output_path=output_path, geo_id=geo_id)


@prepare.command(name="agilent")
def prepare_agilent_command(
    input_dir: Path = typer.Argument(
        ..., help="Path to the folder with the input files"
    ),
    output_file: Path = typer.Argument(..., help="Path to the output file"),
    grep_pattern: str = typer.Argument(
        "\.txt$", help="Pattern with which to find the files"
    ),
    version: str = typer.Option("latest", help="Specify BioTEA box container version"),
    log_name: Optional[str] = typer.Option(
        None, help="Specify BioTEA box log name for the run"
    ),
    remove_controls: bool = typer.Option(False, help="Remove control probes?"),
    plot_number: Optional[int] = typer.Option(
        None, help="Maximum number of plots to show"
    ),
    plot_size: str = typer.Option("12,5", help="Size of plots as 'width,height'"),
    use_png: bool = typer.Option(
        False, help="Generate .png files instead of pdf files (600 ppi resolution)."
    ),
    verbose: bool = typer.Option(False, help="Increase verbosity of BioTEA box logs"),
):
    """Prepare agilent expression data for analysis."""
    pour.prepare_agilent(
        input_dir=input_dir,
        output_file=output_file,
        grep_pattern=grep_pattern,
        version=version,
        log_name=log_name,
        remove_controls=remove_controls,
        plot_number=plot_number,
        plot_size=plot_size,
        use_png=use_png,
        verbose=verbose,
    )


@prepare.command(name="affymetrix")
def prepare_affymetrix_command(
    input_dir: Path = typer.Argument(
        ..., help="Path to the folder with the input files"
    ),
    output_file: Path = typer.Argument(..., help="Path to the output file"),
    version: str = typer.Option("latest", help="Specify BioTEA box container version"),
    log_name: Optional[str] = typer.Option(
        None, help="Specify BioTEA box log name for the run"
    ),
    remove_controls: bool = typer.Option(False, help="Remove control probes?"),
    plot_number: int = typer.Option(1e10, help="Maximum number of plots to show"),
    plot_size: str = typer.Option("12,5", help="Size of plots as 'width,height'"),
    use_png: bool = typer.Option(
        False, help="Generate .png files instead of pdf files (600 ppi resolution)."
    ),
    verbose: bool = typer.Option(False, help="Increase verbosity of BioTEA box logs"),
):
    """Prepare affymetrix expression data for analysis."""
    pour.prepare_affymetrix(
        input_dir=input_dir,
        output_file=output_file,
        version=version,
        log_name=log_name,
        remove_controls=remove_controls,
        plot_number=plot_number,
        plot_size=plot_size,
        use_png=use_png,
        verbose=verbose,
    )


@cli_root.command(name="analyze")
def run_biotea_box_analysis_command(
    options_path: Path = typer.Argument(..., help="Path to the options file"),
    output_dir: Path = typer.Argument(..., help="Path to the output directory"),
    input_file: Path = typer.Argument(..., help="Path to the input expression matrix"),
    version: str = typer.Option("latest", help="Specify BioTEA box container version"),
    log_name: Optional[str] = typer.Option(
        None, help="Specify BioTEA box log name for the run"
    ),
    verbose: bool = typer.Option(False, help="Increase verbosity of BioTEA box logs"),
):
    """Run Differential Gene Expression with BioTEA box."""
    pour.run_biotea_box_analysis(
        options_path=options_path,
        output_dir=output_dir,
        input_file=input_file,
        version=version,
        log_name=log_name,
        verbose=verbose,
    )

    log.info("BioTEA completed.")


@cli_root.command(name="initialize")
def init_with_options_command(
    path: Path = typer.Argument(
        ..., help="Path to the folder that has to be initialized"
    ),
    metadata: Optional[Path] = typer.Argument(
        None, help="Path to a metadata file to use to generate the config"
    ),
):
    """Generate an options file suitable for `biotea analize`

    If a metadata file is specified, it should be a metadata file like one that
    is produced by `biotea retrieve`, with a first column with sample names and
    a series of other columns with the sample variables.
    """
    make_path_valid(path, dir=True)
    if metadata:
        log.debug("Got a metadata path, so running interactively.")
        options = interactive_metadata_to_biotea_box_options(metadata)
        with (path / "bioTEA_run_options.yaml").open("w+") as outstream:
            yaml.dump(options, outstream, default_flow_style=False)
        return

    shutil.copy(
        pkg_resources.path(resources, "bioTEA_run_default_options.yaml"),
        path / "bioTEA_run_options.yaml",
    )
    log.info(f"Initialized options @ {path / 'bioTEA_run_options.yaml'}")


class ValidSpecies(str, Enum):
    human = "human"
    drosophila = "drosophila"
    mouse = "mouse"
    rat = "rat"
    bee = "apis"


@annotate.command(name="apply")
def annotate_file_command(
    target: Path = typer.Argument(
        ..., help="Path to the input expression matrix to annotate"
    ),
    output: Path = typer.Argument(..., help="Path to the annotated output path"),
    annotation_database: str = typer.Argument(
        "internal",
        help="Annotation database to use. Pass 'internal' to use the default human database. Otherwise, a path to the database file generated with `annotations generate`",
    ),
    version: str = typer.Option("latest", help="Specify BioTEA box container version"),
    log_name: Optional[str] = typer.Option(
        None, help="Specify BioTEA box log name for the run"
    ),
    verbose: bool = typer.Option(False, help="Increase verbosity of BioTEA box logs."),
):
    """Annotate some expression data or DEA output with annotation data."""
    pour.annotate_file(
        target=target,
        output=output,
        annotation_database=annotation_database,
        version=version,
        log_name=log_name,
        verbose=verbose,
    )


@annotate.command(name="generate", hidden=True)
def generate_annotations_command(
    target: Path = typer.Argument(
        ..., help="Path to the file where the annotations will be stored"
    ),
    organism: ValidSpecies = typer.Argument(
        ValidSpecies.human, help="Species to generate annotations for"
    ),
    verbose: bool = typer.Option(False, help="Increase verbosity of BioTEA box logs."),
):
    """Generate annotations to use with BioTEA box."""
    print("You got to a hidden command! This has not been implemented yet.")
