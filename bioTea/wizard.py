import importlib.resources as pkg_resources
import itertools
import logging
import shutil
import string
from collections import Counter
from enum import Enum
from itertools import permutations
from math import floor
from pathlib import Path
from typing import Optional, Union

import pandas as pd
import typer
import yaml
from colorama import Fore
from fuzzywuzzy import fuzz
from tqdm import tqdm

from bioTea import pour, resources
from bioTea.utils.errors import BioTeaError, ErrorManager, RaiserHandler
from bioTea.utils.path_checker import is_pathname_valid
from bioTea.utils.strings import WIZARD_LOGO
from bioTea.utils.tools import (
    Replacer,
    ask_choices,
    contains_numbers,
    recursive_dict_update,
    user_input,
)

log = logging.getLogger(__name__)


class ValidManufacturers(Enum):
    def __str__(self) -> str:
        return str(self.value)

    agilent = "Agilent Technologies"
    affymetrix = "Affymetrix"


def glob_manufacturer(manufacturer_string) -> ValidManufacturers:
    tiers = {}
    for value in ValidManufacturers.__members__.values():
        tiers[str(value)] = fuzz.ratio(str(value), manufacturer_string)
    best_match_score = max(tiers.values())
    best_key = max(tiers, key=tiers.get)
    if best_match_score < 60:
        log.warn(
            f"Unsure about the manufacturer. I think it is {best_key}, but at a confidence of only {best_match_score}. Is '{manufacturer_string}' supported?"
        )

    return best_key


def sanitize_design_string(values: list) -> list[str]:
    # Character-only
    def contains_illegal(x):
        return any([y in ", -" for y in str(x)])

    if any(
        [contains_numbers(x) for x in values] + [contains_illegal(x) for x in values]
    ):
        log.warn(
            "There are illegal characters in the design string. Getting rid of them by replacing the factors."
        )
        replacer = Replacer(list(string.ascii_uppercase))
        sanitized_values = replacer.sanitize(values)
        log.info(
            "Replaced values: {}".format(
                ", ".join([f"{old} > {new}" for old, new in replacer.matches.items()])
            )
        )
        return sanitized_values
    log.debug("No need to sanitize input design string.")
    return values


def standardize_pairings(values: list) -> list[str]:
    # Number only
    log.info("Standardizing parings.")
    replacer = Replacer(itertools.count(0, 1))
    sanitized_values = replacer.sanitize(values)

    log.info(
        "New pairings: {}".format(
            ", ".join([f"{old} > {new}" for old, new in replacer.matches.items()])
        )
    )
    return sanitized_values


def interactive_metadata_to_biotea_box_options(
    metadata: Union[Path, pd.DataFrame],
    primary_var: Optional[str] = None,
    use_all_contrasts: bool = False,
) -> dict:
    log.debug("Generating a BioTEA_box_options.yaml file.")
    log.warn(
        "Due to limitations in parsing yaml data, the helpful comments in the "
        + "options will be lost by generating the file in this way. "
        + "Please refer to the manual directly for help "
        + "if you need to edit the file manually later."
    )

    if type(metadata) != pd.DataFrame:
        metadata = pd.read_csv(metadata)

    if "Sample_id" not in metadata.columns:
        typer.echo("Metadata file does not have a 'Sample_id' column. Aborting.")
        raise typer.Abort()

    metadata = metadata.sort_values(by=["Sample_id"])
    samples = ", ".join(list(metadata["Sample_id"]))
    log.debug(f"Using the following sample order: {samples}")

    meta_vars = [x for x in metadata.columns if x not in ["Sample_id"]]

    if any(metadata.applymap(lambda x: x in ["na", "null", None])):
        log.warning(
            "Detected some possible NAs or Null values in the metadata. This could cause errors later. Perhaps check the metadata first?"
        )

    if primary_var is not None:
        if primary_var in meta_vars:
            log.warn(
                "The specified primary variable is not valid. Falling back to prompting the user."
            )
            primary_var = None

    while primary_var is None:
        primary_var = ask_choices("Select the test variable", meta_vars)

        if not typer.confirm(f"Selection: {primary_var}. Is this correct?"):
            primary_var = None

    parsed_design_var = sanitize_design_string(metadata[primary_var])

    contrasts = None
    if len(set(parsed_design_var)) == 1:
        log.error(
            f"There is only one level in the variable {primary_var}. Cannot proceed with the analysis."
        )

    all_contrasts = [f"{x}-{y}" for x, y in permutations(set(parsed_design_var), 2)]

    if use_all_contrasts:
        log.info(f"Using all contrasts: {all_contrasts}")
        contrasts = all_contrasts

    if contrasts is not None and len(meta_vars) == 2:
        base = ask_choices("Select the 'control' variable", set(parsed_design_var))
        test = [x for x in meta_vars if x != base][0]
        contrasts = [f"{test}-{base}"]

    while contrasts is None:
        typer.echo(
            f"Specify the contrasts of interest, as 'test-control test-control ...'.\nE.g. '{all_contrasts[0]}'"
        )
        contrasts = ask_choices(
            "Please make a selection:", all_contrasts, accept_list=True
        )

    # -- Pairings
    all_other_vars = [x for x in meta_vars if x != primary_var]
    pairings = None
    if len(all_other_vars) > 0:
        typer.echo(
            f"Are there sample pairings (I.E. different samples but taken from the same patient)? If so, specify the variable that describes the pairings. If there are none, leave this empty:"
        )
        pairing_var = ask_choices("Please make a selection:", all_other_vars + [""])
        if pairing_var == "":
            pairings = None
        else:
            pairings = standardize_pairings(list(metadata[pairing_var]))
            counter = Counter(pairings)
            if len((eccessive_pairs := [x for x, y in counter.items() if y > 2])) > 0:
                log.warn(
                    f"There are one or more pairing(s) ({len(eccessive_pairs)}) that occur more than twice: {eccessive_pairs}.\n Are you sure this is the correct pairing variable? Maybe there are technical replicates in the study?"
                )
                typer.confirm("Continue anyway?", abort=True)

    # -- Batch effect
    all_other_vars = [x for x in meta_vars if x not in [primary_var, pairing_var]]
    batch_var = None
    if len(all_other_vars) > 0:
        typer.echo(
            f"Is there a variable that specifies the analysis batch? Specify it now. If not, leave this empty."
        )
        batch_var = ask_choices("Please make a selection:", all_other_vars + [""])
        if batch_var == "":
            batch_var = None

    all_other_vars = [
        x for x in meta_vars if x not in [primary_var, batch_var, pairing_var]
    ]
    other_vars = None
    if len(all_other_vars) > 0:
        typer.echo(
            f"Are there any other variables to account for? Specify them here, in a comma-separated list (e.g. var1, var2, var3...):"
        )
        other_vars = ask_choices(
            "Please make a selection:", all_other_vars + [""], accept_list=True
        )
        if other_vars == [""]:
            other_vars = None

    typer.echo("Recap:")
    typer.echo(f"\tPrimary variable to analize: {primary_var}")
    typer.echo(f"\tContrasts: {', '.join(contrasts)}")
    if batch_var:
        typer.echo(f"\tVariable to correct batches on: {batch_var}")
    else:
        typer.echo("\tNo batches to correct for.")
    if other_vars:
        typer.echo(f"\tOther variables to correct for: {', '.join(other_vars)}")
    else:
        typer.echo(f"\tNo other variables to take into account.")

    typer.confirm("\nIs this correct?", abort=True)

    log.info("Updating defaults with user data.")
    defaults = yaml.safe_load(
        pkg_resources.open_text(resources, "bioTEA_run_default_options.yaml")
    )
    log.debug("Loaded defaults.")

    if pairings:
        parsed_design_var = [f"{x}{y}" for x, y in zip(parsed_design_var, pairings)]
    new_expr_design = ", ".join(parsed_design_var)
    if batch_var:
        new_batches_design = ",".join(sanitize_design_string(metadata[batch_var]))
    else:
        new_batches_design = None
    if other_vars:
        new_extra_vars = {
            var: ", ".join(sanitize_design_string(metadata[var])) for var in other_vars
        }
    else:
        new_extra_vars = None

    defaults["design"]["experimental_design"] = new_expr_design
    log.debug(f"New design string: {new_expr_design}")
    defaults["design"]["contrasts"] = contrasts
    log.debug(f"New contrasts: {contrasts}")
    defaults["design"]["batches"] = new_batches_design
    log.debug(f"New batch string: {new_batches_design}")
    defaults["design"]["extra_limma_vars"] = new_extra_vars
    log.debug(f"New extra limma vars: {new_extra_vars}")

    return defaults


def interactive_general_options():
    show_snip = typer.confirm("Show data snippets during the analysis?", True)
    annotation_database = typer.confirm(
        "Use the internal (human) annotation database?", True
    )
    if not annotation_database:
        log.info(
            Fore.GREEN
            + "If you need to use a different annotation database, consider using `biotea annotations` instead, or edit the analysis file manually."
            + Fore.RESET
        )

    save_png = typer.confirm("Save png plots instead of pdf plots?", False)
    if save_png:
        png_resolution = typer.prompt("Png resolution (in pixels per inch)", 300)
    else:
        png_resolution = 300
    plot_width = typer.prompt("Plot width (in inches)", 16)
    plot_height = typer.prompt("Plot height (in inches)", 9)

    enumerate_plots = typer.confirm("Enumerate plots while saving them?", True)

    dry = typer.confirm(
        "Run in dryrun mode (this runs the analysis, but saves no output)?", False
    )
    renorm = typer.confirm(
        "Renormalize input data before analyzing (useful if the input data is really not homogeneous even after preparation)?",
        False,
    )
    convert_counts = typer.confirm(
        "Is the input data a count matrix (from RNA-seq)? Should the data be made continuous with `voom`?",
        False,
    )
    while True:
        limma = typer.confirm("Run the limma analysis?", True)
        rp = typer.confirm("Run the rankproduct analysis?", True)
        if limma or rp:
            break
        log.warning("Please run at least one between limma and rankprod.")

    return {
        "general": {
            "show_data_snippets": show_snip,
            "annotation_database": annotation_database,
            "plots": {
                "save_png": save_png,
                "plot_width": plot_width,
                "plot_height": plot_height,
                "png_resolution": png_resolution,
                "enumerate_plots": enumerate_plots,
            },
        },
        "switches": {
            "dryrun": dry,
            "renormalize": renorm,
            "convert_counts": convert_counts,
            "limma": limma,
            "rankproduct": rp,
        },
    }


def wizard_unhandled():
    typer.echo(WIZARD_LOGO)

    typer.echo(
        "Welcome to the bioTEA wizard. This helper will guide you through the whole bioTEA analysis."
    )
    typer.echo("You can exit at any time by pressing CTRL+C")
    typer.echo(
        Fore.YELLOW
        + "WARNING: The wizard is still experimental, and may produce errors. Use with caution."
    )
    typer.echo(
        "Select a folder (that will be created if non-existant) to use as a working directory:"
    )
    staging_path = user_input(
        "> ",
        is_pathname_valid,
        retry_prompt="The path seems invalid. Try again.",
    )
    log.debug(f"Got for path: {staging_path}")

    staging_path, temp_path = pour.stage_new_analysis(staging_path)

    typer.echo("Please specify the GEO id of the data. It should start with 'GSE':")
    geo_id = user_input(
        "> ",
        lambda x: x.startswith("GSE"),
        retry_prompt="That seems wrong. GEO series should start with 'GSE...'. Try again.",
    )

    series = pour.retrieve_geo_data(staging_path, geo_id, temp_dir=temp_path)

    new_options = interactive_metadata_to_biotea_box_options(series.generate_metadata())

    new_options = recursive_dict_update(new_options, interactive_general_options())
    log.debug(f"Final input dict: {new_options}")

    typer.echo(
        "We can start the analysis. First, we prepare the expression data to an expression frame."
    )

    best_platform = glob_manufacturer(series.platform.manufacturer)
    is_platform_correct = typer.confirm(
        f"The manufacturer label is {series.platform.manufacturer}. I think it is {best_platform}. Am I correct?",
        True,
    )
    if not is_platform_correct:
        log.error(
            "Cannot proceed. Prepare the data manually, then run `biotea initialize` followed by `biotea analyze yourself."
        )
        raise typer.Abort

    remove_controls = typer.confirm("Remove control probes from the input data?", True)
    if typer.confirm(
        "Reduce the number of diagnostic plots (to 10% of all samples)?", True
    ):
        plot_number = floor(len(series.samples) * 0.1)
    else:
        plot_number = len(series.samples)
    log.debug(f"Plot number set to be {plot_number}")

    for sample in tqdm(series.samples, "Unpacking samples..."):
        shutil.unpack_archive(
            sample.suppl_data_local_path, temp_path / "unpacked_raw_samples"
        )

    expression_file_path = staging_path / "expression_matrix.csv"
    if best_platform == ValidManufacturers.affymetrix:
        log.info("Preparing affymetrix data.")

        pour.prepare_affymetrix(
            temp_path / "unpacked_raw_samples",
            expression_file_path,
            remove_controls=remove_controls,
            plot_number=plot_number,
            plot_size=f"{new_options['general']['plots']['plot_width']},{new_options['general']['plots']['plot_height']}",
            use_png=new_options["general"]["plots"]["save_png"],
        )
    elif best_platform == ValidManufacturers.agilent:
        log.info("Preparing agilent data.")

        # Some magic to get the prefix
        suffix = Path(series.samples[0].suppl_data_local_path).suffix
        log.debug(f"Suffix automatically detected to be '{suffix}'")
        pour.prepare_agilent(
            temp_path / "unpacked_raw_samples",
            expression_file_path,
            # The \ is there to escape the .
            grep_pattern=f"\{suffix}",
            remove_controls=remove_controls,
            plot_number=plot_number,
            plot_size=f"{new_options['general']['plots']['plot_width']},{new_options['general']['plots']['plot_height']}",
            use_png=new_options["general"]["plots"]["save_png"],
        )

    log.info(
        f"Done preparing the input data. Prepared data is @ {staging_path / 'expression_matrix.csv'}"
    )

    log.info("Running container.")
    log.debug("Writing options file.")
    options_path = staging_path / "wizard_box_options.yaml"
    with options_path.open("w+") as fileout:
        yaml.dump(new_options, fileout)

    pour.run_biotea_box_analysis(
        options_path=options_path,
        output_dir=staging_path / "analysis",
        input_file=expression_file_path,
    )

    typer.echo(
        f"The analysis ended. Take a look inside {staging_path} for the output. Temporary files will be cleaned automatically."
    )
    log.info("Wizard completed.")


def wizard(*args, **kwargs):
    # Without this, the error manager tries to give args to the wizard,
    # which causes it to crash.
    def _wrapper(*args, **kwargs):
        wizard_unhandled()

    wizard_manager = ErrorManager(_wrapper)

    wizard_manager.add_handler(BioTeaError, RaiserHandler())

    wizard_manager.run(args, kwargs)
