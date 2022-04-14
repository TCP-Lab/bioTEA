import importlib.resources as pkg_resources
import itertools
import logging
import string
from collections import Counter
from enum import Enum
from itertools import permutations
from pathlib import Path
from typing import Optional, Union

import pandas as pd
import typer
import yaml
from colorama import Fore
from fuzzywuzzy import fuzz

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
    agilent = "agilent"
    affymetrix = "affymetrix"


def glob_manufacturer(manufacturer_string) -> ValidManufacturers:
    tiers = {}
    for value in ValidManufacturers.__members__.values():
        tiers[manufacturer_string] = fuzz.ratio(value, manufacturer_string)
    best_match = max(tiers)
    best_key = max(tiers, key=tiers.get)
    if best_match < 60:
        log.warn(
            f"Unsure about the manufacturer. I think it is {best_key}, but at a confidece of only {best_match}. Is '{manufacturer_string}' supported?"
        )


def sanitize_design_string(values: list) -> list[str]:
    # Character-only
    if any([contains_numbers(x) for x in values]):
        log.warn(
            "There are numbers in the design string. Getting rid of them by replacing the factors."
        )
        replacer = Replacer(list(string.ascii_uppercase))
        sanitized_values = replacer.sanitize(values)
        log.info(
            "Replaced values: {}".format(
                ", ".join(
                    [f"{old} > {new}" for old, new in zip(values, sanitized_values)]
                )
            )
        )
        return sanitized_values
    log.debug("No nood to sanitize input design string.")
    return values


def standardize_pairings(values: list) -> list[str]:
    # Number only
    log.info("Standardizing parings.")
    replacer = Replacer(itertools.count(0, 1))
    sanitized_values = replacer.sanitize(values)

    log.info(
        "New pairings: {}".format(
            ", ".join([f"{old} > {new}" for old, new in zip(values, sanitized_values)])
        )
    )
    return sanitized_values


def interactive_metadata_to_biotea_box_options(
    metadata: Union[Path, pd.DataFrame],
    primary_var: Optional[str] = None,
    use_all_contrasts: bool = False,
) -> dict:
    log.debug("Generating a GATTACA_options.yaml file.")
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
        typer.echo(f"\tOther variables to correct for: {''.join(other_vars)}")
    else:
        typer.echo(f"\tNo other variables to take into account.")

    typer.confirm("\nIs this correct?", abort=True)

    log.info("Updating defaults with user data.")
    defaults = yaml.safe_load(
        pkg_resources.open_text(resources, "GATTACA_default_options.yaml")
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
        new_extra_vars = [
            ", ".join(sanitize_design_string(metadata[var])) for var in other_vars
        ]
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
            + "If you need to use a different annotation database, consider unsing `biotea annotations` instead, or edit the analysis file manually."
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
        "Select a folder (that will be created if non-existant) to use as a working directory:"
    )
    staging_path = user_input(
        "> ",
        is_pathname_valid,
        retry_prompt="The path seems invalid. Try again.",
    )

    staging_path, temp_path = pour.stage_new_analysis(staging_path)

    typer.echo("Please specify the GEO id of the data. It should start with 'GSE':")
    geo_id = user_input(
        "> ",
        lambda x: x.startswith("GSE"),
        retry_prompt="That seems wrong. GEO series should start with 'GSE...'. Try again.",
    )

    series = pour.retrieve_geo_data(staging_path, geo_id)

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
            "Cannot proceed. Prepare the data manually, then run `biotea initialize` followed by `biotea analise yourself."
        )
        raise typer.Abort

    future_commands = []

    if best_platform == ValidManufacturers.affymetrix:
        future_commands.append("biotea prepare affymetrix ")


def wizard(*args, **kwargs):
    wizard_manager = ErrorManager(wizard_unhandled)

    wizard_manager.add_handler(BioTeaError, RaiserHandler())

    wizard_manager.run(args, kwargs)
