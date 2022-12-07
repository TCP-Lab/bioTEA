from __future__ import annotations

import json
import logging
import os
from abc import ABC
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from numbers import Number
from pathlib import Path
from typing import Any, Callable

import docker
import docker.errors
import requests
import typer
from docker.types import Mount
from packaging.version import LegacyVersion, parse
from typer import Abort

from bioTea import __version__
from bioTea.utils.errors import ContainerExitError, ImageNotFoundError
from bioTea.utils.path_checker import is_path_exists_or_creatable_portable
from bioTea.utils.tools import ConsoleWindow

log = logging.getLogger(__name__)

NAMESPACE = "cmalabscience"
LEAF = "biotea-box"
REPO = f"{NAMESPACE}/{LEAF}"

POSSIBLE_LOG_LEVELS = ("info", "debug", "error", "warning", "disable")


@dataclass
class BioTeaBoxVersion:
    raw_version: str

    @property
    def realversion(self):
        return parse(self.raw_version)

    def __lt__(self, other: BioTeaBoxVersion) -> bool:
        if type(other) is str:
            other = BioTeaBoxVersion(other)

        if type(other) is not BioTeaBoxVersion:
            raise TypeError(f"Cannot compare 'BioTeaBoxVersion' and '{type(other)}'")
        return self.realversion < other.realversion

    def __eq__(self, other: object) -> bool:
        if type(other) is str:
            other = BioTeaBoxVersion(other)

        if type(other) is not BioTeaBoxVersion:
            raise TypeError(f"Cannot compare 'BioTeaBoxVersion' and '{type(other)}'")
        return self.realversion == other.realversion

    def __str__(self) -> str:
        return self.raw_version


def get_installed_versions(
    client: docker.DockerClient = docker.from_env(),
) -> list[BioTeaBoxVersion]:
    local_images = client.images.list(REPO)
    local_images = [
        local_image.tags[0][(len(REPO) + 1) :] for local_image in local_images
    ]
    return [BioTeaBoxVersion(version) for version in local_images]


def pull_biotea_box_version(
    version: BioTeaBoxVersion, client: docker.DockerClient = docker.from_env()
):
    log.debug(f"Looking to see if {version} is available...")
    if not version in get_all_versions():
        log.error(f"Version {version} not found remotely.")
        raise ImageNotFoundError(str(version))

    log.info(f"Pulling remote image {version}...")
    client.images.pull(f"{REPO}:{version.raw_version}")
    log.info("Pulled image.")


def delete_biotea_box_version(
    version: BioTeaBoxVersion, client: docker.DockerClient = docker.from_env()
):
    if not version in get_installed_versions():
        log.error(f"Version {version} not found locally.")
        raise ImageNotFoundError(str(version))

    log.info(f"Removing image {version}...")
    client.images.remove(image=f"{REPO}:{version}")
    log.debug(f"Done removing image.")

    return True


def get_all_versions() -> list[BioTeaBoxVersion]:
    endpoint = f"https://registry.hub.docker.com/v2/namespaces/{NAMESPACE}/repositories/{LEAF}/tags"
    try:
        res = requests.get(endpoint, timeout=20)
    except Exception as e:
        log.exception("Failed to retrieve the biotea-box repo information", e)
        raise Abort

    images = json.loads(res.text)["results"]

    versions = []
    for image in images:
        version = BioTeaBoxVersion(image["name"])
        if type(version.realversion) is LegacyVersion:
            log.debug(f"Version '{version}' discarded as it is a Legacy version.")
            continue
        versions.append(version)

    if not versions:
        log.warn("No valid remote versions have been found.")

    return versions


def get_latest_version() -> BioTeaBoxVersion:
    all_vers = get_all_versions()
    if all_vers:
        return sorted(all_vers)[-1]
    raise ImageNotFoundError("No valid 'latest' version found.")


def is_version_compatible(version: BioTeaBoxVersion) -> bool:
    """Test if the input version is compatible with the current bioTEA version.

    Assumes compatibility for the future (bleeding) as well as all versions with
    identical major and minor versions.

    Args:
        version (BioTeaBoxVersion): The version to check for compatibility.

    Returns:
        bool: Whether the version is compatible or not.
    """

    if version == "bleeding":
        return True

    biotea_parsed = parse(__version__)

    log.debug(
        f"Checking for version compatibility."
        f"BioTEA version: {biotea_parsed}. Inputted version: {version.realversion}"
    )

    return (
        version.realversion.major == biotea_parsed.major
        and biotea_parsed.minor == version.realversion.minor
    )


# Some checks
def na_or(check) -> Callable:
    def _wrapped_check(argument):
        if argument is None:
            return True
        if type(argument) == str and argument.upper() in ("NA"):
            return True
        else:
            return check(argument)

    return _wrapped_check


def is_(argtype) -> Callable:
    def _wrapped_check(argument):
        return issubclass(type(argument), argtype)

    return _wrapped_check


def is_in(arglist) -> Callable:
    def _wrapped_check(argument):
        return argument in arglist

    return _wrapped_check


def is_valid_design_string(argument):
    # TODO: implement this check
    return is_(str)(argument)


def is_valid_color(argument):
    # TODO: implement this
    return is_(str)(argument)


def is_list_of(check) -> Callable:
    def _wrapped_check(argument):
        if not type(argument) == list:
            return False

        return all([check(x) for x in argument])

    return _wrapped_check


def _or_(x: Callable, y: Callable):
    def _wrapped(argument):
        return x(argument) or y(argument)

    return _wrapped


class BioTeaBoxArgument:
    """Class used to mark an argument in a BioTeaBoxInterface dict."""

    def __init__(self, check: Callable, default: Any) -> None:
        self.check = check
        self.default = default

        assert self.check(
            default
        ), "Invalid default BioTeaBox value. Someone coded it wrong."

    def __call__(self, argument=None):
        if argument == None:
            argument == self.default

        if not self.check(argument):
            raise ValueError(f"Argument check failed. Invalid argument {argument}")


class RequiredBioTeaBoxArgument(BioTeaBoxArgument):
    def __init__(self, check: Callable) -> None:
        self.check = check
        # The default does not matter. It HAS to be overridden.
        self.default = None

    def __call__(self, argument):
        if not self.check(argument):
            raise ValueError(f"Argument check failed. Invalid argument {argument}")


class BioTeaBoxInterface(ABC):
    """Abstract class that models an interface with BioTeaBox.

    Defines the arguments that can be passed to a BioTeaBox command, and handles
    parsing them to an object that can be given to `run_biotea_box` to run the
    concrete command.
    """

    possible_args: dict = None
    """The possible arguments to the interface.

    This is a dictionary of "value_name" = BioTeaBoxArgument.
    If a Callable, it is used to test the arg before passing it to BioTeaBox.
    If a RequiredArgument,
    """

    # I am not 100% sure this is the correct way to use this, but it fails
    # if "possible_args" is not defined, so I'm happy.
    def __init_subclass__(cls, /, **kwargs):
        super().__init_subclass__(**kwargs)
        if cls.possible_args is None:
            raise TypeError("Must specify `possible_args`.")

    @classmethod
    def parse_arguments(self, **kwargs) -> str:
        """Check the input args and parse them to a BioTeaBox-compliant string."""
        required_args = [
            key
            for key, val in self.possible_args.items()
            if type(val) is RequiredBioTeaBoxArgument
        ]

        assert all(
            [x in kwargs.keys() for x in required_args]
        ), "Missing required args: {}".format(
            ", ".join([x for x in required_args if x not in kwargs.keys()])
        )
        assert all(
            [x in self.possible_args.keys() for x in kwargs.keys()]
        ), "Unrecognized argument(s) {}".format(
            ", ".join([x for x in kwargs.keys() if x not in self.possible_args.keys()])
        )

        for key, value in kwargs.items():
            # These will raise an error if the check fails. So, if this passes,
            # all passed args are OK.
            try:
                self.possible_args[key](value)
            except ValueError:
                # Re-raise with more context
                raise ValueError(f"Argument check failed for key {key}: {value}")

        # Here, we are sure of three things:
        # 1. All required arguments are overridden by `kwargs`
        # 2. All arguments in `kwargs` are in the possible_arguments.
        # 3. All arguments in `kwargs` are valid (they pass the checks)
        # Therefore, we can update the default values with the kwargs safely.
        defaults = {
            key: self.possible_args[key].default for key in self.possible_args.keys()
        }
        defaults.update(kwargs)

        # BioTeaBox wants a JSON-encoded string.
        return f"'{json.dumps(defaults, indent=None)}'"


## >> Interface definitions
# To define a new interface, or check that one is compilant, look in the
# `entrypoint.R` of the BioTeaBox module, at the `defaults` list.
# Convert this list to a dictionary, replacing every `NULL` with RequiredArgument


class PrepAffyInterface(BioTeaBoxInterface):
    possible_args: dict = {
        "output.file": RequiredBioTeaBoxArgument(is_path_exists_or_creatable_portable),
        "remove.controls": BioTeaBoxArgument(is_(bool), True),
        "n_plots": BioTeaBoxArgument(is_(int), 1_000_000),
        # Plot options
        "use_pdf": BioTeaBoxArgument(is_(bool), True),
        "plot_width": BioTeaBoxArgument(is_(int), 16),
        "plot_height": BioTeaBoxArgument(is_(int), 9),
        "png_ppi": BioTeaBoxArgument(is_(int), 250),
        "enumerate_plots": BioTeaBoxArgument(is_(bool), True),
    }


class PrepAgilInterface(BioTeaBoxInterface):
    possible_args: dict = {
        "output_file": RequiredBioTeaBoxArgument(is_path_exists_or_creatable_portable),
        "remove_controls": BioTeaBoxArgument(is_(bool), True),
        "n_plots": BioTeaBoxArgument(is_(int), 1_000_000),
        "grep_pattern": BioTeaBoxArgument(is_(str), "*.(txt|TXT)"),
        # Plot options
        "use_pdf": BioTeaBoxArgument(is_(bool), True),
        "plot_width": BioTeaBoxArgument(is_(int), 16),
        "plot_height": BioTeaBoxArgument(is_(int), 9),
        "png_ppi": BioTeaBoxArgument(is_(int), 250),
        "enumerate_plots": BioTeaBoxArgument(is_(bool), True),
    }


class AnalyzeInterface(BioTeaBoxInterface):
    possible_args: dict = {
        "input.file": RequiredBioTeaBoxArgument(is_path_exists_or_creatable_portable),
        "experimental_design": RequiredBioTeaBoxArgument(is_valid_design_string),
        "contrasts": RequiredBioTeaBoxArgument(is_list_of(is_(str))),
        "min_log2_expression": BioTeaBoxArgument(is_(Number), 4.0),
        "fc_threshold": BioTeaBoxArgument(is_(Number), 0.5),
        "min_groupwise_presence": BioTeaBoxArgument(is_(Number), 0.8),
        "show_data_snippets": BioTeaBoxArgument(is_(bool), True),
        "annotation_database": BioTeaBoxArgument(is_(bool), True),
        "dryrun": BioTeaBoxArgument(is_(bool), False),
        "renormalize": BioTeaBoxArgument(is_(bool), False),
        "convert_counts": BioTeaBoxArgument(is_(bool), False),
        "run_limma_analysis": BioTeaBoxArgument(is_(bool), True),
        "run_rankprod_analysis": BioTeaBoxArgument(is_(bool), True),
        "batches": BioTeaBoxArgument(na_or(is_valid_design_string), "NA"),
        "extra_limma_vars": BioTeaBoxArgument(
            na_or(is_list_of(is_valid_design_string)), "NA"
        ),
        "group_colors": BioTeaBoxArgument(
            is_list_of(is_valid_color),
            [
                "cornflowerblue",
                "firebrick3",
                "olivedrab3",
                "darkgoldenrod1",
                "purple",
                "magenta3",
            ],
        ),
        # Plot options
        "use_pdf": BioTeaBoxArgument(is_(bool), True),
        "plot_width": BioTeaBoxArgument(is_(int), 16),
        "plot_height": BioTeaBoxArgument(is_(int), 9),
        "png_ppi": BioTeaBoxArgument(is_(int), 250),
        "enumerate_plots": BioTeaBoxArgument(is_(bool), True),
    }


class AnnotateInterface(BioTeaBoxInterface):
    possible_args: dict = {
        "expression_data_path": RequiredBioTeaBoxArgument(
            is_path_exists_or_creatable_portable
        ),
        "output_path": RequiredBioTeaBoxArgument(is_path_exists_or_creatable_portable),
        "database_name": BioTeaBoxArgument(_or_(is_(str), is_(bool)), "internal"),
    }


## <<


def run_biotea_box(
    command: str,
    arguments: dict[str],
    interface: BioTeaBoxInterface,
    input_anchor: Path,
    output_anchor: Path,
    log_anchor: Path,
    version: str = "latest",
    console_level: str = "info",
    logfile_level: str = "debug",
    log_name: str = "auto",
) -> int:
    assert logfile_level in POSSIBLE_LOG_LEVELS
    assert console_level in POSSIBLE_LOG_LEVELS

    client = docker.from_env()

    if version == "latest":
        version = get_latest_version()
        log.info(f"Latest version: {version}")

    version = BioTeaBoxVersion(version)

    if version not in get_installed_versions(client=client):
        pull_biotea_box_version(version, client=client)

    if not is_version_compatible(version):
        log.warn(
            f"Selected an incompatible version '{version}'. BioTEA might not work."
        )
        typer.confirm("Continue anyway?", abort=True)

    for path in [input_anchor, output_anchor, log_anchor]:
        assert is_path_exists_or_creatable_portable(
            path
        ), f"Path {path} is inaccessible."
        assert not path.is_file(), f"Path {path} points to a file, not a folder."

        if not path.exists():
            log.debug(f"Making anchor point: {path}")
            os.makedirs(path)

    try:
        image = client.images.get(f"{REPO}:{version}")
    except docker.errors.ImageNotFound:
        log.error(f"Cannot find local image: {version}")
        return 0

    if log_name == "auto":
        now = datetime.today().strftime("%Y-%m-%d-%H:%M:%S")
        log_name = f"bioTEA_{now}"

    log.debug(f"Parsing arguments with interface '{type(interface)}'")
    try:
        parsed_args = interface.parse_arguments(**arguments)
    except ValueError:
        log.exception(
            "Invalid arguments passed to interface. Please open an issue with the bioTEA logs."
        )
        raise

    log.info(f"Launching container with version {version}")

    try:
        # The composed command is like such:
        # UUID GUID command logname loglevel_console loglevel_file (args)
        composed_command = (
            str(os.getuid())
            + " "
            + str(os.getgid())
            + " "
            + command
            + " "
            + log_name
            + " "
            + console_level
            + " "
            + logfile_level
            + " "
            + parsed_args
        )
        log.info(f"Composed command: {composed_command}")
        container = client.containers.run(
            image,
            command=composed_command,
            stderr=True,
            detach=True,
            mounts=[
                # Needs the `str` or the internal serializer dies
                Mount("/bioTEA/target", str(output_anchor.absolute()), type="bind"),
                Mount(
                    "/bioTEA/input",
                    str(input_anchor.absolute()),
                    type="bind",
                    read_only=True,
                ),
                Mount("/bioTEA/logs", str(log_anchor.absolute()), type="bind"),
            ],
        )
    except Exception as e:
        log.error(f"Launching container failed: {e}")
        raise e

    try:
        with ConsoleWindow(10, name="BioTeaBox container", line_prefix="> ") as window:
            for line in container.logs(stream=True):
                window.print(line.decode().rstrip())
    except KeyboardInterrupt:
        log.warn("Got shutdown signal. Killing container.")
        container.kill()
        container.remove()
        raise KeyboardInterrupt

    try:
        # Sometimes the container does not stop immediately.
        # This assures it does, even on errors.
        container.stop()
    except Exception:
        pass

    # Reload the container status
    container.reload()
    statuscode = container.wait()["StatusCode"]
    log.debug(f"Container exited with status {container.status} [{statuscode}]")
    log.debug(f"Removing container...")
    container_logs = container.logs().decode()
    container.remove()
    if statuscode != 0:
        container_error = "\n".join(container_logs.split("\n")[-5:])
        raise ContainerExitError(
            f"Container had a non-zero exit status: {statuscode}. Last five lines of container output: {container_error}"
        )

    return 0


class SpecialCommand(Enum):
    versions = "versions"
    test = "test"


def run_special_biotea_command(command: SpecialCommand, version: BioTeaBoxVersion):
    """Runs a special bioTEA box command that does not require args.

    This means we also do not need an interface, or mounts, or anything special.
    """
    client = docker.from_env()

    if version == "latest":
        version = get_latest_version()
        log.info(f"Latest version: {version}")

    if version not in get_installed_versions(client=client):
        pull_biotea_box_version(version, client=client)

    if not is_version_compatible(version):
        log.warn(
            f"Selected an incompatible version '{version}'. BioTEA might not work."
        )
        typer.confirm("Continue anyway?", abort=True)

    try:
        image = client.images.get(f"{REPO}:{version}")
    except docker.errors.ImageNotFound:
        log.error(f"Cannot find local image: {version}")
        return 0

    log.info(
        f"Running special command '{command.value}' in container version '{version}'."
    )

    composed_command = str(os.getuid()) + " " + str(os.getgid()) + " " + command.value

    bin_logs = client.containers.run(image, command=composed_command, stderr=True)

    print(bin_logs.decode("UTF-8"))

    return 0
