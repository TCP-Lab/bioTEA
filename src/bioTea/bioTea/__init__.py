"""bioTEA - Transcript Expression Analysis

A pipeline for processing transcription data to get differential gene expression.
"""
import importlib.resources as pkg_resources
import logging
import os
from copy import copy
from logging import StreamHandler
from logging.handlers import RotatingFileHandler
from pathlib import Path

import yaml
from colorama import Back, Fore, Style, init

from bioTea import resources

init(autoreset=True)

__version__ = "1.0.0"
# Set what is exported by the __init__
__all__ = ["__version__", "OPTIONS"]

# Parse local options for the tool.
DEFAULT_OPTIONS = yaml.safe_load(
    pkg_resources.open_text(resources, "default_options.yml")
)


def parse_local_options(*args):
    updated_defaults = copy(DEFAULT_OPTIONS)
    for dict in args:
        invalid_keys = [new_key not in DEFAULT_OPTIONS.keys() for new_key in dict]
        if invalid_keys:
            raise ValueError(
                "Invalid local option(s) {}.".format(", ".join(invalid_keys))
            )
        updated_defaults.update(args)

    return updated_defaults


_possible_option_paths = [
    Path("~/.bioTEA/config.yaml"),
    Path("~/.bioTEA/config.yml"),
    Path("~/.config/bioTEA/config.yaml"),
    Path("~/.config/bioTEA/config.yaml"),
]
_all_local_opts = []
for path in _possible_option_paths:
    if not path.exists():
        continue
    with path.open("r") as file:
        _all_local_opts.append(yaml.safe_load(file))
OPTIONS = parse_local_options(*_all_local_opts)


class ColorFormatter(logging.Formatter):
    # Change this dictionary to suit your coloring needs!
    COLORS = {
        "WARNING": Fore.YELLOW,
        "ERROR": Fore.RED,
        "DEBUG": Style.BRIGHT + Fore.MAGENTA,
        "INFO": Fore.GREEN,
        "CRITICAL": Style.BRIGHT + Fore.RED,
    }

    def format(self, record):
        reset = Fore.RESET + Back.RESET + Style.NORMAL
        color = self.COLORS.get(record.levelname, "")
        if color:
            record.name = Style.BRIGHT + Fore.BLACK + record.name + reset
            if record.levelname != "INFO":
                record.msg = color + record.msg + reset
            record.levelname = color + record.levelname + reset
        return logging.Formatter.format(self, record)


# Setup logging
log = logging.getLogger("bioTea")  # Keep this at the module level name
log.setLevel(logging.DEBUG)
log.propagate = False
# Keep this at DEBUG - set levels in handlers themselves

format = "%(asctime)s [%(levelname)s] %(name)s: %(message)s"
file_formatter = logging.Formatter(format)
console_formatter = ColorFormatter(format)

_LOG_PATH = (Path(OPTIONS["log_folder"]) / "bioTEA.log").expanduser().resolve()

if not _LOG_PATH.parent.exists():
    os.makedirs(_LOG_PATH.parent)

file_h = RotatingFileHandler(
    filename=Path(_LOG_PATH),
    encoding="utf-8",
    mode="a+",
    maxBytes=1e5,
    backupCount=5,
)
file_h.setFormatter(file_formatter)
file_h.setLevel(logging.DEBUG)
stream_h = StreamHandler()
stream_h.setFormatter(console_formatter)
stream_h.setLevel(logging.DEBUG)

log.addHandler(file_h)
log.addHandler(stream_h)
