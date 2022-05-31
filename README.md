[![Release](https://img.shields.io/github/v/release/CMA-Lab/bioTEA?style=flat-square)](https://github.com/CMA-Lab/bioTEA/releases)
[![PyPi](https://img.shields.io/pypi/v/biotea?style=flat-square)](https://pypi.org/project/bioTEA/)
[![Tests](https://img.shields.io/github/workflow/status/CMA-Lab/bioTEA/Tests?label=Tests&style=flat-square)](https://github.com/CMA-Lab/bioTEA/blob/main/CONTRIBUTING.md)
[![PyVersions](https://img.shields.io/pypi/pyversions/biotea?style=flat-square)](https://www.python.org/)

# BioTEA
BioTEA, where Tea is short for Transcript Enrichment Analysis, is a pipeline for Differential Gene expression Analysis with microarray and RNA-seq data.
It can download, preprocess and perform DEAs quickly, easily and in a reproducible way from the command line.

**Read the publication:**
> [Visentin, L.; Munaron, L.; Ruffinatti, F.A. BioTEA: containerized methods of analysis for microarray-based transcriptomics data. Preprints 2022, 1, 0.](https://doi.org/10.1101/2022.05.26.493502)

## Installation

> **IMPORTANT**: BioTEA works on UNIX systems. To run on Windows, use the [Windows Subsystems for Linux service](https://docs.microsoft.com/en-us/windows/wsl/install).

1. Install **Docker**. The exact process is specific to your package manager:
   - For Ubuntu, [follow this guide from the official Docker documentation](https://docs.docker.com/engine/install/ubuntu/).
   - For MacOS, [follow this guide from the official Docker documentation](https://docs.docker.com/desktop/mac/install/).
   - For Arch Linux, install with:
     ```zsh
     pacman -Syu docker
     systemctl enable --now docker.service
     ```
     You may need administrator privileges.
   - For other distros, check your distro package manager documentation.
2. Install **Python** version 3.9 or over. Again, this is dependent on your package manager:
   - On Ubuntu, run `apt update && apt install python`.
   - On Arch linux, run `pacman -Syu python`.
   - On MacOS, [follow this guide in the python docs](https://docs.python-guide.org/starting/install3/osx/).
3. **Optional** but strongly reccomended: Make a Python virtual environment to use bioTEA in. You can search online for a way to do this in your OS.
4. Install bioTEA with `pip`: `pip install biotea`.

If installed correctly, `biotea info` should give some information on the tool.

## Usage
The publication provides an overview of the tool and its usage. It is a good place to start. For more information on the various commands, read [the wiki](https://github.com/CMA-Lab/bioTEA/wiki).

If you run into problems, read [the FAQ page on the wiki](https://github.com/CMA-Lab/bioTEA/wiki/Frequently-Asked-Questions). If you still cannot solve the issue, [file a bug report](https://github.com/CMA-Lab/bioTEA/issues/new?assignees=&labels=bug&template=bug_report.md&title=%5BBUG%5D+), detailing as much as you can your problem, including the versions of bioTEA, Python interpreter, Docker engine and your OS.

## Contributing
To learn how you can contribute to the tool, [read the CONTRIBUTING guide](https://github.com/CMA-Lab/bioTEA/blob/main/CONTRIBUTING.md).
