# Contributing to BioTEA

Thank you for wanting to contribute to this project! I assume you have some programming knowledge so we will skip the basics.

To contribute, we follow the standard issue/pull request system that is allowed by GitHub. Make a fork of the repo and (optionally) open an issue that describes what you want to fix/edit/improve/change. The issue will be the hub for discussion for the change. If you implemented the change, open a pull request (if you never dd before, follow [this guide from GitHub](https://docs.github.com/en/get-started/exploring-projects-on-github/finding-ways-to-contribute-to-open-source-on-github) for more information on the process of forking and making pull requests).
We will review and merge your request if we find it reasonable an well-written.

## Contributing to the box
"The box" refers to the Biotea docker container. Its code lives in `src/box/`. The box is made from a base image that contains all required packages, the `biotea-base` image. The code is inserted from the base image in the container to allow the separate management of dependencies and code. In this way, the required R libraries are frozen, and automated tests (such as here on github) do not take three hours of compile time. 

The box code is divided in modules. Each module is called in a particular way by the entrypoint ([entrypoint.R](https://github.com/CMA-Lab/bioTEA/blob/9d8f4f9f67145cc98c913ccd997ace62295a8817/src/box/entrypoint.R)), so please read the entrypoint file first. A template for new modules is inside `src/box/modules/template/`, so you can start wih that if you want to make a new module.

## Contributing to the python wrapper
The Python code lives in `src/biotea/`. The code is a standard PyPA package. We use `bumpversion` and Black to manage the package version and the code style, respectively. I personally have pre-commit hooks setup to run the code through Black, but you may do it manually before you submit the code.

Please make docstring to functions, following the [Google standard for Python docstrings](https://github.com/google/styleguide/blob/gh-pages/pyguide.md#38-comments-and-docstrings).

Test if the package works with `pytest` before submitting a pull request. Tests live in the `tests/` folder. To run tests, you will need to rebuild the docker locally with the "bleeding" version tag. In the `./src` folder, run `docker build -t cmalabscience/biotea-box:bleeding .` to rebuild the docker with your local code. Then launch `pytest ./bioTea` to run tests.

The structure of the Python package is straightforward (I hope). Start reading [the `cli.py`](https://github.com/CMA-Lab/bioTEA/blob/9d8f4f9f67145cc98c913ccd997ace62295a8817/src/bioTea/bioTea/cli.py) file first. The exported command that is invoked by `biotea` is `cli_root()` (at the time of writing). Refer to the [`setup.cfg`](https://github.com/CMA-Lab/bioTEA/blob/9d8f4f9f67145cc98c913ccd997ace62295a8817/src/bioTea/setup.cfg) file for more info.

In short, the Cli registers commands. The core of the package is the docker wrapper, which interacts with the docker engine with an external package that gives an easy-to-use API for the daemon. I triple-check the parameters with so-called `BioTeaBoxInterface`s, virtual classes that can be used to type-check inputs before being passed to the box. The box also checks the inputs, but its checks are more fragile.

For questions or concerns, contact @mrhedmad.
