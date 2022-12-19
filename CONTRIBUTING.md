# Contributing to BioTEA

Thank you for wanting to contribute to this project! I assume you have some programming knowledge so we will skip the basics.

To contribute, we follow the standard issue/pull request system that is favoured by GitHub.
Make a fork of the repo and (optionally) open an issue that describes what you want to fix/edit/improve/change.
The issue will be the hub for discussion for the change.
If you implemented the change, open a pull request (if you don't know how to, follow [this guide from GitHub](https://docs.github.com/en/get-started/exploring-projects-on-github/finding-ways-to-contribute-to-open-source-on-github) for more information on the process of forking and making pull requests).
We will review and merge your request if we find it reasonable an well-written.

## Contributing BioTEA
The code is a standard PyPA package.
Install the required dev packages with `pip install -r requirements-dev.txt`.
We use `bumpversion` and Black to manage the package version and the code style, respectively.
I personally have pre-commit hooks setup to run the code through Black, but you may do it manually before you submit the code.
If you want to use pre-commit too, use `pre-commit install` before you commit.

Please write docstring to functions, following the [Google standard for Python docstrings](https://github.com/google/styleguide/blob/gh-pages/pyguide.md#38-comments-and-docstrings).

### Testing
Test if the package works with `pytest` before submitting a pull request.
Tests live in the `tests/` folder.
To run tests, you will need to rebuild the docker locally with the "bleeding" version tag.
Checkout the Box's repository ([https://github.com/CMA-Lab/bioTEA-box]), run `docker build -t cmalabscience/biotea-box:bleeding ./<path_to_the_box_repo>` to rebuild the docker with your local code.
Then launch `pytest ./bioTea` to run tests.

I'd be really grateful if you write more tests for the package. Please remember to always test the bleeding containers, not the latest ones.

The structure of the Python package is straightforward (I hope).
I'd suggest reading the `cli.py` file first, and following the imports around if you want to familiarize yourself with the code.
The exported command that is invoked by `biotea` is `cli_root()` (at the time of writing).
Refer to the `setup.cfg` file for more info.

In short, the Cli registers commands.
The core of the package is the docker wrapper, which interacts with the docker engine with an external package that gives an easy-to-use API for the daemon.
I triple-check the parameters with so-called `BioTeaBoxInterface`s, classes that can be used to type-check inputs before being passed to the box.
The box also checks the inputs, but its checks are more fragile.

For questions or concerns, contact @mrhedmad.

## Contributing to the box
"The box" refers to the Biotea docker container.
Its code lives in its own repository: [https://github.com/CMA-Lab/bioTEA-box].
Please look at that repository for more information.
