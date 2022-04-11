# This file, when sourced, must do the following:
# - Load necessary packages for the module
# - Handle the incoming command line argument(s)
# - Run the actual command.
#
# The environment is shaped as such:
# bioTEA/
#   src/
#       shared/
#           ...
#       modules/
#           ...
#   input/  < Mounted to the input folder
#   target/ < Mounted to the output folder
#   logs/   < Mounted to the logs folder
#
# This script has access to the following:
# > The tidyverse;
# > The `tools` library in src/shared/tools.R;
#
# The logging module is already sourced, so the log object is present in the
# environment.
#
# The command line options passed by the user (the wrapper), are in the
# `module.args` global option.

# Load the arguments -----------------------------------------------------------
args <- getOption("module.args")

# Test that the arguments are valid --------------------------------------------
# I expect these arguments, in order:
# expression_data_path output_path database_name
# Pass "NULL" or NULL to use the defaults. Setting NULL in the defaults
# signifies a required argument.

defaults = list(
    expression_data_path = NULL, output_path = NULL, database_name = NA
)

fun_args <- validate_arguments(args, defaults)

# Add the hardcoded arguments
fun_args$expression_data_path <- paste0(
    "/bioTEA/input/", fun_args$expression_data_path
)
fun_args$output_path <- paste0("/bioTEA/target/", fun_args$output_path)

# Load required libraries.
#module.packages <- c()
#graceful_load(module.packages)

# Load the functions for this module
#source("/bioTEA/modules/some_module/...")

# Run the module
# > Small fix for the
do.call("annotate_to_file", args = fun_args)
