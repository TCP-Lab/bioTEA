#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

# Args here are in this order:
# > UUID GUID command logname loglevel_console loglevel_file ...
# The ... are the args to pass on the command/module.

# Restore UUIDs and GUIDs on exit
# TODO: Test if this is possible to do with the docker run -u option,
# and an internal user.
.restore_uids <- function(UUID, GUID) {
    cat(paste0("Restoring files to UUID/GUID ", UUID, ":", GUID, "\n"))

    restorer <- function(...) {
        registered <- getOption("OWNERSHIP_REGISTER", default = c())

        # This can fail twice, hence the double call.
        if (length(registered) == 0) {
            cat("No files to restore permissions to.")
            return()
        }

        registered <- registered[file.exists(registered)]

        if (length(registered) == 0) {
            cat("No files to restore permissions to.")
            return()
        }

        reg_files <- registered[file_test("-f", registered)]
        reg_folders <- registered[file_test("-d", registered)]

        if (length(reg_files) > 0) {
            exit_files <- system2(
                "chown",
                args = c(paste0(UUID, ":", GUID), paste0("'", reg_files, "'"))
            )
        } else {
            exit_files <- 0
        }

        if (length(reg_folders) > 0) {
            exit_folders <- system2(
                "chown",
                args = c("-R", paste0(UUID, ":", GUID), paste0("'", reg_folders, "'"))
            )
        } else {
            exit_folders <- 0
        }

        if (exit_files == 0 & exit_folders == 0) {
            cat(paste0("Restored permissions to ", length(reg_files), " file(s) and ", length(reg_folders), " folder(s).\n"))
        } else {
            cat(paste0("Could not restore file permissions. Exit code files:", exit_files, " folders: ", exit_folders))
        }
    }
    return(restorer)
}


invisible(
    reg.finalizer(
        environment(), .restore_uids(args[1], args[2]), onexit = TRUE
    )
)


args <- args[-c(1, 2)]

COMMAND <- args[1]

if (COMMAND == "test") {
    # This is a testrun.
    # This means a few things:
    #  - The usual paths are not mounted (e.g. /bioTEA/target...)
    #  - All logging has to be redirected to stdout (e.g., just CAT, and not to file.)
    #  - No initial packages are loaded by the tests themselves. Use the
    #  setup/teardown files to load things instead.
    library(testthat)

    cat("Starting shared tests...\n")
    test_file("/bioTEA/shared/tests/test-shared.R")

    cat("Starting module tests...\n")
    modules <- c("prepagil", "prepaffy", "annotation", "analize")
    # Test all modules
    for (path in paste0("/bioTEA/modules/", modules, "/tests/test-module.R")) {
        test_file(path)
    }

    cat("Done executing tests.\n")
    quit(save = "no", status = 0)
} else if (COMMAND == "versions") {
    # Spit out the versions
    vers <- as.data.frame(installed.packages())[, 'Version', drop = FALSE]
    cat("R package versions:\n")
    print(vers)
    quit(save = "no", status = 0)
}


args <- args[-1]

# Args here are in this order:
# > logname loglevel_console loglevel_file ...
# The ... are the args to pass on the command/module.

# Load the base packages
suppressPackageStartupMessages({
    library(tidyverse)
    library(progress)
})

source("/bioTEA/shared/tools.R")

if (length(args) < 4) {
    stop(paste0("Not enough arguments: ", length(args)))
}

# Setup logging facilities
log$set_name(args[1])
log$set_console_level(args[2])
log$set_file_level(args[3])

log$debug("Logfile initialized as '", args[1], "'")

log$info("bioTEA container version: ", readLines("/bioTEA/VERSION"))

# The last arg is the JSON string
COMMAND_ARGS <- jsonlite::fromJSON(args[length(args)])

run_module <- function(module_name, module_args, exit_immediately = FALSE) {
    log$info("Saving environment...")
    # The order here is important: I want that .PREVIOUS_OPTIONS is saved in
    # .PREVIOUS_LS
    .PREVOUS_OPTIONS <- options()
    .PREVIOUS_PKGS <- paste0('package:', names(sessionInfo()$otherPkgs))
    .PREVIOUS_LS <- ls(all.names = TRUE)

    log$info("Setting module arguments (", paste(module_args, collapse = ", "), ")")
    options(module.args = module_args)

    # Run the module
    log$info("Running module '", module_name, "'...")
    source(paste0("/bioTEA/modules/", module_name, "/entrypoint.R"))

    if (exit_immediately) {
        quit(save = "no", status = 0)
    }

    # Restore the environment
    log$info("Resetting options...")
    # Keep only the "OWNERSHIP_REGISTER" option
    ownership_register <- getOption("OWNERSHIP_REGISTER")
    .NEW_OPTIONS <- options()
    .NEW_OPTIONS[] <- list(NULL) # This makes them all NULL
    options(modifyList(.NEW_OPTIONS, .PREVOUS_OPTIONS, keep.null = TRUE))
    options(OWNERSHIP_REGISTER = ownership_register)

    log$info("Unloading packages...")
    .NEW_PKGS <- paste0('package:', names(sessionInfo()$otherPkgs))
    .TO_UNLOAD <- .NEW_PKGS[! .NEW_PKGS %in% .PREVIOUS_PKGS]
    invisible(lapply(.TO_UNLOAD, detach, character.only=TRUE, unload=TRUE))

    log$info("Cleaning memory...")
    .NEW_LS <- ls(all.names = TRUE)
    .TO_REMOVE <- .NEW_LS[! .NEW_LS %in% .PREVIOUS_LS]
    rm(list = .TO_REMOVE)

    log$info("Environment cleaned.")
}

# Run the desired module(s)
tryCatch(
    {
        switch(
            COMMAND,
            prepaffy = {
                run_module("prepaffy", COMMAND_ARGS, exit_immediately = TRUE)
            },
            prepagil = {
                run_module("prepagil", COMMAND_ARGS, exit_immediately = TRUE)
            },
            annotation = {
                run_module("annotation", COMMAND_ARGS, exit_immediately = TRUE)
            },
            analyze = {
                run_module("analyze", COMMAND_ARGS, exit_immediately = TRUE)
            },
            utils = {
                run_module("utils", COMMAND_ARGS, exit_immediately = TRUE)
            },
            {
                msg <- paste0("Unrecognized command '", COMMAND, "'.")
                stop(msg)
            }
        )
    },
    error = function(cond) {
        log$debug("Got an error.")
        traceback()
        log$error(cond$message)
        message(cond$message)
        quit(status = 1, save = "no")
    }
)
