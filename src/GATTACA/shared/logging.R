# The logging libs around are overkill. I just need two types of files:
# A data log, for writing data, and a normal log, to write output info.
# The log needs to be always saved in '/bioTEA/target/', and always
# sent to the console.
#
# Additionally, the `logging` library *always* makes the log files. I want them
# made only when something gets written to them. This mini-module fixes that.
#
# Author: Hedmad

LOGLEVELS <- list(
    "debug" = 10,
    "info" = 20,
    "warning" = 30,
    "error" = 40,
    # This level is meant to disable this kind of logging.
    # Doing so as a level is cheating, but is clean enough for me.
    "disable" = 1000
)

#' Internal function to set the variables needed for the log name.
#'
#' The path and extensions are automatically set.
.set_logname <- function(name) {
    biotea_log_path <- paste0("/bioTEA/logs/", name, ".log")
    biotea_data_log_path <- paste0("/bioTEA/logs/", name, "_data.log")

    options("bioTEA_log" = biotea_log_path)
    options("bioTEA_data_log" = biotea_data_log_path)

    register_for_ownership(biotea_log_path)
    register_for_ownership(biotea_data_log_path)
}


#' Internal to set the log level for the "target".
#'
#' The target can be stdout for the console, and file for the logfile.
#' Loglevels are taken from the above list (LOGLEVELS).
.set_loglevel <- function(level, target) {
    # Levels can be "debug" < "info" < "warning" < "error".
    # Only messages that match that level or above are logged.
    if (! level %in% names(LOGLEVELS)) {stop(paste("Invalid loglevel", level))}
    if (! target %in% c("stdout", "file")) {stop(paste("Invalid target", target))}

    # R FUCKING SUCKS BALLS - The paste0 function does not get evaluated, so it
    # tries to set it as the name, and fails. So i have to do this shit
    path <- paste0("bioTEA_log_level_", target)
    newopt <- list(LOGLEVELS[level])
    names(newopt) <- path
    options(newopt)
}

# Define some simple wrappers for later.
.set_loglevel_file <- function(level) {.set_loglevel(level, "file")}
.set_loglevel_stdout <- function(level) {.set_loglevel(level, "stdout")}


#' Internal function. Prepare a list of object for printing in a message.
#'
#' Handles converting them to string, and adding some padding. The names of the
#' objects in the list are saved as the object name.
#'
#' Example:
#'  .prep_data(list("A vector" = c(1:10), "Flowers" = iris))
.prep_data <- function(arg_list, padding = "    ") {
    string <- ""
    for (i in seq_along(arg_list)){
        string_data <- get.print.str(arg_list[[i]])
        padded <- paste0(padding, gsub("\\n", paste0("\n", padding), string_data))
        string <- paste0(string, "[", names(arg_list)[i], "] >>>\n", padded, "\n<<<\n")
    }
    return(string)
}

#' Internal function. Push strings to the log.
#'
#' Unnamed arguments are collapsed and pasted together if pushing to the
#' standard log, and passed to `.prep_data` if not.
#'
#' Raises an error if pushing to the file log, bug the filename has not been
#' set with `.set_logname` beforehand.
.push_to_log <- function(..., level, kind = "standard") {
    args <- list(...)

    if (kind == "data") {
        # The data log is given already as a list. So we "unlist" the list.
        args <- args[[1]]
    }

    if (! kind %in% c("standard", "data")) {stop(paste0("Invalid kind ", kind))}

    slevel <- names(LOGLEVELS)[unlist(LOGLEVELS) >= level][1]

    if (kind == "standard") {
        string <- paste0(unlist(args), collapse = "")
        msg <- paste0(date(), " [", slevel, "]: ", string, "\n")
    } else {
        msg <- .prep_data(args)
    }

    if (level >= getOption("bioTEA_log_level_stdout", default = 20)){
        cat(msg)
    }

    if (level >= getOption("bioTEA_log_level_file", default = 20)) {
        if (kind == "standard") {
            file <- getOption("bioTEA_log", default = stop("Unset log name. Cannot log messages to file."))
        } else {
            file <- getOption("bioTEA_data_log", default = stop("Unset log name. Cannot log messages to file."))
        }
        cat(msg, file = file, append = TRUE)
    }
}


# A wrapper list that exposes the logging functions.
log <- list(
    set_name = .set_logname,
    set_console_level = .set_loglevel_stdout,
    set_file_level = .set_loglevel_file,

    debug = \(...){.push_to_log(..., level = LOGLEVELS[["debug"]])},
    info = \(...){.push_to_log(..., level = LOGLEVELS[["info"]])},
    warn = \(...){.push_to_log(..., level = LOGLEVELS[["warning"]])},
    error = \(...){.push_to_log(..., level = LOGLEVELS[["error"]])},
    data = \(data_list){.push_to_log(data_list, level = LOGLEVELS[["info"]], kind = "data")}
)
