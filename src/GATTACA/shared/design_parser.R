# Header Info ------------------------------------------------------------------
#
# This file hosts the design parser for bioTEA
#
# ------------------------------------------------------------------------------

#' Expand labels with the intercalated strategy.
#'
#' @param core A string of comma separated labels to expand
#' @param times The number of times to expand
#' @param initial.num The initial number to use to expand wildcards.
#' @param ignore_asterisk If TRUE, asterisk wildcards are not replaced.
#'
#' @returns The expanded labels as a single string
#'
#' @author MrHedmad
expand_intercalated <- function(core, times, initial.num, ignore_asterisk = FALSE) {
  stopifnot(
    "`core` is an empty string" = {core != ""},
    "`core` starts with a comma. This leads to weird outputs" = {!startsWith(core, ",")},
    "One or more entries are empty strings, as if you sent a `,,`" = {
      all(str_split(core, ",")[[1]] != "")
    }
  )

  result <- ""
  rep.number <- initial.num
  for (i in rep(0, times)) {
    if (! ignore_asterisk) {
      uniquestr <- gsub("\\*", rep.number, core)
    } else {
      uniquestr <- core
    }
    result <- paste(result, uniquestr, sep = ",")
    rep.number <- rep.number + 1
  }
  # The result starts with an extra , as there is a paste("", ..., sep = ",")
  # This gets rid of it.
  result <- paste0(strsplit(result, "")[[1]][-1], collapse = "")

  return(result)
}


#' Expand labels with the ordered strategy.
#'
#' @param core A string of comma separated labels to expand
#' @param times The number of times to expand
#' @param initial.num The initial number to use to expand wildcards.
#' @param ignore_asterisk If TRUE, asterisk wildcards are not replaced.
#'
#' @returns The expanded labels as a single string
#'
#' @author MrHedmad
expand_ordered <- function(core, times, initial.num, ignore_asterisk = FALSE) {
  stopifnot(
    "`core` is an empty string" = {core != ""},
    "`core` starts with a comma. This leads to weird outputs" = {!startsWith(core, ",")},
    "One or more entries are empty strings, as if you sent a `,,`" = {
      all(str_split(core, ",")[[1]] != "")
    }
  )

  result <- ""
  rep.number <- initial.num
  for (entry in strsplit(core, ",")[[1]]) {

    for (i in rep(0, times)){
      if (! ignore_asterisk) {
        uniquestr <- gsub("\\*", rep.number, entry)
      } else {
        uniquestr <- entry
      }
      result <- paste(result, uniquestr, sep = ",")
      rep.number <- rep.number + 1
    }

    rep.number <- initial.num
  }

  # The result starts with an extra , as there is a paste("", ..., sep = ",")
  # This gets rid of it.
  result <- paste0(strsplit(result, "")[[1]][-1], collapse = "")

  return(result)
}


#' Get the capture groups from a regexec and regmatches call.
#'
#' @param patters A RegEx pattern to use.
#' @param string The string to test against
#'
#' @returns A vector with the first match found in first position and the
#' capture groups in the rest of the vector.
#'
#' @author MrHedmad
get_captures <- function(pattern, string) {
  matches <- regexec(pattern, string, perl = TRUE)
  captures <- regmatches(string, matches)

  return(captures[[1]])
}


#' Parser for experimental designs
#'
#' Each sample is marked with a letter (or series of letters) representing
#' conditions. They can also be marked with arbitrary numbers representing
#' sample pairs for paired designs. The letter and optional numbers are known as
#' labels, and are separated by commas (all spaces are ignored).
#'
#' The parser respects already expanded labels
#'   > design_parser("a, b, c") # Unpaired
#'     [1] "a, b, c"
#'   > design_parser("a1, b2, c3") # Paired
#'     [1] "a1, b2, c3"
#' To avoid high repetition, the parser supports two types of pattern expansion:
#' Round brackets represent intercalated expansions, where labels inside the
#' brackets are repeated in the order inside the brackets for the number
#' of times specified:
#'   > design_parser("(a, b):3") # Unpaired 'intercalated'
#'     [1] "a, b, a, b, a, b"
#' Square brackets represent ordered expansions, where each label in the brackets
#' is repeated on its own the number of times specified:
#'   > design_parser("[a, b]:3") # Unpaired 'ordered'
#'     [1] "a, a, a, b, b, b"
#'   > design_parser("[a, b, c]:3") # Unpaired 'ordered'
#'     [1] "a, a, a, b, b, b, c, c, c"
#' The * wildcard inside the brackets will be replaced with unique numbers, so
#' that expansion is actually useful for paired designs:
#'   > design_parser("(a*, b*):3") # Paired 'intercalated'
#'     [1] "a1, b1, a2, b2, a3, b3"
#'   > design_parser("[a*, b*]:3") # Paired 'ordered'
#'     [1] "a1, a2, a3, b1, b2, b3"
#'   > design_parser("[a*, b*, c*]:2") # Paired 'ordered'
#'     [1] "a1, a2, b1, b2, c1, c2"
#' The wildcard is assured to not collide with any already used numbers in the
#' pattern, starting one number after the largest number found:
#'   > design_parser("a3, b7, (a*, b*):3") # Paired 'intercalated'
#'     [1] "a3, b7, a8, b8, a9, b9, a10, b10"
#'   > design_parser("a3, b7, [a*, b*]:3") # Paired 'ordered'
#'     [1] "a3, b7, a8, a9, a10, b8, b9, b10"
#'  The various patterns can be mixed and matched:
#'   > design_parser("(a*, b*):3, a3, b6, [a*]:2")
#'     [1] "a7, b7, a8, b8, a9, b9, a3, b6, a10, a11"
#'  Note: Both types of expansion work identically if only one label is specified
#'  in the brackets.
#'
#' @param rawstr The raw design string to be parsed.
#' @param ignore_asterisk If TRUE, ignores any asterisks in the string.
#'
#' @author MrHedmad
design_parser <- function(rawstr, ignore_asterisk = FALSE) {
  rawstr <- gsub(" ", "", rawstr)

  # The `:x` modifiers pollute finding the max patient number. This removes
  # them.
  str_no_times = gsub(":[0-9]+", "", rawstr)
  used.numbers <- find_nums(str_no_times)
  initial.num <- if (is.null(used.numbers)) {1} else {max(used.numbers) + 1}

  # Split the initial string by commas *not* in parenthesis.
  splitted <- strsplit(rawstr, ",(?![^(^\\[]*[\\)\\]])", perl = TRUE)[[1]]

  # I put a lot of comments here as there are many nested ifs and its hard
  # to know what is doing what.
  intercalated.pattern <- "\\((.*?)\\):([0-9]+)"
  ordered.pattern <- "\\[(.*?)\\]:([0-9]+)"

  result <- ""

  for (unexpanded in splitted) {
    # There is some code duplication here that I leave for clarity as
    # this bit is complex enough already...
    if (length(grep(intercalated.pattern, unexpanded)) == 1) {
      captures <- get_captures(intercalated.pattern, unexpanded)
      expanded <- expand_intercalated(
        core = captures[2],
        times = as.integer(captures[3]),
        initial.num = initial.num,
        ignore_asterisk = ignore_asterisk
      )
      # As we've used as.integer(captures[3]) many nums, we need to increase
      # this number that much for the other expansions.
      initial.num <- initial.num + as.integer(captures[3])

      result <- paste0(result, expanded, sep = ",")
      next
    }
    if (length(grep(ordered.pattern, unexpanded)) == 1) {
      captures <- get_captures(ordered.pattern, unexpanded)
      expanded <- expand_ordered(
        core = captures[2],
        times = as.integer(captures[3]),
        initial.num = initial.num,
        ignore_asterisk = ignore_asterisk
      )
      # As we've used as.integer(captures[3]) many nums, we need to increase
      # this number that much for the other expansions.
      initial.num <- initial.num + as.integer(captures[3])

      result <- paste0(result, expanded, sep = ",")
      next
    }

    # If we get here then the unexpanded pattern needs no expanding
    result <- paste0(result, unexpanded, sep = ",")
  }

  # The result starts with an extra , as there is a paste("", ..., sep = ",")
  # This gets rid of it.
  result <- gsub('.{1}$', '', result)
  result <- gsub(",", ", ", result) # Some extra space to be easy on the eyes

  # The new design just needs to be splitted again...
  result <- strsplit(result, ", ")[[1]]
  return(result)
}


#' Split a parsed design into the groups and the pairings vectors.
#'
#' Essentially, the character (a-z) portion of each entry will be included in the
#' groups vector, while the numerical portion the same for the pairings vector.
#'
#' @param experimental_design An experimental design vector (such as one parsed
#'   by `design_parser`).
#' @returns A list with the groups vector of str in slot $groups and the pairings
#'   vector of ints in the $pairings vector.
#'
#' @author MrHedmad
split_design <- function(experimental_design) {

  stopifnot(
    "experimental_design must not contain empty strings" =
      {all(experimental_design != "")}
  )

  get_group <- function(x) {
    # Get rid of any numbers
    x <- gsub("[0-9]+", "", x, perl = TRUE)
    return(x)
  }

  get_pairings <- function(x) {
    # Get rid of any letters
    x <- gsub("[a-zA-Z]+", "", x, perl = TRUE)
    if (x == "") {
      return(NA)
    }
    return(as.integer(x))
  }
  # This complains about some comparison, but it seems to be a false positive.
  result <- suppressWarnings(
    list(
      groups = unlist(map(experimental_design, get_group)),
      pairings = unlist(map(experimental_design, get_pairings))
    )
  )
  return(result)
}
