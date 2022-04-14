#!/usr/bin/env Rscript

# License ----------------------------------------------------------------------
# MIT License
#
# Copyright (c) 2021 Feat-FeAR
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# ------------------------------------------------------------------------------


#' Merge expression matrices with annotations and sort them.
#'
#' Fuses by row names.
#'
#' @param gene.stat The table of genes, usually a DEG summary-statistic top-table
#'   or an expression matrix.
#' @param annotation the matrix containing the annotation data
#' @param sort.by the name or index of the column used to sort the final data set
#'
#' @author FeAR, MrHedmad
merge_annotations <- function(gene.stat, annotation, sort.by = 1) {
  log$info("Checking if annotation data can be used to annotate input...")
  # Check the matching degree between array and annotation layouts
  missing_probes <- sum(
    ! row.names(gene.stat) %in% row.names(annotation)
  )
  missing_perc <- missing_probes / length(gene.stat) * 100
  if (missing_perc >= 99.9) {
    stop("There are no annotations for this data. Please use a specific database, or change database.")
  } else {
    if (missing_probes > 0) {
      log$warn(paste0(
        "There are ", missing_probes, " probes with no annotations. ",
        round(missing_perc, 4), "% of total."
      ))
    }
  }

  log$info("Checking for conflicting columns...")
  conflicting_cols <- colnames(gene.stat) %in% colnames(annotation)
  if (sum(conflicting_cols) > 0) {
    conflicting_cols_name <- colnames(gene.stat)[conflicting_cols]
    log$warn(paste0(
      "Found conflicting columns in input data: ",
      paste(conflicting_cols_name, collapse = ", "),
      ". Overriding them with new annotation data."
    ))
  }

  gene.stat <- gene.stat[, !conflicting_cols]

  # 'merge' function to merge two matrix-like objects horizontally
  # and cast to data frame (right outer join).
  # NOTICE: both gene.stat and annotation are supposed to have the Probe_IDs
  # as row names
  joined = merge(
    annotation, gene.stat,
    by.x = "row.names", by.y = "row.names"
  )
  # The merge has to convert the row names to a column. This reverts it.
  rownames(joined) = joined[,1]
  gene.stat = joined[,-1]

  # Print out the number of NAs in the annotations for each type of annotation
  notMap = matrix(0, nrow = 2, ncol = dim(joined)[2],
                  dimnames = list(c("NA entries","%"), colnames(joined)))
  for (i in colnames(joined)) {
    notMap[1,i] = sum(is.na(joined[,i]))
    notMap[2,i] = round(notMap[1,i]/dim(joined)[1]*1e2, digits = 2)
  }
  # Take only the cols from the annotations
  notMap <- notMap[, colnames(annotation)]
  log$info(paste("Missing annotations:", get.print.str(notMap), sep = "\n"))

  # Re-sort the data frame by the content of 'sort.by' column
  # ('sort.by' can be either a number or a column name)
  gene.stat = gene.stat[order(gene.stat[,sort.by]),]

  return(gene.stat)
}


#' Get the possible annotation names of a specific database.
#'
#' This also installs and loads the database package from Bioconductor.
#'
#' @param db_namespace The name of the library containing the db
#'
#' @returns A string vector with the names of the available annotations.
#'
#' @author MrHedmad
get_db_names <- function(db_namespace) {
  suppressWarnings({
    stopifnot("Invalid database name - cannot be empty"=db_namespace==character(0))
    if (!require(db_namespace, character.only = TRUE)) {
      BiocManager::install(db_namespace, update = FALSE)
      suppressPackageStartupMessages(library(db_namespace, character.only = TRUE))
    }
  })
  possibilities <- ls(paste0("package:", db_namespace))
  db_name = gsub("\\.db", "", db_namespace)
  possibilities <- sapply(
    possibilities, gsub,
    pattern = db_name, replacement = ""
  )

  # Clean out the things that start with _ or . as they are functions and junk
  possibilities <- sapply(
    possibilities, gsub,
    pattern = "^[_\\.].*", replacement = "")

  possibilities <- possibilities[possibilities != ""]
  names(possibilities) <- NULL
  return(possibilities)
}


#' Get specified annotations from a certain db from bioconductor.
#'
#' @param db_name The name of the db package.
#' @param selections A vector of strings with the annotations to get from the db.
#'   The available annotations can be seen by using the `get_db_names` function
#' @returns A data.frame with probe IDs as rownames and one column per annotation.
#'
#' @author MrHedmad
get_remote_annotations <- function(
  db_name, selections = c("ACCNUM", "SYMBOL", "GENENAME")
) {
  library(purrr)
  # get_db_names also loads the db in memory, so I don't do it here.
  possible_selections <- get_db_names(db_name)
  log$info("Checking selections...")
  if (!all(selections %in% possible_selections)) {
    stop(
      paste0(
        "Invalid selection(s): ",
        paste(selections[!selections %in% possible_selections], collapse = ", "),
        ". Possible selections: ",
        paste(possible_selections, collapse = ", "),
        "."
      )
    )
  }

  # Load the data
  log$info("Loading the annotation data...")
  db_clean_name <- gsub("\\.db", "", db_name)
  data <- as.list(rep(NA, times = length(selections)))
  names(data) <- selections

  for (selection in selections) {
    # This raises deprecation warnings for no reason.
    suppressWarnings(
      { data[selection] <- get(paste0(db_clean_name, selection)) }
    )
  }

  merge_genedata <- function(x, y) {
    return(
      merge(x, y, by = "probe_id", all = TRUE)
    )
  }

  collapse_to_str <- function(inputlist) {
    sapply(
      contents(inputlist),
      function(x) {
        suppressWarnings({if (is.na(x)){x} else {paste(x, collapse = " /// ")}})}
    )
  }

  dataframetize <- function(named_vector, name) {
    probe_id <- names(named_vector)
    data <- data.frame()[1:length(named_vector), ]
    data[[name]] <- named_vector
    data$probe_id <- probe_id
    return(data)
  }

  # We need dataframes to manipulate
  log$info("Collapsing annotations...")
  data <- map(data, collapse_to_str)
  log$info("Casting annotations...")
  container <- as.list(rep(NA, length(data)))
  names(container) <- names(data)
  for (i in seq_along(data)) {
    container[[i]] <- dataframetize(data[[i]], name = names(data)[i])
  }
  data <- container
  rm(container)
  log$info("Collapsing annotations again...")
  data <- purrr::reduce(data, merge_genedata)

  # The merging functions need the probe_ids as rownames
  log$info("Cleaning up rownames...")
  row.names(data) <- data$probe_id
  data$probe_id <- NULL

  return(data)
}

#' Annotate a dataframe with annotation data.
#'
#' Annotates with SYMBOL, GENENAME, ENSEMBL, package_name and version columns,
#' denoting in order the gene symbol (if any), the long gene name, the ensembl
#' id(s) related to the gene, the source annotation package name(s) and
#' version(s).
#'
#' If `database_name` is unspecified, uses the internal annotation data.
#' Otherwise uses the data from the specified database to perform the annotation.
#'
#' The external package must have the SYMBOL, GENENAME, and ENSEMBL columns, or
#' this function will fail.
#'
#' @param expression_set A data.frame with row.names the probe ids.
#' @param database_name An optional str representing the name of the database
#'   to source the annotations from. Defaults to `NA`.
#'
#' @returns A dataframe with additional annotation columns.
annotate_data <- function(expression_set, database_name = NA) {
  log$info("Finding annotations...")
  # Added compatibility patch with bioTEA. Also runs when passing "TRUE"
  if (is.na(database_name) | database_name == TRUE) {
    log$info("Loading local annotations.")
    load(file = "/bioTEA/modules/annotation/resources/full_annotations.RData")

    if (! "full_annotations" %in% ls()) {
      stop("I loaded something, but it did not contain the 'full_annotations' object.")
    }
  } else {
    log$info("Loading remote annotations.")
    full_annotations <- get_remote_annotations(
      database_name, selections = c("SYMBOL", "GENENAME", "ENSEMBL")
      )
    full_annotations$package_name <- database_name
    full_annotations$version <- packageVersion(database_name)
  }

  log$info("Merging annotations with the data...")
  merged_data <- merge_annotations(expression_set, full_annotations)

  return(merged_data)
}


#' Annotate a file containing expression data with annotations from a db.
#'
#' The file needs to be in .csv format with a column named "probe_id" representing
#' probe ids. The output file is similarly formatted.
#'
#' Annotates with SYMBOL, GENENAME, ENSEMBL, package_name and version columns,
#' denoting in order the gene symbol (if any), the long gene name, the ensembl
#' id(s) related to the gene, the source annotation package name(s) and
#' version(s).
#'
#' If `database_name` is unspecified, uses the internal annotation data.
#' Otherwise uses the data from the specified database (from bioconductor)
#' to perform the annotation.
#'
#' The external package must have the SYMBOL, GENENAME, and ENSEMBL columns, or
#' this function will fail.
#'
#' @param expression_data_path A str with the path to the csv file containing
#'   the data to be annotated.
#' @param output_path A full path to the output file.
#' @param database_name An optional str representing the name of the database
#'   to source the annotations from. Defaults to `NA`, loading the local
#'   annotations.
annotate_to_file <- function(expression_data_path, output_path, database_name) {
  log$info("Loading input data...")
  expression_set <- read_expression_data(expression_data_path)

  log$info("Annotating data...")
  annotated_set <- annotate_data(expression_set, database_name)

  write_expression_data(annotated_set, output_path)
  log$info("Written annotations.")
}
