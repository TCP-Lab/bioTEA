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

# Header Info ------------------------------------------------------------------
#
# Agilent TXT raw data --to--> expression matrix
#
# a FeAR R-script - 27-Dec-2021
#
# NOTE
# read.maimages() function from limma requires as its first argument a data frame
# containing (at least) a column called 'FileName' with the names of the raw-data
# files to be used for the analysis. For Agilent arrays such a list is usually
# given in the form of a tab-delimited file named "Targets.txt", possibly
# containing other kind of information about the experimental design. However,
# this script ignores any Targets.txt file found within the input directory and
# builds its own target list run-time.
#
# Script outline:
#   - Raw data loading
#   - 'normexp' background correction
#   - Quantile-Quantile interarray normalization
#   - Negative control probe evaluation
#   - Invalid probes removal
#   - Expression matrix saving
#
# ------------------------------------------------------------------------------

log$debug("Sourcing the 'Agilent_TXT_to_Expression.R' file.")

agil2expression <- function (
  input_dir, output_file,
  grep_pattern = "*.(txt|TXT)",
  remove_controls = TRUE,
  n_plots = Inf
) {

  set.seed(1) # This module uses random sampling

  # Inputting data
  output_dir <- dirname(output_file)
  setwd(output_dir)

  log$info(paste0("Finding input files matching the pattern \'",
                  grep_pattern, "\'..."))
  raw_files <- list.files(path = input_dir, pattern = grep_pattern)

  # Remove possible "Targets.txt" file from row_files list
  target.index <- grep("targets.txt", raw_files, ignore.case = TRUE)
  if (length(target.index) > 0) {
    raw_files = raw_files[-target.index]
  }

  if (length(raw_files) == 0) {
    log$error("No input files found. Aborting.")
    stop_quietly()
  }

  log$info(
    paste("Found", length(raw_files), "input files:", paste(raw_files, collapse = ", "))
  )
  log$info("Reading in input files...")
  expression_data <- read.maimages(
    files = file.path(input_dir, raw_files),
    source = "agilent.median",
    green.only = TRUE
  )

  print_data <- as.data.frame(expression_data$E)
  colnames(print_data) <- raw_files
  print_data <- log(print_data, 2)
  if (nrow(print_data) > 100000) {
    log$info("Reducing raw dataset...")
    print_data <- reservoir_sample(print_data, 100000)
    print_data <- as.data.frame(print_data)
  }

  # Print MA plots to diagnose the data.
  if (n_plots > 0) {
    ma.plots <- get_better_mas(print_data, title = "Raw probes MA plot - {x} vs Median of other samples")

    if (n_plots != Inf) {
      stopifnot(
        "Invalid amount of plots to display"={is.wholenumber(n_plots)}
      )
      if (n_plots > length(ma.plots)) {
        log$warn(paste0(
          "Number of plots to display (", n_plots,
          ") is higher than the number of plots to be saved (", length(ma.plots),
          "). Printing all of them."
        ))
        n_plots <- length(ma.plots)
      }
      ma.plots <- ma.plots[1:n_plots]
    }

    pb <- progress_bar$new(
      format = "Saving plots... [:bar] :percent (:eta)",
      total = length(ma.plots), clear = FALSE, width= 80)
    pb$tick(0)
    for (i in seq_along(ma.plots)) {
      maplot <- ma.plots[[i]]
      printPlots(\() { suppressMessages(print(maplot)) }, paste(i, "-", maplot$labels$title))
      pb$tick()
    }
  } else {
    log$info("The number of plots is less or equal to 0. Skipping MA plot generation.")
  }


  log$info("Making overall boxplot...")
  p <- function(){
    bplot <- ggplot(data = melt(print_data), aes(y = value, x = variable)) +
      geom_boxplot(outlier.alpha = 0.5, outlier.size = 1) +
      theme_bw() +
      scale_x_discrete(
        labels = 1:length(print_data)
      ) +
      ylab("Expression") + xlab("Sample") +
      ggtitle("Unnormalized Boxplots")
    print(bplot)
  }
  printPlots(suppressMessages(p), "Unnormalized Boxplots")

  rm(print_data)

  log$info("Finished inputting data.")

  # Normalization
  log$info("Running Normalization")
  log$info("Background correcting...")
  expression_data <- limma::backgroundCorrect(expression_data, method = "normexp", offset = 50)

  # This step also log2s the data
  log$info("Running interarray normalization...")
  expression_data = limma::normalizeBetweenArrays(expression_data, method = "quantile")

  expression_set <- expression_data$E
  log$info(paste("Dataset dimensions - Cols:", ncol(expression_set), "Rows:", nrow(expression_set)))

  # Check the negative control probes to estimate the log2_intensity of
  # unhybridized spots, to be used as the threshold value for filtering in bioTEA
  neg.ctrl = expression_data$genes$ControlType == -1
  neg.id = unique(expression_data$genes$ProbeName[neg.ctrl])

  log$info(paste0(sum(neg.ctrl), " Negative-Control probes have been found, corresponding to ",
                  length(neg.id), " unique probe(s) [", neg.id, "]. Mean value of ",
                  length(expression_set[neg.ctrl,]), " unhybridized spots = ",
                  mean(expression_set[neg.ctrl,])))

  # Print MA plots to diagnose the data.
  if (n_plots > 0) {
    colnames(expression_set) <- make.names(raw_files)
    ma.plots <- get_better_mas(as.data.frame(expression_set), title = "Normalized MA plot - {x} vs Median of other samples")

    if (n_plots != Inf) {
      ma.plots <- ma.plots[1:n_plots]
    }

    pb <- progress_bar$new(
      format = "Saving plots... [:bar] :percent (:eta)",
      total = length(ma.plots), clear = FALSE, width= 80)
    pb$tick(0)
    for (i in seq_along(ma.plots)) {
      maplot <- ma.plots[[i]]
      printPlots(\() { suppressMessages(print(maplot)) }, paste(i, "-", maplot$labels$title))
      pb$tick()
    }
  } # No need to log the warning again.

  log$info("Making overall boxplot...")
  p <- function(){
    bplot <- ggplot(data = melt(as.data.frame(expression_set)), aes(y = value, x = variable)) +
      geom_boxplot(outlier.alpha = 0.5, outlier.size = 1) +
      theme_bw() +
      scale_x_discrete(
        labels = 1:length(as.data.frame(expression_set))
      ) +
      ylab("Expression") + xlab("Sample") +
      ggtitle("Normalized Boxplots")
    print(bplot)
  }
  printPlots(suppressMessages(p), "Normalized Boxplots")

  if (remove_controls) {
    log$info("Filtering out control probes...")
    control_probes <- abs(expression_data$genes$ControlType) == 1
    control_probes_percent <- round(sum(control_probes) / nrow(expression_set) * 100, 4)
    log$info(paste(
      "Found", sum(control_probes), "control probes,", control_probes_percent,
      "% of total probes. Removing them..."
    ))

    expression_data <- expression_data[!control_probes, ]
    expression_set <- expression_data$E
    log$info(paste("Filtered dataset dimensions - Cols:", ncol(expression_set), "Rows:", nrow(expression_set)))
  }

  log$info("Finding replicate probes and collapsing them...")
  # Replace value of replicate probes with their mean - Probe_ID is used to identify the replicates
  expression_data = avereps(expression_data,  ID = expression_data$genes$ProbeName)

  expression_set <- expression_data$E
  log$info(paste("Final dataset dimensions - Cols:", ncol(expression_set), "Rows:", nrow(expression_set)))

  expression_set <- as.data.frame(expression_set)
  colnames(expression_set) <- make.names(raw_files)

  log$info("Saving output file...")

  write_expression_data(expression_set, output_file)

}
