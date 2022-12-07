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
# bioTEA v3
# General Algorithm for The Transcriptional Analysis by one-Channel Arrays
#
# a FeAR R-script
#
# Pipeline for one-Color (HD) Microarrays
# Data are supposed to be already background-subtracted, log2-transformed, and
# interarray-normalized
#
# ------------------------------------------------------------------------------

# Stupid r is stupid. Need this to use in pipes
dp_select <- dplyr::select

#' Make plots to diagnose the presence of batch effects
#'
#' Uses printPlots to save plots, and assumes its options are already set.
#'
#' The produced plots are an hierarchical cluster and a PCA plot.
#'
#' @param expression_set The expression set to evaluate
#' @param groups The groups associated with the data (for plotting)
#' @param group_colors The colours associated with the groups of the data
#'
#' @author Hedmad
diagnose_batch_effects <- function(
  expression_set, groups, group_colors, title_mod = NULL
) {
  log$info("Starting sample-wise hierarchical clustering and PCA for batch-effect detection...")
  # Matrix Transpose t() is used because dist() computes the distances between
  # the ROWS of a matrix
  # Distance matrix (NOTE: t(expression_set) is coerced to matrix)
  log$info("Performing hierarchical clustering...")
  expression_set |> t() |> dist() |> hclust(method = "ward.D") ->
    hierarchical_clusters

  printPlots(
    \(){plot(hierarchical_clusters)},
    paste("Dendrogram", title_mod, sep = " - ")
  )

  log$info("Performing PCA...")
  # Bundle some metadata in the PCA object for later.
  # Strictly enforced that rownames(metadata) == colnames(expression_set)
  metadata = data.frame(
    groups = groups,
    row.names = colnames(expression_set)
  )

  # Do the PCA (centering the data before performing PCA, by default)
  PCA_object = pca(expression_set, metadata = metadata)

  log$info("Finished running PCA. Plotting results...")
  printPlots(
    \(){print(screeplot(PCA_object))},
    paste("Scree Plot", title_mod, sep = " - ")
  )

  printPlots(
    \() {
      suppressMessages(print(
        biplot(
          PCA_object, colby = "groups", colkey = group_colors,
          title = "Principal Component Analysis"
        )
      ))
    },
    paste("PCA", title_mod, sep = " - ")
  )

  printPlots(
    \() {
      suppressMessages(print(
        pairsplot(
          PCA_object, colby = "groups", colkey = group_colors,
          title = "Paired PCA Plots"
        )
      ))
    },
    paste("PCA Pairs", title_mod, sep = " - ")
  )
}


#' Run groupwise filtering on expression data.
#'
#' The expression data *must* be named with colnames starting with the same
#' groups `groups`, for example by using `make.names(groups)`.
#'
#' The data is filtered by keeping genes that are expressed more than
#' `expression_threshold` in at least `min_groupwise_presence` samples in
#' at least one group.
#'
#' @param expression_set A data.frame with the data to be filtered.
#' @param groups A vector with the group names that correspond to the columns
#'   in the `expression_set`.
#' @param min_groupwise_presence The minimum groupwise presence used to filter.
#' @param expression_threshold The log2 expression threshold to use to consider
#'   a gene to be sufficiently expressed in a sample.
#'
#' @returns A data.frame with the filtered data.
filter_expression_data <- function(
  expression_set, groups, min_groupwise_presence, expression_threshold
) {
  log$info(
    "Filtering a ", ncol(expression_set), " cols by ", nrow(expression_set),
    " rows expression set."
  )

  original_dims <- dim(expression_set)

  report <- paste(
    "Initial dims: ", ncol(expression_set), " cols by ", original_dims[1], " rows",
    "\nMin presence per group: ", min_groupwise_presence,
    "\nExpression threshold (log2): ", expression_threshold
    )

  log$debug(
    "Raw colnames:", paste(colnames(expression_set), collapse = ", "),
    "\nGroups:", paste(groups, collapse = ", ")
  )

  group_sizes <- table(groups)
  min_presences <- ceiling(group_sizes * min_groupwise_presence)

  table_report <- data.frame(
    groups = names(group_sizes),
    sizes = as.vector(group_sizes),
    min_presence = as.vector(round(group_sizes * min_groupwise_presence, 2)),
    actual_min_presence = as.vector(min_presences)
  )

  report <- paste(report, get.print.str(table_report), sep = "\n")

  # To filter, we do three things. First, we look at the data and split it
  # groupwise. Secondly, we apply to each split, row-wise, a function that
  # gives us TRUE if at least `min_presence` columns have expression over
  # `expression_threshold` expression. We save these vectors into a data frame,
  # the `truth_dataframe`, with a col for each group. Finally, we collapse
  # this dataframe to a single truth key, with one entry per row, by applying
  # `any` row-wise, thus keeping an entry if the function described above is
  # true in at least one group.
  truth_dataframe <- data.frame(row.names = rownames(expression_set))

  for (group in unique(groups)) {
    # I cannot use "filter" as we first need to compare all cols to see if
    # the rowwise test passes, and only then we can filter.
    filter_fun <- function(frame) {
      kOverA_filter <- kOverA(min_presences[[group]], expression_threshold)
      frame |> apply(MARGIN = 1, FUN = kOverA_filter) ->
        filter_key
      return(filter_key)
    }

    expression_set |> dp_select(starts_with(group)) |> filter_fun() ->
      truth_dataframe[[group]]
  }

  truth_dataframe |> apply(2, sum) -> nr_kept_genes
  report <- paste(
    report, "--------------\nRetained genes:", get.print.str(nr_kept_genes),
    "Percentage:", get.print.str(nr_kept_genes / original_dims[1] * 100),
    sep = "\n"
  )

  truth_dataframe |> apply(1, any) -> truth_key
  expression_set <- expression_set[truth_key, ]

  report <- paste(
    report,
    paste("-----------\nFiltered dims:", ncol(expression_set), "cols by ", nrow(expression_set), "rows"),
    paste("Percentage retained genes:", nrow(expression_set) / original_dims[1] * 100),
    sep = "\n"
    )

  log$data(list("filtering_report" = report))
  return(expression_set)
}


#' Produce a limma design matrix given a set of groups and potentially other
#' variables
#'
#' Only the groups are assured to be all included (so all of them can be
#' used by `makeContrasts`).
#'
#' @param groups A character vector with the group names, in the order of the
#'   columns of the data.
#' @param ... Any character vector, with arbitrary names, included as variables
#'   in the design matrix. Note that one level from each variable is removed
#'   due to how `model.matrix` works.
#'
#' @author Hedmad
make_limma_design <- function(groups, ...) {
  args <- list(...)
  if (length(args) != 0) {
    stopifnot(
      "All extra variables must be named (as in `some_var = c(...)`)" = {
        !is.null(names(args)) & all(names(args) != "")
      },
      "The `groups` variable and all other variables must have the same length."={
        all(length(groups) == sapply(args, length))
      },
      "All extra variable names must be unique" = {
        # This might already be enforced...?
        length(args) == length(unique(names(args)))
      }
    )

    factor_vars <- lapply(args, as.factor)
  }

  groups <- as.factor(groups)
  factorial_scheme <- paste0("Factor: groups\n   ",
                             "Levels: ", paste(levels(groups),
                                               collapse = ", "))

  str_formula <- "~ 0 + groups"
  # The matrix names follow a pattern:
  #   - The groups are always all included, in alphabetical order.
  #   - The other vars are included, in alphabetical order, except for the
  #     first level of each factor.
  groups |> levels() |> sort() -> matrix_names
  if (length(args) != 0) {
    for (some_var in names(factor_vars)) {
      str_formula <- paste0(str_formula, " + factor_vars$", some_var)
      factorial_scheme <- paste(factorial_scheme,
                                paste0("Factor: ", some_var, "\n   ",
                                       "Levels: ",
                                       paste(levels(factor_vars[[some_var]]),
                                             collapse = ", ")),
                                sep = "\n")
      pretty_level_names <- paste(
        some_var, levels(factor_vars[[some_var]])[-1], sep = "_"
      )
      matrix_names <- c(
        matrix_names, pretty_level_names
      )
    }
  }

  pretty_formula <- paste("log2(expression)",
                          str_remove_all(str_formula, "factor_vars\\$"))

  log$data(list(
    "linear_model_formula" = paste0(pretty_formula, "\n", factorial_scheme)
  ))

  mm <- model.matrix(formula(str_formula))
  # NOTE: The model matrix has a variety of attributes that *could* mean
  # something. Some of them carry the original variable names of the data,
  # as well as the colnames. I was worried that the calculations would be
  # affected in some way by these variables (like the $contrasts attribute)
  # so I tested it (November 17, 2021, limma ver 3.50.0) by removing these
  # arguments from the model matrix before running limma. The outputs are
  # identical in the two cases (tested with `identical()`), so I feel
  # confident in replacing the colnames here, as I think that these extra
  # attributes are ignored by limma. - Hedmad
  #
  # The attribute $contrasts is just to keep track of the parametrization
  # used to dummy-code categorical variables within the design matrix (see
  # getOption("contrasts")). So yes, colnames can be safely replaced and
  # yes, the $contrasts attribute is ignored by lmFit(). However it should
  # be noted that different dummy codes ("contr.treatment" vs "contr.sum")
  # lead to different beta coefficients when using a model formula with
  # intercept; in that case you have to set
  #   options(contrasts = c("contr.treatment", "contr.poly"))
  # in order to get the actual logFC values in the first column of topTable()
  # during a DEA. Notably, such an option does not affect model.matrix()
  # output if the design matrix is defined with no intercept (~0) as we do
  # here in bioTEA. - FeAR
  colnames(mm) <- matrix_names

  return(mm)
}


#' Run a DEA with `limma`.
#'
#' @param expression_set The expression set to run the analysis with.
#'   It is a data.frame with row names as probes and column as samples. The
#'   col names are assumed to start with one of the possible levels in the
#'   experimental design.
#' @param design The design matrix for the limma analysis.
#' @param contrasts A character vector describing contrasts suitable for
#'   `limma::makeContrasts`.
#' @param fc_threshold The absolute Fold-change threshold to use to consider
#'   a gene as differentially expressed, regardless of the P-value or FDR.
#'
#' @returns A list of Data.frames, each with the DEGs found using a different
#'   contrast. Each dataframe has a column named `markings`, with the marking
#'   of either Upregulated (`1`), Downregulated (`-1`) or Not Significant (`0`).
#'
#' @author FeAR, Hedmad
run_limma <- function(
  expression_set, design, contrasts,
  fc_threshold = 0.5
) {
  log$info("Running differential expression analysis by limma.")

  log$info("Making contrasts matrix...")
  makeContrasts(
    contrasts = contrasts,
    levels = design
  ) -> contrast_matrix

  log_data(contrast_matrix, "Contrast Matrix", shorten = FALSE)

  log$info("Computing contrasts...")
  lmFit(expression_set, design) -> limma_fit

  limma_fit |> contrasts.fit(contrast_matrix) |> eBayes() -> limma_Bayes

  log$info("Getting Differential expressions...")
  pb <- progress_bar$new(
    format = "Generating... [:bar] :percent (:eta)",
    total = length(contrasts), clear = FALSE, width= 80)
  pb$tick(0)

  DEGs.limma = list() # Create an empty list
  for (i in seq_along(contrasts)) {
    # This fills the list with data.frames
    DEGs.limma[[i]] = topTable(
      limma_Bayes, coef = i, number = Inf,
      adjust.method = "BH", sort.by = "B"
    )
    log$info(paste(colnames(DEGs.limma[[i]]), collapse = ", "))
    pb$tick()
  }

  names(DEGs.limma) <- contrasts

  # Markings for DEGs given a P-value
  # We don't filter it out here as it is not suggested. See the help for the
  # `decideTests` function.
  decideTests(limma_Bayes, adjust.method = "BH", p.value = 0.05) |>
    as.data.frame() -> markings
  for (contrast in seq_along(contrasts)) {
    # Add the "markings" column
    contr_markings <- markings[contrast]
    colnames(contr_markings) <- "markings"
    DEGs.limma[[contrast]] <- merge(
      DEGs.limma[[contrast]], contr_markings, sort = FALSE, by = "row.names"
    )
    rownames(DEGs.limma[[contrast]]) <- DEGs.limma[[contrast]]$Row.names
    DEGs.limma[[contrast]]$Row.names <- NULL

    # Filter out markings with low FC
    DEGs.limma[[contrast]]$markings[
      abs(DEGs.limma[[contrast]]$logFC) < fc_threshold
    ] <- 0
  }

  # Show Hyperparameters
  d0 = limma_Bayes$df.prior           # prior degrees of freedom
  dg = mean(limma_fit$df.residual)    # original degrees of freedom
  hyp = cbind(
    c(limma_Bayes$s2.prior,           # prior variance
      mean(limma_fit$sigma^2),        # mean sample residual variance
      mean(limma_Bayes$s2.post),      # mean posterior residual variance
      d0, dg, d0/(d0+dg))             # Shrinkage degree
  )
  rownames(hyp) = c(
    "Prior Var",
    "Sample Residual Mean Var",
    "Posterior Residual Mean Var",
    "Prior df",
    "Original df",
    "Shrinkage degree"
  )
  colnames(hyp) = "Hyperparameters"
  log_data(hyp, "Limma Hyperparameters", shorten = FALSE)

  return(DEGs.limma)
}


#' Extract markings from a list of limma DEGs.
#'
#' Looks for the `markings` column and extracts it in a new data.frame.
#' Uses the last Data frame rownames for simplicity, assuming all frames
#' come from the same call of `run_limma` or `run_rankprod`.
extract_markings <- function(DEGs.list) {
  container <- list()
  for (contrast in names(DEGs.list)) {
    container[contrast] <- DEGs.list[[contrast]]["markings"]
  }
  container <- data.frame(container)
  rownames(container) <- rownames(DEGs.list[[contrast]])
  return(container)
}

#' Prints out either a venn or an upset plot from a markings frame,
#' such as that of `extract_markings`.
#'
#' If the data does not allow either, does not plot anything.
#'
#' Uses the PrintPlots function, so the options can be set from outside
#' this function.
make_overlaps_from_markings <- function(markings, toolname = "") {
  if (length(markings) > 3 & sum(markings) > 0) {
    # Prepare the dataset to allow the upset plot
    upset_data <- list()
    for (i in seq_along(markings)) {
      upset_data[[colnames(markings)[i]]] <- rownames(markings)[as.logical(markings[[i]])]
    }
    upset_data <- fromList(upset_data)

    printPlots(
      \() {
        upset(
          upset_data, nsets = Inf, nintersects = NA,
          keep.order = TRUE
        ) |> print()
      },
      paste("Intersection Plot", toolname, sep = " - ")
    )

  } else if (sum(markings) > 0) {
    printPlots(\(){vennDiagram(markings)}, paste("Upset Plot", toolname, sep = " - "))
  } else {
    log$warn("Cannot plot a Venn or Upset Plot as no DEGs have been detected.")
  }
}


#' Make and save (in the current WD) diagnostic plots for limma results.
#'
#' This prints some plots with `printPlots`, and assumes that the correct
#' options have already been selected.
#'
#' The plots include: A Venn diagram OR an upset plot of the detected DEGs
#' (if there are more than 3 contrasts), a set of MA plots with the detected
#' DEGs highlighted, and a set of volcano plots with the (annotated) DEGs.
#'
#' Looks for a column named "SYMBOL" to source the volcano plot labels with,
#' else uses the rownames.
#'
#' @param DEGs.limma A named list of Data.frames. The names of the list must
#'   represent the contrasts that are described in the data.frames.
#'   Looks for a column named `markings` in each frame that describes which
#'   genes are Upregulated (`1`), Downregulated (`-1`) or Not Significant (`0`).
#' @param fc_threshold The FC threshold that was used when calculating the
#'   DEGs.limma.
#' @param volcano_colour Colour to use to mark interesting points in the volcano
#'   plot.
#'
#' @author FeAR, Hedmad
diagnose_limma_data <- function(
  DEGs.limma, fc_threshold = 0.5, volcano_colour = "firebrick3"
) {
  log$info("Making limma DEG plots...")

  extract_markings(DEGs.limma) |> abs() -> markings

  make_overlaps_from_markings(markings, toolname = "limma")

  # MA-Plots with significant DEGs
  # Find Axis Limits
  log$info("Finding axis limits...")
  max.M.value = max(sapply(DEGs.limma, \(data){max(abs(data$logFC))} ))
  max.A.value = max(sapply(DEGs.limma, \(data){max(data$AveExpr)} ))
  min.A.value = min(sapply(DEGs.limma, \(data){min(data$AveExpr)} ))
  min.P.value = min(sapply(DEGs.limma, \(data){min(data$P.Value)} ))

  # MA-Plot with DEGs
  log$info("Making Limma MA-Plots...")
  # I cannot place a progress bar here as `printPlots` logs to stdout.
  for (i in seq_along(DEGs.limma)) {
    # Mark in red/blue all the up-/down- regulated genes
    # This MA plot is made with the LogFC and estimates from the TopTables
    printPlots(
      \() {
        mamaplot(
          x = DEGs.limma[[i]]$AveExpr, y = DEGs.limma[[i]]$logFC,
          input_is_ma = TRUE,
          highligths = list(
            "red" = DEGs.limma[[i]]$markings == 1,
            "blue" = DEGs.limma[[i]]$markings == -1
          ),
          xrange = c(min.A.value, max.A.value),
          yrange = c(NA, max.M.value),
          title = paste0("MA plot with Limma DEGs: ", names(DEGs.limma)[i])
        ) |> print()
      },
      paste("MA-Plot with Limma DEGs ", names(DEGs.limma)[i], sep = "")
    )
  }

  # Volcano Plots
  log$info("Making Limma Volcano plots...")
  for (i in seq_along(DEGs.limma)) {
    volcano_p_threshold = find_BH_critical_p(DEGs.limma[[i]]$adj.P.Val)

    # Enhanced Volcano Plot
    if ("SYMBOL" %in% colnames(DEGs.limma[[i]])) {
      volcano_labels <- DEGs.limma[[i]]$SYMBOL
    } else {
      volcano_labels = rownames(DEGs.limma[[i]])
    }

    get_h_volcano <- function() {
      # NOTICE: When in a for loop, you have to explicitly print your
      # resulting EnhancedVolcano object
      suppressWarnings(
        # This prints warnings as - I think - the internal implementation
        # uses xlim and ylim but ggplot2 ignores them.
        print(
          EnhancedVolcano(
            DEGs.limma[[i]],
            x = "logFC", y = "P.Value",
            pCutoff = volcano_p_threshold, FCcutoff = fc_threshold,
            pointSize = 1,
            col = c("black", "black", "black", volcano_colour),
            lab = volcano_labels,
            #selectLab = myLabels[1:high.DEG],
            labSize = 4,
            title = names(DEGs.limma)[i],
            subtitle = "Limma",
            legendPosition = "none"
          )
        )
      )
    }
    printPlots(get_h_volcano, paste("Volcano with Limma DEGs ", names(DEGs.limma)[i], sep = ""))
  }
}


#' Run a DEA with `RankProd`.
#'
#' @param expression_set The expression set to run the analysis with.
#'   It is a data.frame with row names as probes and column as samples. The
#'   col names are assumed to start with one of the possible levels in the
#'   experimental design.
#' @param groups A vector describing the groups of the data, in the same order
#'   as the columns.
#' @param contrasts A character vector describing contrasts.
#' @param batches The batches the samples fall in. Leave to `null` if none.
#'   Setting both this and `pairings` is unsupported.
#' @param pairings A vector describing the pairings of the data or NULL if no
#'   pairings are present. Must be the same length as the columns of the
#'   expression_set. Setting both this and `batches` is unsupported.
#' @param fc_threshold The absolute Fold-change threshold to use to consider
#'   a gene as differentially expressed, regardless of the P-value or FDR.
#'
#' @returns A list of Data.frames, each with the DEGs found using a different
#'   contrast. Each dataframe has a column named `markings`, with the marking
#'   of either Upregulated (`1`), Downregulated (`-1`) or Not Significant (`0`).
#'
#' @author FeAR, Hedmad
run_rankprod <- function(
  expression_set, groups, contrasts,
  batches = NULL,
  pairings = NULL, fc_threshold = 0.5
) {
  log$info("Running differential expression analysis with rankproduct...")

  # Handling batches AND pairings is currently not supported.
  if (!is.null(batches) & !is.null(pairings)) {
    log$warn("Cannot handle both batches and pairings. Setting BATCHES to `null`.")
    batches <- NULL
  }

  # Make a container for the rankprod results
  DEGs.rankprod = list()

  log$info(paste(
    "Rankprod Parameters:",
    paste("Groups:", paste(groups, collapse = ", ")),
    paste("Pairings:", paste(pairings, collapse = ", ")),
    paste("Batches:", paste(batches, collapse = ", "))
  ))

  log$info("Renaming columns according to groups input...")
  colnames(expression_set) <- make.unique_from1(groups)

  for (i in seq_along(contrasts)) {
    log$info(paste0("Running analysis ", i, " of ", length(contrasts)))

    log$info("Finding control and case groups...")
    unpacked_groups = strsplit(contrasts[i], split = "-", fixed = TRUE)[[1]]

    control_group <- unpacked_groups[2]
    treated_group <- unpacked_groups[1]

    expression_set |> dp_select(
      starts_with(control_group) | starts_with(treated_group)
    ) -> sub_expression_set

    # The above filter rearranges the columns. This crashes later as the
    # column order is needed to match the pairings for subtraction.
    # So we return to the original one here.
    original_col_order <- colnames(expression_set)[
      colnames(expression_set) %in% colnames(sub_expression_set)
    ]
    sub_expression_set <- sub_expression_set[, original_col_order]

    # Make the cl array, representing class labels of the sample
    colnames(sub_expression_set) |> startsWith(treated_group) |> as.numeric() ->
      rp_class_labels

    if (!is.null(pairings)) {
      # To make a paired run, we need to subtract the matching paired sets.
      # First, we need to get the pairings that survived the filtering
      colnames(expression_set) %in% colnames(sub_expression_set) ->
        surviving_cols
      sub_parings <- pairings[surviving_cols]

      # We make an empty dataframe to hold the subtraction result
      subtracted_expression_set <- data.frame(row.names = rownames(sub_expression_set))
      for (pairing in unique(sub_parings)) {

        # Get only the columns of this pair...
        paired_expression_set <- sub_expression_set[sub_parings == pairing]
        # Select the "control" and "treated" group columns
        paired_expression_set |> dp_select(starts_with(control_group)) ->
          control_set
        paired_expression_set |> dp_select(starts_with(treated_group)) ->
          treated_set

        # If there is more than one sample for each pairing in each set,
        # we need to collapse them in some way as there is ambiguity on how
        # to subtract them.
        if (ncol(control_set) != 1) {
          log$warn(
            paste0(
              "There are more than 1 '", control_group,
              "' paired samples (",
              paste(colnames(control_set), collapse = ", "), ").",
              " The average of them will be used instead."
            )
          )
          control_set <- rowMeans(control_set)
          control_set <- as.data.frame(control_set)
          colnames(control_set) <- paste0("averaged_", control_group)
        } else if (ncol(control_set) == 0) {
          stop(
            paste0(
              "There are no paired columns for group '", control_group,
              "' for pairing ", pairing
            )
          )
        }

        if (ncol(treated_set) != 1) {
          log$warn(
            paste0(
              "There are more than 1 '", treated_group,
              "' paired samples (",
              paste(colnames(treated_set), collapse = ", "), ").",
              " The average of them will be used instead."
            )
          )
          treated_set <- rowMeans(treated_set)
          treated_set <- as.data.frame(treated_set)
          colnames(treated_set) <- paste0("averaged_", treated_group)
        } else if (ncol(treated_set) == 0) {
          stop(
            paste0(
              "There are no paired columns for group '", treated_group,
              "' for pairing ", pairing
            )
          )
        }

        # Now, control and treated sets can be subtracted as they are one
        # and only one column.
        subtracted_partial_set <- control_set - treated_set

        subtracted_expression_set <- merge(
          subtracted_expression_set, subtracted_partial_set, by = 0
        )
        # The merge makes the row names into their own col. This returns them
        # to actual row names.
        rownames(subtracted_expression_set) <- subtracted_expression_set$Row.names
        subtracted_expression_set$Row.names <- NULL
      }
      log$info("Finished pairing dataset.")
      # We put the new data in the same containers
      sub_expression_set <- subtracted_expression_set
      # The class labels are now identical as the set is paired
      rp_class_labels <- rep(1, ncol(subtracted_expression_set))
    }

    # Handle batches
    if (!is.null(batches)) {
      log$info("Setting batches...")
      if (length(batches) != ncol(sub_expression_set)) {
        stop("Length of batches vector is not the same as the groups.")
      }
      if (any(table(paste(batches, groups)) == 1)) {
        log$warn("Some batches have only one sample per group. Rankprod cannot correct such a batch effect")
        rp_origin <- rep(1, length(groups))
      }
      batches |> as.factor() |> as.numeric() -> rp_origin
    } else {
      rp_origin <- rep(1, ncol(sub_expression_set))
    }

    # invisible(capture.output()) is to suppress automatic output to console
    # WARNING: therein <- (instead of =) is mandatory for assignment!
    log$info("Running RankProduct...")
    RP.out <- RP.advance(
      sub_expression_set,
      rp_class_labels, origin = rp_origin,
      gene.names = rownames(sub_expression_set),
      logged = TRUE, na.rm = FALSE, plot = FALSE, rand = 123
    )

    # Compute full DEG Tables (returns a list of 2 matrices, not data frames)
    log$info("Computing DEG table...")
    invisible(
      capture.output(
        # The Inf cutoff is to print all genes
        DEGs.RP <- topGene(RP.out, logged = TRUE, logbase = 2, cutoff = Inf)
      )
    )

    # Ameliorate original output
    for (j in 1:2) {
      # Take the log2 values and invert FCs to get the 'Case vs Ctrl' measure
      # Doing so, DEGs.RP[[1]] are the up-DEGs and DEGs.RP[[2]] are the downs
      DEGs.RP[[j]][,3] = -log2(DEGs.RP[[j]][,3])
      colnames(DEGs.RP[[j]])[3] = "logFC" # Change column name
    }

    # Condense the data from the two matrices into a single dataframe to
    # be stored in the output
    partial_data_up <- as.data.frame(DEGs.RP[[1]])
    partial_data_dw <- as.data.frame(DEGs.RP[[2]])

    # The colname order is always known
    colnames(partial_data_up) <- c(
      "gene.index", "RP/Rsum.UP", "logFC", "pfp.UP", "P.value.UP"
    )
    colnames(partial_data_dw) <- c(
      "gene.index", "RP/Rsum.DOWN", "logFC", "pfp.DOWN", "P.value.DOWN"
    )

    partial_data <- merge(
      partial_data_up, partial_data_dw, by = c("row.names", "gene.index", "logFC")
    )
    # Merging by multiple columns including logFC can be risky if small (order
    # of magnitude 1e-8) random differences between up- and down- DEG tables are
    # introduced during topGene() calculation. In this case we would loose those
    # genes when merging... it's an unlucky event, but better safe than sorry.
    if (nrow(partial_data) != nrow(expression_set)) {
      log$warn(
        paste(
          nrow(expression_set) - nrow(partial_data),
          "entries have been lost in merging partial_data_up with _down!"
        )
      )
    }
    # Restore the rownames to actual rownames
    rownames(partial_data) <- partial_data$Row.names
    partial_data$Row.names <- NULL

    # We add the "markings" column, with 1 for upregulated, 0 for not significant
    # and -1 for downregulated DEGs.
    # Initialize the array
    markings <- rep(0, length(partial_data$gene.index))

    markings[
      partial_data$logFC > fc_threshold &
        partial_data$pfp.UP < 0.05
    ] <- 1
    markings[
      partial_data$logFC < -fc_threshold &
        partial_data$pfp.DOWN < 0.05
    ] <- -1
    partial_data$markings <- markings

    # We add the AverageExpression column to all the DEG lists to be consistent
    # with the output from limma. The averages are computed overall, not
    # groupwise.
    # This is a left outer join, dropping averages as needed.
    averages <- data.frame("AveExpr" = apply(expression_set, 1, mean))
    partial_data <- merge(
      partial_data, averages, by = "row.names",
      all.x = TRUE
    )
    # Restore the rownames to actual rownames
    rownames(partial_data) <- partial_data$Row.names
    partial_data$Row.names <- NULL

    # Reorder the cols with the magic of dplyr
    partial_data |>
      dp_select(gene.index, logFC, AveExpr, everything()) ->
      partial_data

    # Put the data in the list
    DEGs.rankprod[[contrasts[i]]] <- partial_data

  } # Hard to see but here ends the loop on all the contrasts

  return(DEGs.rankprod)
}


#' Make and save (in the current WD) diagnostic plots for rankprod results.
#'
#' This prints some plots with `printPlots`, and assumes that the correct
#' options have already been selected.
#'
#' Looks for a column named "SYMBOL" to source the volcano plot labels with,
#' else uses the rownames.
#'
#' The plots include: A Venn diagram OR an upset plot of the detected DEGs
#' (if there are more than 3 contrasts), a set of MA plots with the detected
#' DEGs highlighted, and a set of volcano plots with the (annotated) DEGs.
#'
#' @param DEGs.rankprod A list of DEG tables such as that produced by
#'   `run_rankprod`.
#' @param fc_threshold The fold change threshold that was used in the
#'   `run_rankprod` call.
#' @param `volcano_colour` The colour of the significant DEGs in the volcano
#'   plot.
diagnose_rankprod_data <- function(
  DEGs.rankprod, fc_threshold = 0.5, volcano_colour = "firebrick3"
) {
  log$info("Making rankprod DEG plots...")

  extract_markings(DEGs.rankprod) |> abs() -> markings

  make_overlaps_from_markings(markings, toolname = "RankProd")

  # MA-Plots with significant DEGs
  # Find Axis Limits
  log$info("Finding axis limits...")
  max.M.value = max(sapply(DEGs.rankprod, \(data){max(abs(data$logFC))} ))
  max.A.value = max(sapply(DEGs.rankprod, \(data){max(data$AveExpr)} ))
  min.A.value = min(sapply(DEGs.rankprod, \(data){min(data$AveExpr)} ))
  min.P.value = min(sapply(DEGs.rankprod, \(data){min(c(data$P.value.DOWN, data$P.value.UP))} ))

  # MA-Plot with DEGs
  log$info("Making Rankprod MA-Plots...")
  # I cannot place a progress bar here as `printPlots` logs to stdout.
  for (i in seq_along(DEGs.rankprod)) {
    # Mark in red/blue all the up-/down- regulated genes
    # This MA plot is made with the LogFC and estimates from the TopTables
    printPlots(
      \() {
        mamaplot(
          x = DEGs.rankprod[[i]]$AveExpr, y = DEGs.rankprod[[i]]$logFC,
          input_is_ma = TRUE,
          highligths = list(
            "red" = DEGs.rankprod[[i]]$markings == 1,
            "blue" = DEGs.rankprod[[i]]$markings == -1
          ),
          xrange = c(min.A.value, max.A.value),
          yrange = c(NA, max.M.value),
          title = paste0("MA plot with RP DEGs: ", names(DEGs.rankprod)[i])
        ) |> print()
      },
      paste("MA-Plot with RankProd DEGs ", names(DEGs.rankprod)[i], sep = "")
    )
  }

  # Volcano Plots
  log$info("Making RankProd Volcano plots...")
  for (i in seq_along(DEGs.rankprod)) {
    # Enhanced Volcano Plot
    if ("SYMBOL" %in% colnames(DEGs.rankprod[[i]])) {
      volcano_labels <- DEGs.rankprod[[i]]$SYMBOL
    } else {
      volcano_labels = rownames(DEGs.rankprod[[i]])
    }

    # We need to harmonize the p-values, as we have two lists (P.value.UP and
    # P.value.DOWN). I choose to keep the P.value.UP for genes with positive
    # LogFC, and P.value.DOWN for negative genes.
    harm_p_vals <- rep(NA, length(DEGs.rankprod[[i]]))
    harm_p_vals[DEGs.rankprod[[i]]$logFC > 0] <-
      DEGs.rankprod[[i]]$P.value.UP[DEGs.rankprod[[i]]$logFC > 0]
    harm_p_vals[DEGs.rankprod[[i]]$logFC <= 0] <-
      DEGs.rankprod[[i]]$P.value.DOWN[DEGs.rankprod[[i]]$logFC <= 0]
    DEGs.rankprod[[i]]$harmonized_p_value <- harm_p_vals

    # I do the same for the adj.p.values to find the critical p-value threshold
    harm_adj_p_vals <- rep(NA, length(DEGs.rankprod[[i]]))
    harm_adj_p_vals[DEGs.rankprod[[i]]$logFC > 0] <-
      DEGs.rankprod[[i]]$pfp.UP[DEGs.rankprod[[i]]$logFC > 0]
    harm_adj_p_vals[DEGs.rankprod[[i]]$logFC <= 0] <-
      DEGs.rankprod[[i]]$pfp.DOWN[DEGs.rankprod[[i]]$logFC <= 0]

    volcano_p_threshold <- find_BH_critical_p(harm_adj_p_vals)

    get_h_volcano <- function() {
      # NOTICE: When in a for loop, you have to explicitly print your
      # resulting EnhancedVolcano object
      suppressWarnings(
        # This prints warnings as - I think - the internal implementation
        # uses xlim and ylim but ggplot2 ignores them.
        print(
          EnhancedVolcano(
            DEGs.rankprod[[i]],
            x = "logFC", y = "harmonized_p_value",
            pCutoff = volcano_p_threshold, FCcutoff = fc_threshold,
            pointSize = 1,
            col = c("black", "black", "black", volcano_colour),
            lab = volcano_labels,
            labSize = 4,
            title = names(DEGs.rankprod)[i],
            subtitle = "RankProd",
            legendPosition = "none"
          )
        )
      )
    }
    printPlots(get_h_volcano, paste("Volcano with RankProd DEGs ", names(DEGs.rankprod)[i], sep = ""))
  }
}


bioTEA <- function(
  input.file,
  output.dir,
  experimental_design,
  contrasts,
  min_log2_expression = 4,
  fc_threshold = 0.5,
  min_groupwise_presence = 0.8,
  slowmode = FALSE,
  show_data_snippets = TRUE,
  annotation_database = TRUE,
  dryrun = FALSE,
  renormalize = FALSE,
  convert_counts = FALSE,
  run_limma_analysis = TRUE,
  run_rankprod_analysis = TRUE,
  batches = NA,
  extra_limma_vars = NA,
  group_colors = c("cornflowerblue", "firebrick3", "olivedrab3", "darkgoldenrod1", "purple", "magenta3")
) {
  # This function is so long, a description wouldn't fit here.
  # Refer to the project's README.

  set.seed(1) # Rank Prod needs randomness.

  # ---- Making static functions ----
  pitstop <- pitstop.maker(slowmode)
  printdata <- printif.maker(show_data_snippets, topleft.head)

  log$info("Parsed options.")

  # ---- Move to output.dir ----
  setwd(output.dir)

  # ---- Variable setup ----
  # To reuse as-is the script, I unpack the variables from the yaml file here
  # TODO : Improvement - These should be checked here for basic validity, so
  #        we crash sooner rather than later.
  log$info("Inputting variables...")

  # Flags for script-wide IFs
  write_data_to_disk = !dryrun # The ! is important.
  if (dryrun) {
    log$warn("Suppressing printing of plots, as this is a dryrun.")
    options(
      save.PNG.plot = FALSE,
      save.PDF.plot = FALSE
    )
  }

  if (annotation_database %in% c(TRUE, FALSE)) {
    use.annotations <- annotation_database
    use.remote.annotations <- FALSE
    database_name <- NA
  } else {
    use.annotations <- TRUE
    use.remote.annotations <- TRUE
    database_name <- annotation_database
  }
  log$debug("use.annotations was set to", str(use.annotations))
  log$debug("use.remote.annotations was set to", str(use.remote.annotations))
  log$debug("database_name was set to", str(database_name))

  if (use.annotations & use.remote.annotations) {
    log$info("Loading remote annotations...")
    # I source the data like this as we can use the `merge_annotations`
    # function which is faster than `annotate_data` if used many times in a row
    annotation_data <- get_remote_annotations(
      database_name, selections = c("SYMBOL", "GENENAME", "ENSEMBL")
    )
    annotation_data$package_name <- database_name
    annotation_data$version <- packageVersion(database_name)
  } else if (use.annotations) {
    log$info("Loading local annotations...")
    load(file = "/bioTEA/modules/annotation/resources/full_annotations.RData")

    if (! "full_annotations" %in% ls()) {
      stop("I loaded something, but it did not contain the 'full_annotations' object.")
    }
    annotation_data <- full_annotations
    rm(full_annotations)
  }

  # Global options that bioTEA reads while running
  options(use.annotations = use.annotations)

  # ---- Data Loading ----
  # Gene Expression Matrix - log2-Intensity-Values
  log$info("Loading data...")

  expression_set <- read_expression_data(input.file)
  printdata(expression_set)
  log_data(expression_set, "Raw expression set")

  pitstop("Finished loading data.")

  # Load the experimental design
  log$info("Loading experimental design...")
  # This is a list containing the group sequence in `$groups` and the IDs for
  # pairing in '$pairing'
  design_parser(experimental_design) |>
    split_design() ->
    experimental_design

  # Check the design for validity
  if (length(experimental_design$groups) != ncol(expression_set)) {
    stop(paste0(
      "The number of samples (", ncol(expression_set),
      ") does not match the number of design groups (",
      length(experimental_design$groups), ")"
    ))
  }

  # Test if we are in paired or unpaired mode
  ..pairing_NAs <- is.na(experimental_design$pairings)
  # We need either ALL or NONE patient NAs
  if (all(..pairing_NAs)) {
    log$info("No sample pairing detected. Running in unpaired mode.")
    paired_mode <- FALSE
  } else if (all(!..pairing_NAs)) {
    log$info("Found sample pairings. Running in paired mode.")
    paired_mode <- TRUE
  } else {
    stop("Some samples have pairing data and some do not. Cannot proceed with pairing ambiguity.")
  }

  # Expand and test batch variable
  if (!is.na(batches)) {
    batches <- design_parser(batches)
    if (length(batches) != ncol(expression_set)) {
      stop(paste0(
        "The number of samples (", ncol(expression_set),
        ") does not match the number of batches (", length(batches), ")"
      ))
    }
  } else {
    batches <- NULL
  }

  # The same for all extra variables
  if (!is.na(extra_limma_vars)) {
    extra_limma_vars <- lapply(
      extra_limma_vars, design_parser, ignore_asterisk = TRUE
    )
    length_check <- lapply(extra_limma_vars, length) == ncol(expression_set)
    if (! all(length_check)) {
      stop(paste0(
        "Some extra limma variables are not the same length as the number of",
        " samples. The culprit(s) are list(s) number ",
        paste(which(length_check == FALSE), collapse = ", "),
        "."
      ))
    }
  } else {
    extra_limma_vars <- NULL
  }

  log$info("Experimental design loaded.")

  # Tidy Sample Names According to the Experimental Design
  # Create a new vector containing tidy group names
  unique_simple_groups <- unique(experimental_design$groups)
  log$info(paste(length(unique_simple_groups), "groups detected:",
                 paste0(unique_simple_groups, collapse = ", ")))
  log$info("Making group names tidy using the experimental design...")
  unique_groups <- make.unique_from1(experimental_design$groups, sep = "_")
  ..old_colnames <- colnames(expression_set)
  colnames(expression_set) <- unique_groups

  if (length(group_colors) < length(unique_simple_groups)) {
    stop("Too few colors in \'group_colors\' vector!")
  }
  # Bind colors to groups
  names(group_colors) <- unique_simple_groups

  # Save Correspondences Table so we can check it later
  ..corrTable = cbind(..old_colnames, colnames(expression_set)) # Cast to matrix
  colnames(..corrTable) = c("Original_ID", "Group_Name")
  printdata(..corrTable)
  log_data(..corrTable, "Correspondence Table", FALSE)

  if (write_data_to_disk) {
    corr_table_out <- file.path(output.dir, "correspondence_table.csv")
    write.csv(..corrTable, corr_table_out, row.names = FALSE, quote = TRUE)
    register_for_ownership(corr_table_out)
    log$info(paste("'correspondence_table.csv' has been saved in", output.dir))
  }

  # Check the contrasts
  raw_contrasts <- contrasts

  unpack_contrasts <- function(contrasts_list) {
    lapply(contrasts_list, strsplit, split = "-") |>
      lapply(unlist) ->
      unpacked
    return(unpacked)
  }

  raw_contrasts |> unpack_contrasts() -> group_contrasts

  # Check if the contrasts groups are actually in the data
  ..check <- unlist(group_contrasts) %in% unique_simple_groups
  if (!all(..check)) {
    stop(paste0(
      "Some groups defined in the contrasts are not present in the data. ",
      "Conflicting groups: ", paste0(group_contrasts[!..check])
    ))
  }

  log$info("Design loaded and approved.")
  pitstop("")

  # ---- Generate and log the limma design ----
  log$info("Making design matrix...")
  additional_limma_vars <- c(
      if (paired_mode) {list(pairings = experimental_design$pairings)} else {NULL},
      if (!is.null(batches)) {list(batches = batches)} else {NULL},
      if (! is.null(extra_limma_vars)) {extra_limma_vars} else {NULL}
    )

  limma_design_matrix <- if (is.null(additional_limma_vars)) {
    make_limma_design(groups = experimental_design$groups)
  } else {
    do.call(make_limma_design, c(list(groups = experimental_design$groups), additional_limma_vars))
  }

  # ---- Count transformation with voom ----
  if (convert_counts) {
    log$info("Converting counts to continuous values.")
    normalization_factor <- edgeR::calcNormFactors(
      expression_set, method = "TMM"
    )
    voomed_exprs <- voom(
      expression_set,
      design = limma_design_matrix,
      lib.size = colSums(expression_set) * normalization_factor
    )
    expression_set <- as.data.frame(voomed_exprs$E)
    log_data(expression_set, "Continuous expression from counts")
    rm(voomed_exprs)
  }


  # ---- Normalization ----
  # After-RMA 2nd Quantile Normalization
  if (renormalize) {
    # Print boxplot before normalization
    printPlots(\() {
      boxplot(
        expression_set, las = 2, col = group_colors,
        main = "Expression values per sample", ylab = "log2 (intensity)"
      )
    }, "Pre-normalization boxpot")
    printPlots(\() {
      plotDensities(
        expression_set, legend = FALSE, main = "Expression values per sample"
      )
    }, "Pre-normalization density")

    log$info("Running quantile-quantile normalization...")
    expression_set <- qq_normalize(expression_set)
    log_data(expression_set, "Post-normalization data")
  }


  # ---- MA-Plot & Box-Plot ----
  # Normalization Final Check with Figure Production
  printPlots(\() {
    boxplot(
      expression_set,
      las = 2, col = group_colors[experimental_design$groups],
      main = "Expression values per sample", ylab = "log2 (intensity)"
      )
    },
    "Final Boxplot"
  )
  printPlots(\() {
    plotDensities(expression_set, legend = FALSE, main = "Expression values per sample")
  }, "Final Density")

  pitstop("Maybe check the plots and come back?")

  # MA-Plot for bias detection - Make all the possible group pairs
  for (combo in combn(unique_simple_groups, 2, simplify = FALSE)) {
    expression_set |> dp_select(starts_with(combo[1])) |> rowMeans() -> group1
    expression_set |> dp_select(starts_with(combo[2])) |> rowMeans() -> group2

    matitle <- paste0("MA_Plot_", combo[[1]], "_vs_", combo[[2]])
    printPlots(\(){
      suppressMessages(print(mamaplot(group1, group2, title = matitle)))
    }, matitle)
  }


  # ---- Clustering ----
  diagnose_batch_effects(
    expression_set, experimental_design$groups, group_colors,
    title_mod = "original"
  )

  if (!is.null(batches)) {
    log$info("Correcting batch effects for visualization...")
    batch_corrected_expr_set <- removeBatchEffect(
      expression_set, batch = batches
    )
    log$info("Diagnosing batch effects again...")
    diagnose_batch_effects(
      batch_corrected_expr_set, experimental_design$groups, group_colors,
      title_mod = "Unbatched"
    )
  }
  pitstop("The PCA and cluster plots should show no obvious cluster.")

  # ---- SD vs Mean Plot ----
  # Poisson Hypothesis Check
  log$info("Building SD_vs_Mean plot...")

  # Un-log intensity values
  log$info("Returning to linear intensities...")
  unlogged_expression_set = 2 ^ expression_set
  printdata(unlogged_expression_set)
  log_data(unlogged_expression_set, "Unlogged expression set")
  pitstop("Did the 'unlogging' mess anything up?")

  # Store values using matrices
  log$info("Making matrices...")
  ..nr_unique_simple_groups <- length(unique_simple_groups)

  matrix(nrow = nrow(expression_set), ncol = ..nr_unique_simple_groups + 1) |>
    as.data.frame() -> ..means_frame
  colnames(..means_frame) <- c(unique_simple_groups, "Global")

  # Make a copy in memory
  ..sd_matrix <- data.frame(..means_frame)

  ..correlation_vector <- as.vector(rep(NA, length(unique_simple_groups) + 1))
  names(..correlation_vector) <- c(unique_simple_groups, "Global")

  # Statistics for each group...
  log$info("Calculating groupwise statistics...")
  for (group in unique_simple_groups) {
    unlogged_expression_set |> dp_select(starts_with(group)) |>
      rowMeans(na.rm = TRUE) ->
      ..means_frame[[group]]

    unlogged_expression_set |> dp_select(starts_with(group)) |>
      apply(1, sd, na.rm = TRUE) ->
      ..sd_matrix[[group]]

    cor(..means_frame[[group]], ..sd_matrix[[group]]) ->
      ..correlation_vector[[group]]
  }

  # ...and for the whole experiment
  log$info("Calculating global statistics...")

  ..means_frame[["Global"]] = rowMeans(unlogged_expression_set, na.rm = TRUE)
  ..sd_matrix[["Global"]] = apply(unlogged_expression_set, 1, sd, na.rm = TRUE)
  ..correlation_vector[["Global"]] = cor(..means_frame[,"Global"], ..sd_matrix[,"Global"])

  # Scatter plot
  log$info("Making plots...")
  printPlots(
    \() {
      par(mfrow = c(1, length(unique_simple_groups)+1))
      X.max = max(..means_frame, na.rm = TRUE)
      Y.max = max(..sd_matrix, na.rm = TRUE)
      for (group in c(unique_simple_groups, "Global")) {
        plot(..means_frame[[group]], ..sd_matrix[[group]],
             xlab = "Mean", ylab = "SD",
             xlim = c(0, X.max), ylim = c(0, Y.max),
             pch = 20, cex = 0.5)
        title(main = group)
        mtext(side = 3, paste("Corr =", toString(round(..correlation_vector[[group]], digits = 5))))
      }
      par(mfrow = c(1, 1))
    },
    "SD_vs_Mean Plot"
  )
  log_data(..correlation_vector, "Poisson Correlation Vector", FALSE)

  # ---- Filtering ----
  expression_set <- filter_expression_data(
    expression_set, groups = experimental_design$groups,
    min_groupwise_presence = min_groupwise_presence,
    expression_threshold = min_log2_expression
  )

  log$info("Done filtering.")

  # ---- DE by Limma ----
  if (run_limma_analysis) {
    DEGs.limma <- run_limma(
      expression_set, design = limma_design_matrix, contrasts = raw_contrasts,
      fc_threshold = fc_threshold
    )

    if (getOption("use.annotations")) {
      log$info("Annotating limma results...")
      for (i in seq_along(DEGs.limma)) {
        DEGs.limma[[i]] <- merge_annotations(DEGs.limma[[i]], annotation_data)
      }
    }

    # Make diagnostic plots for Limma
    diagnose_limma_data(
      DEGs.limma = DEGs.limma, fc_threshold = fc_threshold,
      volcano_colour = group_colors[2]
    )

    # Save full DEG Tables
    if (write_data_to_disk) {
      log$info("Saving Differential expression tables...")
      pb <- progress_bar$new(
        format = "Saving... [:bar] :percent (:eta)",
        total = length(raw_contrasts), clear = FALSE, width= 80)
      pb$tick(0)
      for (i in seq_along(raw_contrasts)) {
        write_expression_data(
          DEGs.limma[[i]], paste0("Limma - DEG Table ", raw_contrasts[i], ".csv"))
        pb$tick()
      }
    }
  }

  # ---- DE by RankProduct ----
  if (run_rankprod_analysis) {
    DEGs.rankprod <- run_rankprod(
      expression_set, groups = experimental_design$groups, contrasts = raw_contrasts,
      batches = batches,
      pairings = if (paired_mode) {experimental_design$pairings} else {NULL},
      fc_threshold = fc_threshold
    )

    if (getOption("use.annotations")) {
      log$info("Annotating RankProd results...")
      for (i in seq_along(DEGs.rankprod)) {
        DEGs.rankprod[[i]] <- merge_annotations(DEGs.rankprod[[i]], annotation_data)
      }
    }

    # Make diagnostic plots for RankProd
    diagnose_rankprod_data(
      DEGs.rankprod = DEGs.rankprod, fc_threshold = fc_threshold,
      volcano_colour = group_colors[2]
    )

    # Save full DEG Tables
    if (write_data_to_disk) {
      log$info("Saving Differential expression tables...")
      pb <- progress_bar$new(
        format = "Saving... [:bar] :percent (:eta)",
        total = length(raw_contrasts), clear = FALSE, width= 80)
      pb$tick(0)
      for (i in seq_along(raw_contrasts)) {
        write_expression_data(
          DEGs.rankprod[[i]], paste0("RankProd - DEG Table ", raw_contrasts[i], ".csv"))
        pb$tick()
      }
    }
  }

  # ---- Comparison Plots - Limma vs RankProd ----
  if (run_limma_analysis & run_rankprod_analysis) {
    log$info("Making comparison plots between limma and rankproduct...")

    # To suppress 'venn.diagram()' logging
    futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

    results.limma <- extract_markings(DEGs.limma)
    results.RP <- extract_markings(DEGs.rankprod)

    # Plot Venn diagrams
    log$info("Plotting Venn diagrams...")
    for (i in seq_along(raw_contrasts)) {
      for (j in c(1, -1)) {

        DEG.id.limma = rownames(results.limma)[which(results.limma[,i] == j)]
        DEG.id.RP = rownames(results.RP)[which(results.RP[,i] == j)]

        if (j == 1) {
          venn.sub = "UP-regulated DEGs"
        } else {
          venn.sub = "DOWN-regulated DEGs"
        }

        if (length(DEG.id.limma) == 0 & length(DEG.id.RP) == 0) {
          log$warn("Both the sets are empty in the contrast: ", raw_contrasts[i], " (", venn.sub, ")")
          next # Skip the current iteration of the for loop without terminating it
        }

        venn.plot = venn.diagram(
          x = list(DEG.id.limma, DEG.id.RP),
          filename = NULL, # to print just on screen
          force.unique = TRUE,
          main = raw_contrasts[i], main.cex = 2, main.fontface = "bold", main.fontfamily = "sans", # Title
          sub = venn.sub, sub.fontfamily = "sans", # Subtitle
          lwd = 2, lty = "blank", fill = group_colors[1:2], # circles
          cex = 2, fontface = "bold", fontfamily = "sans", # numbers
          category.names = c("Limma", "Rank Product"), # names
          cat.cex = 2,
          cat.fontface = "bold",
          cat.default.pos = "outer",
          cat.pos = c(-150, 150),
          cat.dist = c(0.055, 0.055),
          cat.fontfamily = "sans"
        )

        # Create a new canvas and draw the Venn
        printPlots(
          \() {grid.newpage(); grid.draw(venn.plot)},
          paste0(
            "Comparison Venn ", raw_contrasts[i], "_",
            strsplit(venn.sub, split = "-")[[1]][1]
          )
        )
      }
    }
  }

  log$info("bioTEA finished")
}
