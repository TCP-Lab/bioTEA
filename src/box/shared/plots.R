# Header Info ------------------------------------------------------------------
#
# This file hosts plotting tools used throughout the module.
#
# ------------------------------------------------------------------------------

..PRINTPLOT_COUNTER = 1

#' Save a graphical output to '<folderPrefix> Figures' sub-directory.
#'
#' Automatically makes the output folder if not there.
#'
#' @param plotfun A function that resolves to printing a plot to an open
#'   device. This takes a function and not a plot object as some plotting
#'   facilities (notably the default one) cannot print plot objects conveniently.
#' @param figureName name of the output file (without extension)
#' @param folderPrefix for naming the saving subfolder
#'   (defaults to `scriptName` option)
#' @param PNG If true, print the plot to PNG format. Defaults to `save.PNG.plot`
#'   option or TRUE.
#' @param PDF If true, print the plot to PDF format. Defaults to `save.PDF.plot`
#'   option or TRUE.
#' @param plot.width Width of the plot in inches. Defaults to `plot.width` option or 16.
#' @param plot.height Height of the plot in inches. Defaults to `plot.height` option or 9.
#' @param png.ppi The resolution of the png plots in pixels per inch. Defaults
#'   to `png_ppi` option or 50.
#' @param enumerate_plots If true, plots will be enumerated (a progressive
#'   number is inserted at the start of their filename). Defaults to the
#'   `enumerate.plots` option or FALSE.
#'
#' @author FeAR, Hedmad
printPlots = function(
  plotfun,
  figureName,
  folderPrefix = getOption("scriptName", ""),
  PNG = getOption("save.PNG.plot", TRUE), PDF = getOption("save.PDF.plot", TRUE),
  plot.width = getOption("plot.width", 16), plot.height = getOption("plot.height", 9),
  png_ppi = getOption("png_ppi", 400),
  enumerate = getOption("enumerate.plots", FALSE)
) {

  if (enumerate) {
    figureName <- paste0(..PRINTPLOT_COUNTER, "_", figureName)
    ..PRINTPLOT_COUNTER <<- ..PRINTPLOT_COUNTER + 1 # The <<- is important
  }

  figSubFolder = paste0("/bioTEA/target/", folderPrefix, " Figures")
  fullName = file.path(figSubFolder, figureName)

  if (!file.exists(figSubFolder) && (PNG || PDF)) {
    dir.create(figSubFolder)
    register_for_ownership(figSubFolder)
    log$warn("New folder '", figSubFolder, "' has been created in the current WD", sep = "")
  }

  if (PNG) {
    target_path <- paste0(fullName, ".png")
    log$debug(paste0("Saving '", figureName, ".png'..."))
    png(
      file = target_path,
      width = (plot.width), height = (plot.height),
      res = png_ppi
    )
    plotfun()
    dev.off()
    register_for_ownership(target_path)
  }

  if (PDF) {
    target_path <- paste0(fullName, ".pdf")
    log$debug(paste0("Saving '", figureName, "'.pdf..."))
    pdf(
      file = target_path,
      width = plot.width, height = plot.height
    )
    plotfun()
    dev.off()
    register_for_ownership(target_path)
  }
}


#' Print a MA plot. A bit better than other implementations.
#'
#' The "M" parameter is calculated as `m <- x - y`. The "A" parameter as
#' `a <- (x + y) / 2`.
#'
#' @param x A vector of positive numeric values.
#' @param y A vector of positive numeric values.
#' @param .xlab The label to use for the X ("A") axis.
#' @param .ylab The label to use for the Y ("M") axis.
#' @param show_trend Bool. Add a trend line through the data?
#' @param show_density Bool. Add a density blob on the data?
#' @param density_palette A palette (see `?scale_fill_distiller`) for the
#'   density blob.
#' @param xrange Range of X to zoom into. Passed as `c(min, max)`.
#'   Leave `NA` to use default.
#' @param yrange Range of Y to zoom into. Passed as `c(min, max)`.
#'   Leave `NA` to use default.
#' @param highligths A named list of boolean vectors. Each entry will be matched
#'   with the points in the data, and the TRUE points will be marked in the
#'   color specified by the name of the vector.
#' @param input_is_ma If TRUE, then X is treated as the A value, and Y is treated
#'   as the M value, and they are not calculated by this function.
#'
#' @author Hedmad
mamaplot <- function(
  x, y,
  .xlab = "A", .ylab = "M",
  title = "MA Plot",
  show_trend = TRUE,
  show_density = TRUE,
  density_palette = "RdBu",
  xrange = c(NA, NA),
  yrange = c(NA, NA),
  highligths = NULL,
  input_is_ma = FALSE
) {
  library(ggplot2)

  stopifnot("x and y lengths differ" = {length(x) == length(y)})

  if (input_is_ma) {
    m <- y
    a <- x
  } else {
    m <- x - y
    a <- (x + y) / 2
  }

  data <- data.frame(m = m, a = a)
  p <- ggplot(data, aes(x = a, y = m)) +
    geom_point(color = "black", size = 0.1)

  if (show_density) {
    p <- p +
      stat_density_2d(
        geom = "polygon", contour = TRUE,
        aes(fill = after_stat(level)),
        bins = 5,
        alpha = 0.5,
        breaks = c(0.01, 0.05, 0.1, 0.15, 0.2),
        show.legend = FALSE
      ) +
      scale_fill_distiller(palette = density_palette, direction = -1)
  }
  if (show_trend) {
    p <- p +
      geom_smooth(method = "gam", colour = "red", size = 0.7, se = FALSE)
  }
  p <- p +
    ggtitle(title) +
    xlab(.xlab) + ylab(.ylab) +
    theme_bw()

  p <- p +
    coord_cartesian(xlim = xrange, ylim = yrange)

  if (! is.null(highligths)) {
    for (i in seq_along(highligths)) {
      if (all(!highligths[[i]])) {
        # All the entries are FALSE, so we move on
        next
      }

      data_subset <- subset(data, highligths[[i]])
      color <- names(highligths[i])
      p <- p +
        geom_point(
          shape = 18,
          data = data_subset, color = color, size = 3,
          show.legend = FALSE
        )
    }
  }

  return(p)
}


#' Get axis limits suitable for ma plots.
#'
#' @param x The x data frame.
#' @param y The y vector. If NULL, uses the median from `x[-i]`.
#'
#' @returns A list with "xrange" and "yrange" settings.
..get_mama_axis_limits <- function(x, y) {
  stopifnot({
    is.data.frame(x)
    is.vector(y) | is.null(y)
  })

  all_ms <- list(rep(NA, length(x)))
  all_as <- list(rep(NA, length(x)))
  for (i in seq_along(x)) {
    if (is.null(y)) {
      y <- get_median(x[-i])
    }
    all_ms[i] <- y - x[i]
    all_as[i] <- (x[i] + y) / 2
  }

  all_as <- unlist(all_as)
  all_ms <- unlist(all_ms)

  xrange <- c(min(all_as), max(all_as))
  yrange <- c(min(all_ms), max(all_ms))
  return(list("xrange" = xrange, "yrange" = yrange))
}


#' Produce (better) MA plots from the data. Save them with printPlots.
#'
#' The `enumerate` option from printPlots is always on. This prevents accidental
#' name collisions.
#'
#' The plots are returned in order, from worst to best.
#'
#' The x and y input values can fall into one of three cases:
#'   - if `x` is a vector and `y` is a vector, the MA plot of those two vectors
#'     is generated.
#'   - if `x` is a data.frame and `y` is `NULL`, a list of MA plots is returned,
#'     one for each column of `x` showing that column versus the median of the
#'     remaining data.
#'   - if `x` is a data.frame and `y` is a vector, a list of MA plots is
#'     returned, one for each column of `x`, showing that column versus `y`.
#'
#' @param x Either a vector (if y is a vector) or a data.frame of numeric values.
#' @param y Either `NULL` (if x is a data.frame) or a vector of numeric values.
#' @param xlab The label of the X axis for all plots.
#' @param ylab The label of the Y axis for all plots.
#' @param title The title of the plots. If x is a data.frame, the string `"{x}"`
#'   in the title of the plot will be replaced with the colname used to generate
#'   the plot. Similarly, the string `"{y}"` will be replaced with a correct
#'   value for the situation.
#' @param show_trend Bool. Add a trend line through the data?
#' @param show_density Bool. Add a density blob on the data?
#' @param density_palette A palette (see `?scale_fill_distiller`) for the density blob.
#'
#' @author Hedmad
get_better_mas <- function(
  x, y = NULL,
  xlab = "A", ylab = "M",
  title = "MA plot - {x} vs {y}",
  show_trend = TRUE,
  show_density = TRUE,
  density_palette = "RdBu"
) {
  log$info("Making MA plots...")
  format_title <- function(template, x, y) {
    library(stringr)
    x <- path_sanitize(x)
    y <- path_sanitize(y)
    str_replace_all(template, "\\{x\\}", x) |>
      str_replace_all("\\{y\\}", y) -> result
    return(result)
  }

  if (is.data.frame(x)) {plot_order <- rep(NA, length(x))}

  if (is.data.frame(x) & ! is.null(y)) {
    log$info("Plotting cols of x vs vector y...")
    # We need to test all cols of x vs y.
    results = list(rep(NA, length(x)))
    ranges <- ..get_mama_axis_limits(x, y)

    pb <- progress_bar$new(
      format = "Generating... [:bar] :percent (:eta)",
      total = length(x), clear = FALSE, width= 80)
    pb$tick(0)
    for (i in seq_along(x)) {
      # There is code duplication here, but this title line here is different
      # so I don't clean it up to avoid adding useless boilerplate
      # It doesn't work without `unlist`, even when using `[[1]]
      formatted_title <- format_title(title, colnames(x[i]), "Y")
      results[[i]] <- mamaplot(
        x = unlist(x[i]), y = y,
        .xlab = xlab, .ylab = ylab,
        title = formatted_title,
        show_trend = show_trend,
        show_density = show_density,
        density_palette = density_palette,
        xrange = ranges$xrange,
        yrange = ranges$yrange
      )
      plot_order[i] <- get_mamaplot_score(m = (y - unlist(x[i])), a = ((unlist(x[i]) + y) / 2))
      pb$tick()
    }
    return(results[order(plot_order, decreasing = TRUE)])
  } else if (is.data.frame(x) & is.null(y)) {
    log$info("Plotting x with subsets of itself...")
    # We need to test all cols by the median of the remaining ones.
    results = list(rep(NA, length(x)))
    ranges <- ..get_mama_axis_limits(x, y)

    pb <- progress_bar$new(
      format = "Generating... [:bar] :percent (:eta)",
      total = length(x), clear = FALSE, width= 80)
    pb$tick(0)
    for (i in seq_along(x)) {
      y <- get_median(x[-i])$Median

      formatted_title <- format_title(title, colnames(x[i]), "Median of Remaining cols")
      # It doesn't work without `unlist`, even when using `[[1]]`
      results[[i]] <- mamaplot(
        x = unlist(x[i]), y = y,
        .xlab = xlab, .ylab = ylab,
        title = formatted_title,
        show_trend = show_trend,
        show_density = show_density,
        density_palette = density_palette,
        xrange = ranges$xrange,
        yrange = ranges$yrange
      )
      plot_order[i] <- get_mamaplot_score(m = (y - unlist(x[i])), a = ((unlist(x[i]) + y) / 2))
      pb$tick()
    }
    return(results[order(plot_order, decreasing = TRUE)])
  } else if (is.vector(x) & is.vector(y)) {
    log$info("Plotting simple x vs y...")
    # We make a MA plot with the two vectors as is.
    formatted_title <- format_title(title, "X", "Y")
    p <- function() {
      return(mamaplot(
        x = x, y = y,
        .xlab = xlab, .ylab = ylab,
        title = formatted_title,
        show_trend = show_trend,
        show_density = show_density,
        density_palette = density_palette,
        xrange = xrange,
        yrange = yrange
      ))
    }
  } else {
    stop("Invalid x and y types")
  }
}


#' Computes the "badness" score of a set of m and a statistics
#'
#' The badness score is how non-linear the data is. It also takes into account
#' the distance from zero. It is computed by fitting a GAM to the data,
#' generating predictions from the function, and getting the mean of the
#' predictions. The predictions are beforehand binned to prevent really dense
#' regions of the prediction to affect the score.
#'
#' @param m A numeric vector of m values
#' @param a A numeric vector of a values
#'
#' @returns A numeric value representing how bad the distribution is.
#'
#' @author Hedmad
get_mamaplot_score <- function(m, a) {
  plot_data <- data.frame("y" = m, "x" = a)
  ### ----------------

  model <- mgcv::gam(y ~ s(x, bs = "cs"), data = plot_data)
  model |> predict() -> predicted.vals
  predicted.vals |> abs() |> bin_mean(bin_by = a, n_bins = 200) |> mean(na.rm = TRUE) -> score

  ### ----------------
  return(score)
}
