#' Plot a Rhylthyme schedule
#'
#' `geom_rhylthyme()` draws one bar per step, from its start to its end, in
#' one lane per track, with arrows for dependencies between lanes and step
#' names on (or beside) the bars. With a program as the plot's data
#' (`ggplot(program) + geom_rhylthyme()`), the aesthetics default to
#' `xmin = start`, `xmax = end`, `y = lane`, `fill = track`, `step = step_id`
#' and `label = name`, and the arrows come from the program's
#' [dependencies()]; anything you map or pass yourself wins. The geom also
#' works by name: `geom = "rhylthyme"`.
#'
#' Aesthetics: `xmin`, `xmax` and `y` (required); `fill`, `colour`, `alpha`,
#' `linewidth`, `linetype`; `label` (the text for each bar), `step` (the id
#' the `dependencies` table refers to), `xlower` and `xupper` (a variable
#' step's shortest and longest end) and `estimate` (`TRUE` for an estimated
#' length). For a program these default to `range_min`, `range_max` and
#' `estimate`.
#'
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to [ggplot2::layer()], such as
#'   `colour = "white"`.
#' @param arrows Draw dependency arrows between lanes. Solid for a step that
#'   waits for another; dashed when it starts before the other ends (a
#'   negative offset); dotted for a step that runs only if the other aborts.
#' @param labels Draw each bar's `label`: inside the bar when it fits, else
#'   just after it when the lane has room, else not at all.
#' @param dependencies A data frame with `from_step`, `to_step` and
#'   `relation` (and `offset`), as [dependencies()] returns, matched to the
#'   `step` aesthetic. Taken from the program when the plot's data is one.
#' @param ranges Show each variable step's range: a faded extension from
#'   its planned end to its longest, and a thin line under the bar from its
#'   shortest to its longest.
#' @param estimates Outline, dotted, the bars whose length is an estimate
#'   (instrument steps with no duration, indefinite steps, and durations
#'   `rhylthyme plan` filled in), as rhylthyme.com does.
#' @param lanes What each lane is, for a program as the data: `"track"` (the
#'   default; one lane per track, as on rhylthyme.com), `"step"` (one lane
#'   per step, a classic Gantt; step names are then on the axis, so bars get
#'   no label unless you map one) or `"task"` (one lane per resource type, so
#'   contention shows). In any mode, bars that overlap in a lane are stacked
#'   so none hides another.
#' @param collapse_replicates Draw one bar per authored step instead of one
#'   per replicate instance, spanning all its instances and labelled with
#'   the count (only for a program as the data; [steps()] is unaffected).
#' @param label_size Label text size, in mm (as [ggplot2::geom_text()]'s
#'   `size`).
#' @param na.rm If `FALSE`, the default, steps with no computed time are
#'   removed with a warning; if `TRUE`, silently.
#' @return A layer to add to a plot.
#' @examples
#' library(ggplot2)
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' ggplot(p) + geom_rhylthyme()
#' ggplot(p) + geom_rhylthyme(labels = FALSE)
#' @export
geom_rhylthyme <- function(mapping = NULL, data = NULL, stat = "rhylthyme",
                           position = "identity", ..., arrows = TRUE,
                           labels = TRUE, ranges = TRUE, estimates = TRUE,
                           lanes = c("track", "step", "task"),
                           collapse_replicates = FALSE, dependencies = NULL,
                           label_size = 3, na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE) {
  lanes_given <- !missing(lanes)
  lanes <- match.arg(lanes)
  rhylthyme_layer(mapping, data, stat = stat, geom = GeomRhylthyme,
                  position = position, na.rm = na.rm, show.legend = show.legend,
                  inherit.aes = inherit.aes, lanes = lanes, lanes_given = lanes_given,
                  collapse_replicates = collapse_replicates,
                  arrows = arrows, labels = labels, ranges = ranges,
                  estimates = estimates, dependencies = dependencies,
                  label_size = label_size, ...)
}

#' @rdname geom_rhylthyme
#' @export
stat_rhylthyme <- function(mapping = NULL, data = NULL, geom = "rhylthyme",
                           position = "identity", ..., na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
  rhylthyme_layer(mapping, data, stat = StatRhylthyme, geom = geom,
                  position = position, na.rm = na.rm, show.legend = show.legend,
                  inherit.aes = inherit.aes, ...)
}

rhylthyme_layer <- function(mapping, data, stat, geom, position, na.rm,
                            show.legend, inherit.aes, lanes = "track",
                            lanes_given = FALSE, collapse_replicates = FALSE, ...) {
  if (inherits(data, c("rhylthyme_program", "rhylthyme_programs", "rhylthyme_run"))) data <- ggplot2::fortify(data)
  params <- list(na.rm = na.rm, ...)
  params <- params[!vapply(params, is.null, logical(1))]
  layer <- ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = params
  )
  structure(list(layer = layer, data = data, inherit.aes = inherit.aes,
                 lanes = lanes, lanes_given = isTRUE(lanes_given),
                 collapse_replicates = isTRUE(collapse_replicates)),
            class = "rhylthyme_layer")
}

# Fill in the program's default aesthetics and dependencies once the plot is
# known, so they never override a mapping or table the user gave.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.rhylthyme_layer <- function(object, plot, ...) {
  layer <- object$layer
  data <- if (is.data.frame(object$data)) object$data else plot$data
  program <- attr(data, "rhylthyme_program")
  if (is_program_data(data) && inherits(program, c("rhylthyme_program", "rhylthyme_programs", "rhylthyme_run"))) {
    mine <- as.list(layer$mapping %||% ggplot2::aes())
    inherited <- if (isTRUE(object$inherit.aes)) as.list(plot$mapping %||% ggplot2::aes()) else list()
    # x = start is not drawn (bars use xmin/xmax); it lets ggplot2 pick the
    # program's time scale (see scale_type.rhylthyme_elapsed).
    defaults <- as.list(ggplot2::aes(
      x = start, xmin = start, xmax = end, fill = track, step = step_key, label = name,
      xlower = range_min, xupper = range_max, estimate = estimate
    ))
    defaults$y <- as.list(switch(object$lanes %||% "track",
      track = ggplot2::aes(y = lane),
      step = ggplot2::aes(y = step_lane),
      task = ggplot2::aes(y = task_lane)
    ))$y
    # One lane per step already names every bar on the axis.
    if (identical(object$lanes, "step")) defaults$label <- NULL
    if (isTRUE(object$lanes_given) && ("y" %in% c(names(mine), names(inherited)))) {
      warning("`lanes` is ignored because `y` is mapped.", call. = FALSE)
    }
    missing <- setdiff(names(defaults), c(names(mine), names(inherited)))
    if (length(missing)) {
      layer$mapping <- ggplot2::aes(!!!c(mine, defaults[missing]))
    }
    prepared <- plot_data(program, collapse = isTRUE(object$collapse_replicates))
    if (isTRUE(object$collapse_replicates)) layer$data <- prepared$data
    dependencies <- prepared$dependencies
    if (is.null(layer$geom_params$dependencies) && "dependencies" %in% layer$geom$parameters()) {
      layer$geom_params$dependencies <- dependencies
    }
  } else {
    if (isTRUE(object$collapse_replicates)) {
      warning("`collapse_replicates` needs a Rhylthyme program as the data; ignored.", call. = FALSE)
    }
    if (isTRUE(object$lanes_given)) {
      warning("`lanes` needs a Rhylthyme program as the data; map `y` instead.", call. = FALSE)
    }
  }
  plot + layer
}

is_program_data <- function(data) {
  is.data.frame(data) && all(c("step_key", "start", "end", "lane", "track") %in% names(data))
}

#' Turn programs into plotting data
#'
#' One row per step: the columns of [steps()], plus `program` (a factor, for
#' faceting), `step_key` (the step id, prefixed by the program within a set
#' of programs, so it is unique), `track` (a factor in authored order; tracks
#' with the same name share a level, and a colour), the lane factors `lane`
#' (by track), `step_lane` and `task_lane` (see `lanes` in
#' [geom_rhylthyme()]; levels run bottom to top so the first track or step
#' is drawn at the top, and within a set each program's lanes are its own),
#' and `range_min` / `range_max` (a variable step's shortest and longest
#' end; `NA` otherwise). `start`, `end` and the ranges are seconds from
#' program start, or clock times for programs read with `start_at` /
#' `finish_at`; they carry a class that makes ggplot2 use
#' [scale_x_rhylthyme()] (or [scale_x_rhylthyme_clock()]) by default, as
#' `track` does for [scale_fill_rhylthyme()] and the lanes for
#' [scale_y_rhylthyme()]. The program rides along as the
#' `"rhylthyme_program"` attribute, for [geom_rhylthyme()]'s arrows and
#' replicate collapsing.
#'
#' @param model A `rhylthyme_program` or [rhylthyme_programs()].
#' @param data Ignored.
#' @param ... Ignored.
#' @return A data frame.
#' @exportS3Method ggplot2::fortify
fortify.rhylthyme_program <- function(model, data, ...) plot_data(model)$data

#' @rdname fortify.rhylthyme_program
#' @exportS3Method ggplot2::fortify
fortify.rhylthyme_programs <- function(model, data, ...) plot_data(model)$data

# The steps table plus each variable step's range, in plain seconds; steps
# with no time (a run's steps that never started) are not drawn.
plot_frame <- function(model) {
  out <- model$steps
  out <- out[!is.na(out$start) & !is.na(out$end), , drop = FALSE]
  ranged <- !is.na(out$min_seconds) & !is.na(out$max_seconds) & out$max_seconds > out$min_seconds
  out$range_min <- ifelse(ranged, out$start + out$min_seconds, NA_real_)
  out$range_max <- ifelse(ranged, out$start + out$max_seconds, NA_real_)
  out
}

#' @rdname geom_rhylthyme
#' @format NULL
#' @usage NULL
#' @export
StatRhylthyme <- ggplot2::ggproto("StatRhylthyme", ggplot2::Stat,
  required_aes = c("xmin", "xmax", "y"),
  compute_panel = function(data, scales) {
    layout_bars(stack_overlaps(data))
  }
)

#' @rdname geom_rhylthyme
#' @format NULL
#' @usage NULL
#' @export
GeomRhylthyme <- ggplot2::ggproto("GeomRhylthyme", ggplot2::Geom,
  required_aes = c("xmin", "xmax", "y"),
  non_missing_aes = c("ymin", "ymax"),
  default_aes = ggplot2::aes(
    colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1, alpha = NA,
    label = NA, step = NA, estimate = FALSE
  ),
  # A stat other than StatRhylthyme (e.g. stat = "identity") leaves the bar
  # extents to the geom.
  setup_data = function(data, params) {
    if (is.null(data$ymin) || is.null(data$ymax)) data <- layout_bars(stack_overlaps(data))
    data
  },
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", arrows = TRUE, labels = TRUE,
                        ranges = TRUE, estimates = TRUE, dependencies = NULL,
                        label_size = 3) {
    grobs <- list()
    if (isTRUE(ranges) && !is.null(data$xupper)) {
      grobs <- c(grobs, list(range_grob(data, panel_params, coord)))
    }
    grobs <- c(grobs, list(ggplot2::GeomRect$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )))
    if (isTRUE(estimates) && any(data$estimate %in% TRUE)) {
      grobs <- c(grobs, list(estimate_grob(data, panel_params, coord)))
    }
    if (isTRUE(arrows) && !is.null(dependencies)) {
      grobs <- c(grobs, list(arrow_grob(data, dependencies, panel_params, coord)))
    }
    if (isTRUE(labels) && any(!is.na(data$label))) {
      grobs <- c(grobs, list(label_grob(data, panel_params, coord, label_size)))
    }
    grid::gTree(children = do.call(grid::gList, grobs), name = "geom_rhylthyme")
  },
  draw_key = ggplot2::draw_key_rect
)

# A variable step's range: the faded extension from its planned end to its
# longest (as the web draws it), under the bar, and a whisker from shortest
# to longest in the gap below the bar.
range_grob <- function(data, panel_params, coord) {
  r <- data[is.finite(data$xlower) & is.finite(data$xupper), , drop = FALSE]
  if (!nrow(r)) return(grid::nullGrob())
  ext <- r[r$xupper > r$xmax, , drop = FALSE]
  children <- list()
  if (nrow(ext)) {
    ext$xmin <- ext$xmax
    ext$xmax <- ext$xupper
    ext$alpha <- 0.3
    ext$colour <- NA
    children <- c(children, list(ggplot2::GeomRect$draw_panel(ext, panel_params, coord)))
  }
  # Lanes are 1 unit apart and bars 0.8 high: the whisker sits mid-gap.
  y <- r$ymin - 0.05
  cap <- 0.1 / 3
  seg <- data.frame(
    x = c(r$xlower, r$xlower, r$xupper), xend = c(r$xupper, r$xlower, r$xupper),
    y = c(y, y - cap, y - cap), yend = c(y, y + cap, y + cap)
  )
  seg <- coord$transform(seg, panel_params)
  children <- c(children, list(grid::segmentsGrob(
    seg$x, seg$y, seg$xend, seg$yend, default.units = "native",
    gp = grid::gpar(col = "grey35", lwd = 0.4 * ggplot2::.pt, lineend = "butt"),
    name = "whiskers"
  )))
  grid::gTree(children = do.call(grid::gList, children), name = "ranges")
}

# Dotted outline over bars whose length is an estimate.
estimate_grob <- function(data, panel_params, coord) {
  e <- data[data$estimate %in% TRUE, , drop = FALSE]
  e$fill <- NA
  e$colour <- "grey15"
  e$linetype <- 3
  e$linewidth <- 0.6
  e$alpha <- NA
  grob <- ggplot2::GeomRect$draw_panel(e, panel_params, coord)
  grob$name <- "estimates"
  grob
}

arrow_grob <- function(data, dependencies, panel_params, coord) {
  bars <- data
  bars$step <- as.character(bars$step)
  paths <- layout_arrows(bars, dependencies)
  if (!nrow(paths)) return(grid::nullGrob())
  paths <- coord$transform(paths, panel_params)
  first <- !duplicated(paths$arrow)
  kind <- ifelse(paths$relation[first] == "on_abort", 3, ifelse(paths$offset[first] < 0, 2, 1))
  grid::polylineGrob(
    paths$x, paths$y, id = paths$arrow,
    arrow = grid::arrow(length = grid::unit(1.6, "mm"), type = "closed"),
    gp = grid::gpar(col = "grey30", fill = "grey30", lwd = 0.45 * ggplot2::.pt, lty = kind),
    name = "arrows"
  )
}

# Labels are placed when the plot is drawn, when the text and the bars have
# a size: see makeContent.rhylthyme_labels.
label_grob <- function(data, panel_params, coord, label_size) {
  bars <- coord$transform(data, panel_params)
  bars <- bars[!is.na(bars$label), , drop = FALSE]
  grid::gTree(
    bars = data.frame(
      xmin = bars$xmin, xmax = bars$xmax, ymin = bars$ymin, ymax = bars$ymax,
      lane = bars$y, label = as.character(bars$label), fill = bars$fill,
      stringsAsFactors = FALSE
    ),
    fontsize = label_size * ggplot2::.pt,
    name = "labels",
    cl = "rhylthyme_labels"
  )
}

#' @importFrom grid makeContent
#' @exportS3Method grid::makeContent
makeContent.rhylthyme_labels <- function(x) {
  bars <- x$bars
  gp <- grid::gpar(fontsize = x$fontsize)
  measure <- function(text) grid::textGrob(text, gp = gp)
  width <- vapply(bars$label, function(l) {
    grid::convertWidth(grid::grobWidth(measure(l)), "npc", valueOnly = TRUE)
  }, 0, USE.NAMES = FALSE)
  height <- grid::convertHeight(grid::grobHeight(measure("Ag")), "npc", valueOnly = TRUE)
  pad <- grid::convertWidth(grid::unit(1.2, "mm"), "npc", valueOnly = TRUE)
  where <- place_labels(bars, width, rep(height, nrow(bars)), pad = pad)
  shown <- where$placement != "none"
  if (!any(shown)) return(grid::setChildren(x, grid::gList()))
  ink <- ifelse(where$placement == "inside", label_ink(bars$fill), "grey15")
  text <- grid::textGrob(
    bars$label[shown], x = where$x[shown], y = (bars$ymin[shown] + bars$ymax[shown]) / 2,
    hjust = 0, vjust = 0.5, default.units = "npc",
    gp = grid::gpar(fontsize = x$fontsize, col = ink[shown])
  )
  grid::setChildren(x, grid::gList(text))
}
