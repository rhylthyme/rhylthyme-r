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
#' `linewidth`, `linetype`; `label` (the text for each bar) and `step` (the
#' id the `dependencies` table refers to).
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
                           labels = TRUE, dependencies = NULL, label_size = 3,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  rhylthyme_layer(mapping, data, stat = stat, geom = GeomRhylthyme,
                  position = position, na.rm = na.rm, show.legend = show.legend,
                  inherit.aes = inherit.aes, arrows = arrows, labels = labels,
                  dependencies = dependencies, label_size = label_size, ...)
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
                            show.legend, inherit.aes, ...) {
  if (inherits(data, "rhylthyme_program")) data <- ggplot2::fortify(data)
  params <- list(na.rm = na.rm, ...)
  params <- params[!vapply(params, is.null, logical(1))]
  layer <- ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = params
  )
  structure(list(layer = layer, data = data, inherit.aes = inherit.aes),
            class = "rhylthyme_layer")
}

# Fill in the program's default aesthetics and dependencies once the plot is
# known, so they never override a mapping or table the user gave.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.rhylthyme_layer <- function(object, plot, ...) {
  layer <- object$layer
  data <- if (is.data.frame(object$data)) object$data else plot$data
  if (is_program_data(data)) {
    mine <- as.list(layer$mapping %||% ggplot2::aes())
    inherited <- if (isTRUE(object$inherit.aes)) as.list(plot$mapping %||% ggplot2::aes()) else list()
    defaults <- as.list(ggplot2::aes(
      xmin = start, xmax = end, y = lane, fill = track, step = step_id, label = name
    ))
    missing <- setdiff(names(defaults), c(names(mine), names(inherited)))
    if (length(missing)) {
      layer$mapping <- ggplot2::aes(!!!c(mine, defaults[missing]))
    }
    if (is.null(layer$geom_params$dependencies) && "dependencies" %in% layer$geom$parameters()) {
      layer$geom_params$dependencies <- attr(data, "rhylthyme_dependencies")
    }
  }
  plot + layer
}

is_program_data <- function(data) {
  is.data.frame(data) && all(c("step_id", "start", "end", "lane", "track") %in% names(data))
}

#' Turn a program into plotting data
#'
#' One row per step: the columns of [steps()], plus `track` (a factor in
#' authored order) and `lane` (the same, reversed, so the first track is
#' drawn at the top). The program's [dependencies()] ride along as the
#' `"rhylthyme_dependencies"` attribute, for [geom_rhylthyme()]'s arrows.
#'
#' @param model A `rhylthyme_program`.
#' @param data Ignored.
#' @param ... Ignored.
#' @return A data frame.
#' @exportS3Method ggplot2::fortify
fortify.rhylthyme_program <- function(model, data, ...) {
  out <- model$steps
  tracks <- unique(out[c("track_id", "track_name", "track_index")])
  tracks <- tracks[order(tracks$track_index), ]
  label <- if (anyDuplicated(tracks$track_name)) tracks$track_id else tracks$track_name
  key <- match(out$track_id, tracks$track_id)
  out$track <- factor(label[key], levels = label)
  out$lane <- factor(label[key], levels = rev(label))
  attr(out, "rhylthyme_dependencies") <- model$dependencies
  out
}

#' @rdname geom_rhylthyme
#' @format NULL
#' @usage NULL
#' @export
StatRhylthyme <- ggplot2::ggproto("StatRhylthyme", ggplot2::Stat,
  required_aes = c("xmin", "xmax", "y"),
  compute_panel = function(data, scales) {
    layout_bars(data)
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
    label = NA, step = NA
  ),
  # A stat other than StatRhylthyme (e.g. stat = "identity") leaves the bar
  # extents to the geom.
  setup_data = function(data, params) {
    if (is.null(data$ymin) || is.null(data$ymax)) data <- layout_bars(data)
    data
  },
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", arrows = TRUE, labels = TRUE,
                        dependencies = NULL, label_size = 3) {
    bars <- ggplot2::GeomRect$draw_panel(data, panel_params, coord,
                                         lineend = lineend, linejoin = linejoin)
    grobs <- list(bars)
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
