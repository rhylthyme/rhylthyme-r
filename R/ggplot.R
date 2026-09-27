#' Plot a Rhylthyme schedule
#'
#' `geom_rhylthyme()` draws one bar per step, from its start to its end, in
#' one lane per track. With a program as the plot's data
#' (`ggplot(program) + geom_rhylthyme()`), the aesthetics default to
#' `xmin = start`, `xmax = end`, `y = lane` and `fill = track`; anything you
#' map yourself, in the plot or the layer, wins. The geom also works by name:
#' `geom = "rhylthyme"`.
#'
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to [ggplot2::layer()], such as
#'   `colour = "white"`.
#' @param na.rm If `FALSE`, the default, steps with no computed time are
#'   removed with a warning; if `TRUE`, silently.
#' @return A layer to add to a plot.
#' @examples
#' library(ggplot2)
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' ggplot(p) + geom_rhylthyme()
#' @export
geom_rhylthyme <- function(mapping = NULL, data = NULL, stat = "rhylthyme",
                           position = "identity", ..., na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
  rhylthyme_layer(mapping, data, stat = stat, geom = GeomRhylthyme,
                  position = position, na.rm = na.rm, show.legend = show.legend,
                  inherit.aes = inherit.aes, ...)
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
  layer <- ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
  structure(list(layer = layer, data = data, inherit.aes = inherit.aes),
            class = "rhylthyme_layer")
}

# Fill in the program's default aesthetics once the plot is known, so they
# never override a mapping the user gave in ggplot() or in the layer.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.rhylthyme_layer <- function(object, plot, ...) {
  layer <- object$layer
  data <- if (is.data.frame(object$data)) object$data else plot$data
  if (is_program_data(data)) {
    mine <- as.list(layer$mapping %||% ggplot2::aes())
    inherited <- if (isTRUE(object$inherit.aes)) as.list(plot$mapping %||% ggplot2::aes()) else list()
    defaults <- as.list(ggplot2::aes(xmin = start, xmax = end, y = lane, fill = track))
    missing <- setdiff(names(defaults), c(names(mine), names(inherited)))
    if (length(missing)) {
      layer$mapping <- ggplot2::aes(!!!c(mine, defaults[missing]))
    }
  }
  plot + layer
}

is_program_data <- function(data) {
  is.data.frame(data) && all(c("step_id", "start", "end", "lane", "track") %in% names(data))
}

#' Turn a program into plotting data
#'
#' One row per step: the columns of the program's `steps` table, plus `track`
#' (a factor in authored order) and `lane` (the same, reversed, so the first
#' track is drawn at the top).
#'
#' @param model A `rhylthyme_program`.
#' @param data Ignored.
#' @param ... Ignored.
#' @return A data frame.
#' @exportS3Method ggplot2::fortify
fortify.rhylthyme_program <- function(model, data, ...) {
  steps <- model$steps
  tracks <- unique(steps[c("track_id", "track_name", "track_index")])
  tracks <- tracks[order(tracks$track_index), ]
  label <- if (anyDuplicated(tracks$track_name)) tracks$track_id else tracks$track_name
  key <- match(steps$track_id, tracks$track_id)
  steps$track <- factor(label[key], levels = label)
  steps$lane <- factor(label[key], levels = rev(label))
  steps
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
    colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1, alpha = NA
  ),
  # A stat other than StatRhylthyme (e.g. stat = "identity") leaves the bar
  # extents to the geom.
  setup_data = function(data, params) {
    if (is.null(data$ymin) || is.null(data$ymax)) data <- layout_bars(data)
    data
  },
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre") {
    ggplot2::GeomRect$draw_panel(data, panel_params, coord,
                                 lineend = lineend, linejoin = linejoin)
  },
  draw_key = ggplot2::draw_key_rect
)
