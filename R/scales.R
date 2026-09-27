#' Scales for Rhylthyme plots
#'
#' A program plotted with [geom_rhylthyme()] picks these by default: its
#' bars are filled by track from rhylthyme.com's palette, and its time axis
#' reads as elapsed time (or clock time, when the program was read with
#' `start_at` or `finish_at`). Add one yourself to change its options; any
#' other ggplot2 scale replaces them as usual.
#'
#' * `scale_fill_rhylthyme()` / `scale_colour_rhylthyme()`: the web
#'   timeline's palettes, in the same order (colours repeat, as on the web,
#'   when there are more tracks than colours). See [rhylthyme_palettes()].
#' * `scale_x_rhylthyme()`: seconds from program start, labelled `m:ss`, or
#'   `h:mm` once the axis reaches an hour, at round breaks (1, 5, 15 or 30
#'   minutes, 1 or 2 hours, ...). Titled "Elapsed (m:ss)" or "Elapsed
#'   (h:mm)" unless you give a title.
#' * `scale_x_rhylthyme_clock()`: clock times, for an anchored program; a
#'   [ggplot2::scale_x_datetime()] titled "Clock time".
#'
#' @param palette A palette name: one of `names(rhylthyme_palettes())`.
#' @param name,breaks,labels,... Passed on to the underlying ggplot2 scale.
#' @param aesthetics The aesthetics the scale applies to.
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' ggplot(p) + geom_rhylthyme() + scale_fill_rhylthyme("cookbook")
#' ggplot(p) + geom_rhylthyme() + scale_x_rhylthyme(name = "Time since start")
#' @name rhylthyme_scales
NULL

#' @rdname rhylthyme_scales
#' @export
scale_fill_rhylthyme <- function(palette = "classic", ..., aesthetics = "fill") {
  colours <- palette_colours(palette)
  ggplot2::discrete_scale(aesthetics, palette = function(n) rep_len(colours, n), ...)
}

#' @rdname rhylthyme_scales
#' @export
scale_colour_rhylthyme <- function(palette = "classic", ..., aesthetics = "colour") {
  colours <- palette_colours(palette)
  ggplot2::discrete_scale(aesthetics, palette = function(n) rep_len(colours, n), ...)
}

#' @rdname rhylthyme_scales
#' @export
scale_color_rhylthyme <- scale_colour_rhylthyme

#' The web timeline's colour palettes
#'
#' Read from the embedded engine, so they are exactly rhylthyme.com's.
#'
#' @return A named list of character vectors of hex colours.
#' @examples
#' names(rhylthyme_palettes())
#' rhylthyme_palettes()$classic
#' @export
rhylthyme_palettes <- function() {
  if (is.null(.rhylthyme_env$palettes)) {
    .rhylthyme_env$palettes <- lapply(engine()$get("Rhylthyme.PALETTES"), as.character)
  }
  .rhylthyme_env$palettes
}

palette_colours <- function(palette) {
  palettes <- rhylthyme_palettes()
  if (!is.character(palette) || length(palette) != 1 || !palette %in% names(palettes)) {
    stop("`palette` must be one of: ", paste(names(palettes), collapse = ", "), call. = FALSE)
  }
  palettes[[palette]]
}

#' @rdname rhylthyme_scales
#' @export
scale_x_rhylthyme <- function(name = ggplot2::waiver(), breaks = elapsed_breaks,
                              labels = ggplot2::waiver(), ...) {
  parent <- ggplot2::scale_x_continuous(name = name, breaks = breaks, labels = labels, ...)
  ggplot2::ggproto(NULL, parent,
    get_labels = function(self, breaks = self$get_breaks()) {
      if (!inherits(self$labels, "waiver")) {
        return(ggplot2::ggproto_parent(parent, self)$get_labels(breaks))
      }
      format_axis_elapsed(breaks, hours = elapsed_in_hours(self$get_limits()))
    },
    make_title = function(self, ...) {
      title <- ggplot2::ggproto_parent(parent, self)$make_title(...)
      if (is_default_title(title)) {
        if (elapsed_in_hours(self$get_limits())) "Elapsed (h:mm)" else "Elapsed (m:ss)"
      } else {
        title
      }
    },
    make_sec_title = function(self, ...) plain_sec_title(parent, self, ...)
  )
}

# ggplot2 builds the secondary (top) axis title with make_title() too; our
# default title belongs on the primary axis only.
plain_sec_title <- function(parent, self, ...) {
  if (!inherits(self$secondary.axis, "waiver")) return(self$secondary.axis$make_title(...))
  ggplot2::ggproto_parent(parent, self)$make_title(...)
}

#' @rdname rhylthyme_scales
#' @export
scale_x_rhylthyme_clock <- function(name = ggplot2::waiver(), ...) {
  parent <- ggplot2::scale_x_datetime(name = name, ...)
  ggplot2::ggproto(NULL, parent,
    make_title = function(self, ...) {
      title <- ggplot2::ggproto_parent(parent, self)$make_title(...)
      if (is_default_title(title)) "Clock time" else title
    },
    make_sec_title = function(self, ...) plain_sec_title(parent, self, ...)
  )
}

# The title ggplot2 derives from the default mapping (x = start), or none:
# ours to replace. Anything the user set (a scale name, labs(x = )) is kept.
is_default_title <- function(title) {
  is.null(title) || inherits(title, "waiver") || identical(title, "start")
}

elapsed_in_hours <- function(limits) {
  isTRUE(max(abs(limits), na.rm = TRUE) >= 3600)
}

# Round breaks for seconds: the smallest step from a list of natural time
# steps that gives at most 8 breaks across the limits.
elapsed_breaks <- function(limits) {
  limits <- as.numeric(limits)
  if (any(!is.finite(limits))) return(numeric())
  steps <- c(1, 2, 5, 10, 15, 30, 60, 120, 300, 600, 900, 1800, 3600, 7200,
             10800, 14400, 21600, 43200, 86400)
  span <- diff(range(limits))
  step <- steps[which(span / steps <= 8)[1]]
  if (is.na(step)) step <- 86400 * ceiling(span / 86400 / 8)
  seq(ceiling(min(limits) / step) * step, floor(max(limits) / step) * step, by = step)
}

format_axis_elapsed <- function(seconds, hours) {
  seconds <- round(as.numeric(seconds))
  out <- if (hours) {
    sprintf("%d:%02d", seconds %/% 3600, (seconds %% 3600) %/% 60)
  } else {
    sprintf("%d:%02d", seconds %/% 60, seconds %% 60)
  }
  out[is.na(seconds)] <- NA
  out
}

# Plot columns carry a class so ggplot2 picks the scales above by default
# (scale_type()), while any scale the user adds replaces them silently.
elapsed <- function(seconds) structure(as.numeric(seconds), class = "rhylthyme_elapsed")

clock <- function(seconds, start_at) {
  structure(as.numeric(start_at) + as.numeric(seconds),
            class = c("rhylthyme_clock", "POSIXct", "POSIXt"),
            tzone = attr(start_at, "tzone") %||% "")
}

#' @export
`[.rhylthyme_elapsed` <- function(x, i) elapsed(unclass(x)[i])

#' @exportS3Method ggplot2::scale_type
scale_type.rhylthyme_elapsed <- function(x) c("rhylthyme", "continuous")

#' @exportS3Method ggplot2::scale_type
scale_type.rhylthyme_clock <- function(x) c("rhylthyme_clock", "datetime")

#' @exportS3Method ggplot2::scale_type
scale_type.rhylthyme_track <- function(x) c("rhylthyme", "discrete")
