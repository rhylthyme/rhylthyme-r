#' A clean theme for Rhylthyme timelines
#'
#' A minimal theme for Gantt charts: light vertical grid lines on the time
#' axis, none between lanes, no y-axis title (the lanes name themselves) and
#' the legend underneath.
#'
#' @param base_size,base_family Base font size (pt) and family.
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' ggplot(p) + geom_rhylthyme() + theme_rhylthyme()
#' @export
theme_rhylthyme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey88", linewidth = 0.4),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(colour = "grey20"),
      legend.position = "bottom",
      plot.title.position = "plot"
    )
}

#' A finished plot of a Rhylthyme program
#'
#' `ggplot(program) + geom_rhylthyme()` with [theme_rhylthyme()] and the
#' program's name as the title. With one lane per track the fill legend only
#' repeats the lane names, so it is left out. Add to it like any ggplot.
#'
#' @param object A `rhylthyme_program`.
#' @param ... Passed on to [geom_rhylthyme()] (e.g. `lanes = "task"`,
#'   `collapse_replicates = TRUE`).
#' @return A ggplot.
#' @examples
#' library(ggplot2)
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' autoplot(p)
#' autoplot(p, lanes = "task")
#' @exportS3Method ggplot2::autoplot
autoplot.rhylthyme_program <- function(object, ...) {
  args <- list(...)
  lanes <- args$lanes %||% "track"
  plot <- ggplot2::ggplot(object) +
    do.call(geom_rhylthyme, utils::modifyList(list(colour = "white"), args)) +
    theme_rhylthyme() +
    ggplot2::labs(title = object$name)
  if (identical(lanes, "track") && is.null(args$mapping)) plot <- plot + ggplot2::guides(fill = "none")
  plot
}

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
