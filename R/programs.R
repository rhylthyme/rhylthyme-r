#' Several Rhylthyme programs together
#'
#' Combines programs so they can be tabulated and plotted together:
#' `steps()`, `tracks()`, `dependencies()` and `resources()` stack each
#' program's table with a `program` column, and a plot keeps each program's
#' lanes separate (facet with `facet_wrap(~ program, scales = "free_y")`, or
#' use [autoplot()]). Step ids may repeat between programs: plots match
#' dependencies within a program only. `c(p1, p2)` does the same.
#'
#' @param ... `rhylthyme_program`s, runs from [read_rhylthyme_run()], or sets
#'   of them. Argument names become
#'   the `program` labels; otherwise each program's name is used, made
#'   unique.
#' @return A `rhylthyme_programs`.
#' @examples
#' library(ggplot2)
#' breakfast <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' both <- rhylthyme_programs(Saturday = breakfast, Sunday = breakfast)
#' both
#' autoplot(both)
#' @export
rhylthyme_programs <- function(...) {
  args <- list(...)
  given <- names(args) %||% rep("", length(args))
  programs <- list()
  labels <- character()
  for (i in seq_along(args)) {
    a <- args[[i]]
    if (inherits(a, "rhylthyme_programs")) {
      programs <- c(programs, a$programs)
      labels <- c(labels, names(a$programs))
    } else if (inherits(a, c("rhylthyme_program", "rhylthyme_run"))) {
      programs <- c(programs, list(a))
      name <- if (nzchar(given[i])) given[i] else a$name
      labels <- c(labels, if (is.null(name) || is.na(name)) sprintf("program %d", length(programs)) else name)
    } else {
      stop("rhylthyme_programs() takes programs from read_rhylthyme() (or runs from read_rhylthyme_run()).", call. = FALSE)
    }
  }
  if (!length(programs)) stop("rhylthyme_programs() needs at least one program.", call. = FALSE)
  names(programs) <- unique_labels(labels)
  structure(list(programs = programs), class = "rhylthyme_programs")
}

#' @export
c.rhylthyme_program <- function(...) rhylthyme_programs(...)

#' @export
c.rhylthyme_programs <- function(...) rhylthyme_programs(...)

#' @export
c.rhylthyme_run <- function(...) rhylthyme_programs(...)

unique_labels <- function(labels) {
  out <- labels
  seen <- table(labels)
  for (label in names(seen)[seen > 1]) {
    at <- which(labels == label)
    out[at[-1]] <- sprintf("%s (%d)", label, seq_along(at)[-1])
  }
  out
}

#' @export
print.rhylthyme_programs <- function(x, ...) {
  cat(sprintf("<rhylthyme_programs> %d programs\n", length(x$programs)))
  for (label in names(x$programs)) {
    p <- x$programs[[label]]
    cat(sprintf("  %s: %d tracks, %d steps, %s\n", label, nrow(p$tracks), nrow(p$steps),
                format_elapsed(makespan(p))))
  }
  invisible(x)
}

stack_tables <- function(x, table) {
  parts <- lapply(names(x$programs), function(label) {
    t <- table(x$programs[[label]])
    cbind(data.frame(program = rep(label, nrow(t)), stringsAsFactors = FALSE), t)
  })
  do.call(rbind, parts)
}

#' @export
steps.rhylthyme_programs <- function(x, ...) stack_tables(x, steps)

#' @export
tracks.rhylthyme_programs <- function(x, ...) stack_tables(x, tracks)

#' @export
dependencies.rhylthyme_programs <- function(x, ...) stack_tables(x, dependencies)

#' @export
resources.rhylthyme_programs <- function(x, ...) stack_tables(x, resources)

#' @export
as.data.frame.rhylthyme_programs <- function(x, row.names = NULL, optional = FALSE, ...) steps(x)

#' @export
tidy.rhylthyme_programs <- function(x, ...) steps(x)

#' @rdname autoplot.rhylthyme_program
#' @exportS3Method ggplot2::autoplot
autoplot.rhylthyme_programs <- function(object, ...) {
  args <- list(...)
  lanes <- args$lanes %||% "track"
  plot <- ggplot2::ggplot(object) +
    do.call(geom_rhylthyme, utils::modifyList(list(colour = "white"), args)) +
    ggplot2::facet_wrap(ggplot2::vars(program), ncol = 1, scales = "free_y") +
    theme_rhylthyme() +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, face = "bold"))
  if (identical(lanes, "track") && is.null(args$mapping)) plot <- plot + ggplot2::guides(fill = "none")
  plot
}

# Plotting rows and dependencies for a program or a set of them. Each
# program's lanes stay its own (keyed by program, shown by name only: see
# scale_y_rhylthyme()), steps get a `step_key` that is unique across the
# set, and times are elapsed seconds, or clock times when every program is
# anchored. Tracks with the same name share a colour.
plot_data <- function(model, collapse = FALSE) {
  multi <- inherits(model, "rhylthyme_programs")
  programs <- if (multi) model$programs else list(model)
  anchored <- vapply(programs, function(p) !is.null(p$start_at), logical(1))
  if (any(anchored) && !all(anchored)) {
    stop("Anchor all the programs (start_at / finish_at) or none: clock times and elapsed times cannot share an axis.",
         call. = FALSE)
  }
  modes <- c(lane = "track", step_lane = "step", task_lane = "task")
  tops <- lapply(modes, function(m) character())
  track_levels <- character()
  frames <- list()
  deps <- list()
  for (i in seq_along(programs)) {
    p <- programs[[i]]
    key <- if (multi) names(programs)[i] else NULL
    frame <- plot_frame(p)
    d <- p$dependencies
    if (collapse) {
      collapsed <- collapse_replicates(frame, p)
      frame <- collapsed$data
      d <- collapsed$dependencies
    }
    keyed <- function(v) if (is.null(key)) v else paste(key, v, sep = "\t")
    frame$program <- rep(if (multi) key else (p$name %||% NA_character_), nrow(frame))
    frame$step_key <- keyed(frame$step_id)
    d$from_step <- keyed(d$from_step)
    d$to_step <- keyed(d$to_step)
    for (col in names(modes)) {
      f <- lane_factor(frame, modes[[col]])
      frame[[col]] <- keyed(as.character(f))
      tops[[col]] <- c(tops[[col]], keyed(rev(levels(f))))
      if (col == "lane") {
        frame$track <- as.character(f)
        track_levels <- c(track_levels, rev(levels(f)))
      }
    }
    if (anchored[i]) {
      for (col in c("start", "end", "range_min", "range_max")) {
        frame[[col]] <- as.numeric(p$start_at) + frame[[col]]
      }
    }
    frames[[i]] <- frame
    deps[[i]] <- d
  }
  # A program's and a run's rows have different columns; bind on all of them.
  columns <- unique(unlist(lapply(frames, names)))
  frames <- lapply(frames, function(f) {
    for (col in setdiff(columns, names(f))) f[[col]] <- rep(NA, nrow(f))
    f[columns]
  })
  data <- do.call(rbind, frames)
  rownames(data) <- NULL
  for (col in names(modes)) {
    data[[col]] <- structure(factor(data[[col]], levels = rev(unique(tops[[col]]))),
                             class = c("rhylthyme_lane", "factor"))
  }
  data$track <- structure(factor(data$track, levels = unique(track_levels)),
                          class = c("rhylthyme_track", "factor"))
  data$program <- factor(data$program, levels = unique(data$program))
  as_time <- if (all(anchored)) {
    zone <- attr(programs[[1]]$start_at, "tzone") %||% ""
    function(s) clock(s, structure(0, class = c("POSIXct", "POSIXt"), tzone = zone))
  } else {
    elapsed
  }
  for (col in c("start", "end", "range_min", "range_max")) data[[col]] <- as_time(data[[col]])
  attr(data, "rhylthyme_program") <- model
  dependencies <- do.call(rbind, deps)
  rownames(dependencies) <- NULL
  list(data = data, dependencies = dependencies)
}
