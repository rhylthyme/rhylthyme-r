#' Tables from a Rhylthyme program
#'
#' Each returns a plain data frame, ready for dplyr and ggplot2.
#'
#' * `steps()`: one row per step, replicate instances included, in authored
#'   order: `track_id`, `track_name`, `track_index`, `step_id`, `name`,
#'   `task`, `start` and `end` (seconds from program start, as the engine
#'   times them), `duration`, `resolved` (`FALSE` when the engine could not
#'   place the step), `duration_type` (`fixed`, `variable`, `indefinite` or
#'   `instrument`), `min_seconds` and `max_seconds` (the authored range),
#'   `estimate` (the planned length is a guess: instrument steps with no
#'   duration, indefinite steps, and durations `rhylthyme plan` filled in),
#'   `trigger_type`, and for a replicate instance `instance_of` (the
#'   authored step) and `instance_index`. `as.data.frame()` and `tidy()` give
#'   the same table.
#' * `tracks()`: one row per track as the engine lays them out (replicate and
#'   template tracks included): `track_id`, `name`, `track_index`,
#'   `parent_track_id`, `template_id`, `n_steps`, `start`, `end`.
#' * `dependencies()`: one row per trigger that refers to another step:
#'   `from_step`, `to_step`, `relation` (`end_to_start`, `start_to_start` or
#'   `on_abort`), `offset` in seconds (buffers included; negative for a
#'   hand-off before the other step ends), `trigger_type`, and `logic` (`all`
#'   or `any`) for a trigger inside a compound one.
#' * `resources()`: one row per task: `task`, `max_concurrent` (`NA` for a
#'   task steps use that has no constraint), `description`, `n_steps`.
#'
#' @param x A `rhylthyme_program`.
#' @param ... Unused.
#' @return A data frame.
#' @examples
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' steps(p)
#' tracks(p)
#' dependencies(p)
#' resources(p)
#' @name rhylthyme_tables
NULL

#' @rdname rhylthyme_tables
#' @export
steps <- function(x, ...) UseMethod("steps")

#' @rdname rhylthyme_tables
#' @export
tracks <- function(x, ...) UseMethod("tracks")

#' @rdname rhylthyme_tables
#' @export
dependencies <- function(x, ...) UseMethod("dependencies")

#' @rdname rhylthyme_tables
#' @export
resources <- function(x, ...) UseMethod("resources")

#' @export
steps.rhylthyme_program <- function(x, ...) x$steps

#' @export
tracks.rhylthyme_program <- function(x, ...) {
  out <- x$tracks
  s <- x$steps
  out$n_steps <- vapply(out$track_id, function(id) sum(s$track_id == id), integer(1), USE.NAMES = FALSE)
  bound <- function(values, f) if (length(values) && any(!is.na(values))) f(values, na.rm = TRUE) else NA_real_
  out$start <- vapply(out$track_id, function(id) bound(s$start[s$track_id == id], min), 0, USE.NAMES = FALSE)
  out$end <- vapply(out$track_id, function(id) bound(s$end[s$track_id == id], max), 0, USE.NAMES = FALSE)
  out
}

#' @export
dependencies.rhylthyme_program <- function(x, ...) x$dependencies

#' @export
resources.rhylthyme_program <- function(x, ...) {
  constraints <- x$program$resourceConstraints %||% list()
  pick <- function(field, type) {
    vapply(constraints, function(r) {
      v <- r[[field]]
      if (is.null(v)) NA else as.vector(v, mode = type)
    }, vector(type, 1))
  }
  out <- data.frame(
    task = pick("task", "character"),
    max_concurrent = pick("maxConcurrent", "integer"),
    description = pick("description", "character"),
    stringsAsFactors = FALSE
  )
  tasks <- x$steps$task
  used <- setdiff(unique(tasks[!is.na(tasks)]), out$task)
  if (length(used)) {
    out <- rbind(out, data.frame(task = used, max_concurrent = NA_integer_,
                                 description = NA_character_, stringsAsFactors = FALSE))
  }
  out$n_steps <- vapply(out$task, function(t) sum(x$steps$task %in% t), integer(1), USE.NAMES = FALSE)
  out
}

#' @rdname rhylthyme_tables
#' @param row.names,optional Unused; for compatibility with [as.data.frame()].
#' @export
as.data.frame.rhylthyme_program <- function(x, row.names = NULL, optional = FALSE, ...) {
  steps(x)
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' @rdname rhylthyme_tables
#' @export
tidy.rhylthyme_program <- function(x, ...) steps(x)

#' The program as a list
#'
#' The parsed program JSON, unchanged. `jsonlite::toJSON(as.list(p),
#' auto_unbox = TRUE, null = "null", digits = NA)` turns it back into the
#' program's JSON, and `p$json` holds the text exactly as it was read.
#'
#' @param x A `rhylthyme_program`.
#' @param ... Unused.
#' @return A list.
#' @export
as.list.rhylthyme_program <- function(x, ...) x$program

#' @export
print.rhylthyme_program <- function(x, ...) {
  cat(sprintf(
    "<rhylthyme_program> %s: %d tracks, %d steps, %s\n",
    x$name, nrow(x$tracks), nrow(x$steps), format_elapsed(makespan(x))
  ))
  invisible(x)
}

#' Summarise a Rhylthyme program
#'
#' @param object A `rhylthyme_program`.
#' @param ... Unused.
#' @return A `summary.rhylthyme_program`: a list with `name`,
#'   `schema_version`, `makespan` (seconds), `n_steps`, `n_estimated`,
#'   `n_unresolved`, and the [tracks()] and [resources()] tables. Printing it
#'   shows each track's steps and time span, and each resource's limit.
#' @examples
#' summary(read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme")))
#' @export
summary.rhylthyme_program <- function(object, ...) {
  s <- object$steps
  structure(
    list(
      name = object$name,
      schema_version = object$schema_version,
      makespan = makespan(object),
      n_steps = nrow(s),
      n_estimated = sum(s$estimate, na.rm = TRUE),
      n_unresolved = sum(!s$resolved, na.rm = TRUE),
      tracks = tracks(object),
      resources = resources(object)
    ),
    class = "summary.rhylthyme_program"
  )
}

#' @export
print.summary.rhylthyme_program <- function(x, ...) {
  version <- if (is.na(x$schema_version)) "no schemaVersion" else paste("schema", x$schema_version)
  cat(sprintf("%s (%s)\n", x$name, version))
  notes <- c(
    if (x$n_estimated) sprintf("%d estimated", x$n_estimated),
    if (x$n_unresolved) sprintf("%d not placed", x$n_unresolved)
  )
  cat(sprintf(
    "Makespan %s: %d tracks, %s%s\n",
    format_elapsed(x$makespan), nrow(x$tracks), n_steps(x$n_steps),
    if (length(notes)) paste0(" (", paste(notes, collapse = ", "), ")") else ""
  ))
  t <- x$tracks
  if (nrow(t)) {
    cat("\nTracks:\n")
    span <- ifelse(is.na(t$start), "", paste(format_elapsed(t$start), "-", format_elapsed(t$end)))
    lines <- sprintf("  %s  %s  %s", format(t$name), format(n_steps(t$n_steps)), span)
    cat(sub("\\s+$", "", lines), sep = "\n")
  }
  r <- x$resources
  if (nrow(r)) {
    cat("\nResources:\n")
    limit <- ifelse(is.na(r$max_concurrent), "no limit", sprintf("max %d", r$max_concurrent))
    cat(sprintf("  %s  %s  (%s)", format(r$task), format(limit), n_steps(r$n_steps)), sep = "\n")
  }
  invisible(x)
}

n_steps <- function(n) ifelse(n == 1, "1 step", sprintf("%d steps", n))

# Program length: the latest step end (the program starts at 0).
makespan <- function(x) {
  ends <- x$steps$end
  if (length(ends) && any(!is.na(ends))) max(ends, na.rm = TRUE) else 0
}

format_elapsed <- function(seconds) {
  seconds <- round(seconds)
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  ifelse(h > 0, sprintf("%d:%02d:%02d", h, m, s), sprintf("%d:%02d", m, s))
}
