RUNS_SCHEMA <- "runs_schema_0.1.0-alpha.json"

RUN_COLUMNS <- list(
  step_id = character(), run_step_id = character(), instance = integer(),
  start = numeric(), end = numeric(), planned_start = numeric(),
  planned_end = numeric(), ended_by = character(), failed = logical(),
  last_code = character(), paused_seconds = numeric(), waited_on = character()
)

#' Read a Rhylthyme run record
#'
#' A run record (the `runs` schema) is what actually happened when a program
#' was run: when each step really started and ended, and what ended it. It
#' is written by the `rhylthyme` runner and by rhylthyme.com.
#'
#' @param x The run record: a path, an `http(s)://` URL, a JSON string or a
#'   list, as for [read_rhylthyme()].
#' @param program The program that was run (a `rhylthyme_program`, or
#'   anything [read_rhylthyme()] accepts). Optional, but it gives each step
#'   its track, name and task, maps replicate instances to the program's step
#'   ids, and brings the dependency arrows; without it, steps are named by id
#'   and share one lane.
#' @param validate Check the record against the runs schema (see
#'   [read_rhylthyme()]).
#' @param clock If `TRUE`, the default, times are anchored at the record's
#'   `startedAt`: [steps()] adds `start_time` and `end_time`, and plots have a
#'   clock-time axis. `FALSE` keeps elapsed seconds.
#' @return A `rhylthyme_run`: a list with `name`, `run_id`, `program_id`,
#'   `program_version`, `started_at`, `ended_at`, `outcome` (`completed`,
#'   `aborted` or `abandoned`), `context`, the `steps` table and the parsed
#'   `record`. The steps table has one row per step in the record: the
#'   program's `step_id` and track columns, the actual `start`, `end` and
#'   `duration` (seconds from `startedAt`; `NA` for a step that never
#'   started), `status` (`completed`, `aborted`, `skipped`, `unfinished` or
#'   `not_started`), `ended_by`, `failed` (an instrument command failed at
#'   least once) and `last_code` (its last reply), and the plan the runner
#'   had (`planned_start`, `planned_end`).
#' @examples
#' extdata <- function(f) system.file("extdata", f, package = "rhylthyme")
#' dinner <- read_rhylthyme(extdata("thanksgiving_one_oven.json"))
#' run <- read_rhylthyme_run(extdata("thanksgiving_one_oven.run.json"), program = dinner)
#' run
#' steps(run)[c("name", "start", "end", "status")]
#' autoplot(run)
#' @export
read_rhylthyme_run <- function(x, program = NULL, validate = TRUE, clock = TRUE) {
  src <- resolve_source(x)
  record <- tryCatch(
    jsonlite::parse_json(src$json, simplifyVector = FALSE),
    error = function(e) stop("Not valid JSON: ", conditionMessage(e), call. = FALSE)
  )
  if (!is.list(record) || is.null(record$steps) || is.null(record$runId)) {
    stop("Not a Rhylthyme run record: it has no `runId` and `steps`.", call. = FALSE)
  }
  if (isTRUE(validate)) stop_if_invalid(schema_problems(src$json, record, RUNS_SCHEMA), what = "run record")
  if (!is.null(program) && !inherits(program, "rhylthyme_program")) program <- read_rhylthyme(program)
  if (!is.null(program) && !identical(record$programId, program$program$programId)) {
    warning(sprintf("The run is of program '%s', but the program given is '%s'.",
                    record$programId, program$program$programId %||% "(no programId)"), call. = FALSE)
  }

  rows <- typed_frame(
    engine()$call("RhylthymeR.runFromJson", src$json, if (is.null(program)) "" else program$json),
    RUN_COLUMNS
  )
  steps <- run_steps(rows, program)
  started <- parse_iso_time(record$startedAt)
  name <- if (is.null(program)) record$programId else program$name
  structure(
    list(
      name = sprintf("%s (run %s)", name, format(started, "%Y-%m-%d %H:%M")),
      run_id = record$runId,
      program_id = record$programId,
      program_version = record$programVersion,
      started_at = started,
      ended_at = if (is.null(record$endedAt)) NULL else parse_iso_time(record$endedAt),
      outcome = record$outcome,
      context = record$context,
      source = src$source,
      steps = steps,
      tracks = if (is.null(program)) run_track(record) else program$tracks,
      dependencies = if (is.null(program)) typed_frame(list(), DEPENDENCY_COLUMNS) else program$dependencies,
      program = if (is.null(program)) list(tracks = list()) else program$program,
      start_at = if (isTRUE(clock)) started else NULL,
      record = record,
      json = src$json
    ),
    class = "rhylthyme_run"
  )
}

# The run rows, with the program's track and step columns joined on step id
# (or one "Run" lane, when there is no program), and each step's status.
run_steps <- function(rows, program) {
  n <- nrow(rows)
  if (is.null(program)) {
    info <- data.frame(
      track_id = rep("run", n), track_name = rep("Run", n), track_index = rep(1L, n),
      name = rows$step_id, task = rep(NA_character_, n),
      duration_type = rep(NA_character_, n), trigger_type = rep(NA_character_, n),
      instance_of = ifelse(rows$instance > 1 | grepl("-r[0-9]+$", rows$step_id), rows$run_step_id, NA_character_),
      instance_index = ifelse(is.na(rows$instance), NA_integer_, rows$instance),
      stringsAsFactors = FALSE
    )
  } else {
    ps <- program$steps
    at <- match(rows$step_id, ps$step_id)
    info <- ps[at, c("track_id", "track_name", "track_index", "name", "task", "duration_type",
                     "trigger_type", "instance_of", "instance_index"), drop = FALSE]
    info$name[is.na(at)] <- rows$step_id[is.na(at)]
  }
  rownames(info) <- NULL
  status <- ifelse(
    is.na(rows$start), "not_started",
    ifelse(rows$ended_by %in% "abort", "aborted",
           ifelse(rows$ended_by %in% "skipped", "skipped",
                  ifelse(is.na(rows$end), "unfinished", "completed")))
  )
  out <- data.frame(
    info[c("track_id", "track_name", "track_index")],
    step_id = rows$step_id,
    name = info$name,
    task = info$task,
    start = rows$start,
    end = rows$end,
    duration = rows$end - rows$start,
    status = status,
    ended_by = rows$ended_by,
    failed = rows$failed,
    last_code = rows$last_code,
    planned_start = rows$planned_start,
    planned_end = rows$planned_end,
    paused_seconds = rows$paused_seconds,
    waited_on = rows$waited_on,
    run_step_id = rows$run_step_id,
    instance = rows$instance,
    # What plotting and collapsing share with a program's steps table.
    resolved = !is.na(rows$start) & !is.na(rows$end),
    estimate = rep(FALSE, n),
    duration_type = info$duration_type,
    min_seconds = rep(NA_real_, n),
    max_seconds = rep(NA_real_, n),
    trigger_type = info$trigger_type,
    instance_of = info$instance_of,
    instance_index = as.integer(info$instance_index),
    stringsAsFactors = FALSE
  )
  out
}

run_track <- function(record) {
  data.frame(track_id = "run", name = "Run", track_index = 1L,
             parent_track_id = NA_character_, template_id = NA_character_,
             stringsAsFactors = FALSE)
}

# ISO 8601 (the runs schema's date-time): "Z" or a +hh:mm offset, shown in
# the local time zone.
parse_iso_time <- function(x) {
  if (is.null(x) || !is.character(x) || length(x) != 1) return(NULL)
  offset <- 0
  m <- regmatches(x, regexec("([+-])([0-9]{2}):?([0-9]{2})$", x))[[1]]
  if (length(m)) {
    offset <- (as.integer(m[3]) * 3600 + as.integer(m[4]) * 60) * ifelse(m[2] == "+", 1, -1)
    x <- substr(x, 1, nchar(x) - nchar(m[1]))
  }
  x <- sub("Z$", "", x)
  t <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  if (is.na(t)) return(NULL)
  t <- t - offset
  attr(t, "tzone") <- ""
  t
}

#' @export
print.rhylthyme_run <- function(x, ...) {
  s <- x$steps
  counts <- table(factor(s$status, levels = c("skipped", "aborted", "unfinished", "not_started")))
  notes <- c(
    if (any(s$failed, na.rm = TRUE)) sprintf("%d failed", sum(s$failed, na.rm = TRUE)),
    sprintf("%d %s", counts[counts > 0], sub("_", " ", names(counts)[counts > 0]))
  )
  cat(sprintf(
    "<rhylthyme_run> %s: %s, %s, %s%s\n",
    x$name, x$outcome %||% "no outcome", n_steps(nrow(s)), format_elapsed(makespan(x)),
    if (length(notes)) paste0(" (", paste(notes, collapse = ", "), ")") else ""
  ))
  invisible(x)
}

#' @export
steps.rhylthyme_run <- function(x, ...) steps.rhylthyme_program(x, ...)

#' @export
tracks.rhylthyme_run <- function(x, ...) tracks.rhylthyme_program(x, ...)

#' @export
dependencies.rhylthyme_run <- function(x, ...) x$dependencies

#' @export
resources.rhylthyme_run <- function(x, ...) resources.rhylthyme_program(x, ...)

#' @export
as.data.frame.rhylthyme_run <- function(x, row.names = NULL, optional = FALSE, ...) steps(x)

#' @export
tidy.rhylthyme_run <- function(x, ...) steps(x)

#' @rdname fortify.rhylthyme_program
#' @exportS3Method ggplot2::fortify
fortify.rhylthyme_run <- function(model, data, ...) plot_data(model)$data

#' @rdname autoplot.rhylthyme_program
#' @exportS3Method ggplot2::autoplot
autoplot.rhylthyme_run <- function(object, ...) autoplot.rhylthyme_program(object, ...)
