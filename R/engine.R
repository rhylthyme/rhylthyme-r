# The timing engine: @rhylthyme/timeline's index.js, a byte copy in inst/js,
# run in one V8 context per session. It is the only source of step times, so
# every plot agrees with rhylthyme.com.

.rhylthyme_env <- new.env(parent = emptyenv())

engine <- function() {
  ct <- .rhylthyme_env$ct
  if (is.null(ct)) {
    ct <- V8::v8()
    ct$source(system.file("js", "index.js", package = "rhylthyme", mustWork = TRUE))
    ct$source(system.file("js", "bridge.js", package = "rhylthyme", mustWork = TRUE))
    .rhylthyme_env$ct <- ct
  }
  ct
}

#' Versions of the embedded engine and the schemas it reads
#'
#' `rhylthyme_engine_version()` is the version of the
#' [@rhylthyme/timeline](https://github.com/rhylthyme/rhylthyme-timeline)
#' timing engine this package carries: step times match rhylthyme.com's for
#' that engine version. `rhylthyme_schema_versions()` lists the program
#' `schemaVersion`s [read_rhylthyme()] accepts.
#'
#' @return A character vector.
#' @examples
#' rhylthyme_engine_version()
#' rhylthyme_schema_versions()
#' @export
rhylthyme_engine_version <- function() {
  engine()$get("RhylthymeR.version")
}

#' @rdname rhylthyme_engine_version
#' @export
rhylthyme_schema_versions <- function() {
  SCHEMA_VERSIONS
}

# Column types of the tables the engine describes. Steps: one row per step
# (replicate instances included) in authored track and step order, with the
# engine's start and end in seconds from program start. Tracks: as the engine
# expands them (template and replicate tracks included). Dependencies: one
# row per step-referencing trigger.
STEP_COLUMNS <- list(
  track_id = character(), track_name = character(), track_index = integer(),
  step_id = character(), name = character(), task = character(),
  start = numeric(), end = numeric(), duration = numeric(), resolved = logical(),
  duration_type = character(), min_seconds = numeric(), max_seconds = numeric(),
  estimate = logical(), trigger_type = character(), instance_of = character(),
  instance_index = integer()
)
TRACK_COLUMNS <- list(
  track_id = character(), name = character(), track_index = integer(),
  parent_track_id = character(), template_id = character()
)
DEPENDENCY_COLUMNS <- list(
  from_step = character(), to_step = character(), relation = character(),
  offset = numeric(), trigger_type = character(), logic = character()
)

engine_describe <- function(json) {
  out <- engine()$call("RhylthymeR.describeJson", json)
  steps <- typed_frame(out$steps, STEP_COLUMNS)
  steps$duration <- steps$end - steps$start
  list(
    steps = steps,
    tracks = typed_frame(out$tracks, TRACK_COLUMNS),
    dependencies = typed_frame(out$dependencies, DEPENDENCY_COLUMNS)
  )
}

# Rows from the engine (a data frame, or an empty list) as a data frame with
# exactly `columns`, in that order and of those types.
typed_frame <- function(rows, columns) {
  frame <- if (is.data.frame(rows) && nrow(rows)) rows else as.data.frame(columns, stringsAsFactors = FALSE)
  for (col in names(columns)) {
    if (is.null(frame[[col]])) frame[[col]] <- rep(NA, nrow(frame))
    storage.mode(frame[[col]]) <- storage.mode(columns[[col]])
  }
  frame <- frame[names(columns)]
  rownames(frame) <- NULL
  frame
}
