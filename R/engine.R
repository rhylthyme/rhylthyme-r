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

# One row per step (replicate instances included), in authored track and
# step order, with the engine's start and end in seconds from program start,
# how its duration is known (duration_type), whether that is an estimate, and
# for a replicate instance the authored step it came from and its index.
engine_steps <- function(json) {
  rows <- engine()$call("RhylthymeR.stepsFromJson", json)
  columns <- list(
    track_id = character(), track_name = character(), track_index = integer(),
    step_id = character(), name = character(), start = numeric(),
    end = numeric(), resolved = logical(), duration_type = character(),
    estimate = logical(), instance_of = character(), instance_index = integer()
  )
  steps <- if (length(rows)) {
    as.data.frame(rows, stringsAsFactors = FALSE)
  } else {
    as.data.frame(columns, stringsAsFactors = FALSE)
  }
  for (col in names(columns)) {
    if (is.null(steps[[col]])) steps[[col]] <- rep(NA, nrow(steps))
    storage.mode(steps[[col]]) <- storage.mode(columns[[col]])
  }
  steps$duration <- steps$end - steps$start
  first <- names(columns)[1:7]
  steps[c(first, "duration", setdiff(names(columns), first))]
}
