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
# step order, with the engine's start and end in seconds from program start.
engine_steps <- function(json) {
  rows <- engine()$call("RhylthymeR.stepsFromJson", json)
  columns <- list(
    track_id = character(), track_name = character(), track_index = integer(),
    step_id = character(), name = character(), start = numeric(),
    end = numeric(), resolved = logical()
  )
  if (length(rows) == 0) {
    return(as.data.frame(columns, stringsAsFactors = FALSE))
  }
  steps <- as.data.frame(rows, stringsAsFactors = FALSE)
  for (col in names(columns)) {
    if (is.null(steps[[col]])) steps[[col]] <- NA
    storage.mode(steps[[col]]) <- storage.mode(columns[[col]])
  }
  steps$duration <- steps$end - steps$start
  steps[c(names(columns)[1:7], "duration", "resolved")]
}
