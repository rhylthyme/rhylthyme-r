#' Read a Rhylthyme program
#'
#' Reads a Rhylthyme program (JSON) and times every step with the same
#' engine rhylthyme.com uses.
#'
#' @param x Path to a program JSON file.
#' @return A `rhylthyme_program`: a list with the program's `name`,
#'   `description`, `schema_version`, `source`, a `steps` data frame (one row
#'   per step, with `start` and `end` in seconds from program start), the
#'   parsed `program` and its original `json` text.
#' @examples
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' p
#' @export
read_rhylthyme <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`x` must be the path to a Rhylthyme program JSON file.", call. = FALSE)
  }
  if (!file.exists(x)) {
    stop("No such file: ", x, call. = FALSE)
  }
  json <- paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  new_rhylthyme_program(json, source = normalizePath(x))
}

new_rhylthyme_program <- function(json, source) {
  program <- tryCatch(
    jsonlite::parse_json(json, simplifyVector = FALSE),
    error = function(e) stop("Not valid JSON: ", conditionMessage(e), call. = FALSE)
  )
  if (!is.list(program) || is.null(program$tracks)) {
    stop("Not a Rhylthyme program: it has no `tracks`.", call. = FALSE)
  }
  structure(
    list(
      name = program$name %||% program$programId %||% NA_character_,
      description = program$description %||% NA_character_,
      schema_version = program$schemaVersion %||% NA_character_,
      source = source,
      steps = engine_steps(json),
      program = program,
      json = json
    ),
    class = "rhylthyme_program"
  )
}

#' @export
print.rhylthyme_program <- function(x, ...) {
  steps <- x$steps
  total <- if (nrow(steps)) max(steps$end, na.rm = TRUE) else 0
  cat(sprintf(
    "<rhylthyme_program> %s: %d tracks, %d steps, %s\n",
    x$name, length(unique(steps$track_id)), nrow(steps), format_elapsed(total)
  ))
  invisible(x)
}

format_elapsed <- function(seconds) {
  seconds <- round(seconds)
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  ifelse(h > 0, sprintf("%d:%02d:%02d", h, m, s), sprintf("%d:%02d", m, s))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
