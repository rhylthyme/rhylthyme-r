#' Read a Rhylthyme program
#'
#' Reads a Rhylthyme program (JSON), checks it against the program schema and
#' times every step with the same engine rhylthyme.com uses.
#'
#' @param x The program: a path to a JSON file, an `http(s)://` URL, a JSON
#'   string, or a list (as from [jsonlite::read_json()], or built in R; a
#'   length-one vector becomes a JSON scalar, so wrap a one-element array in
#'   [list()] or [I()]).
#' @param validate If `TRUE`, the default, a program that breaks the schema is
#'   an error listing every problem (see [validate_rhylthyme()]). `FALSE`
#'   reads it anyway, for drafts; steps are then timed as far as the engine
#'   can.
#' @return A `rhylthyme_program`: a list with the program's `name`,
#'   `description`, `schema_version`, `source`, a `steps` data frame (one row
#'   per step, with `start` and `end` in seconds from program start), the
#'   parsed `program` and its `json` text.
#' @examples
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' p
#' read_rhylthyme('{"programId": "tea", "name": "Tea", "resourceConstraints": [],
#'   "tracks": [{"trackId": "kettle", "name": "Kettle",
#'   "steps": [{"stepId": "boil", "name": "Boil", "duration": {"type": "fixed", "seconds": 180},
#'   "startTrigger": {"type": "programStart"}}]}]}')
#' @export
read_rhylthyme <- function(x, validate = TRUE) {
  src <- resolve_source(x)
  program <- parse_program(src$json)
  supported_version(program)
  if (isTRUE(validate)) stop_if_invalid(schema_problems(src$json, program))
  new_rhylthyme_program(src$json, program, source = src$source)
}

# Where a program comes from: a list, a JSON string, a URL or a file path.
# Returns the JSON text and a description of the source.
resolve_source <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
    return(list(json = as.character(json), source = "list"))
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`x` must be a path, URL or JSON string, or a list.", call. = FALSE)
  }
  if (grepl("^\\s*(\\[|\\{)", x)) {
    return(list(json = x, source = "string"))
  }
  if (grepl("^https?://", x, ignore.case = TRUE)) {
    return(list(json = fetch_url(x), source = x))
  }
  if (!file.exists(x)) {
    stop("No such file: ", x, call. = FALSE)
  }
  json <- paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  list(json = json, source = normalizePath(x))
}

fetch_url <- function(url) {
  response <- tryCatch(
    http_get(url),
    error = function(e) stop("Could not fetch ", url, ": ", conditionMessage(e), call. = FALSE)
  )
  if (response$status_code >= 400) {
    stop("Could not fetch ", url, ": HTTP ", response$status_code, call. = FALSE)
  }
  text <- rawToChar(response$content)
  Encoding(text) <- "UTF-8"
  text
}

http_get <- function(url) {
  handle <- curl::new_handle(
    timeout = 30, followlocation = TRUE,
    useragent = paste0("rhylthyme-r/", utils::packageVersion("rhylthyme"))
  )
  curl::handle_setheaders(handle, Accept = "application/json")
  curl::curl_fetch_memory(url, handle = handle)
}

parse_program <- function(json) {
  program <- tryCatch(
    jsonlite::parse_json(json, simplifyVector = FALSE),
    error = function(e) stop("Not valid JSON: ", conditionMessage(e), call. = FALSE)
  )
  if (!is.list(program) || is.null(names(program)) || is.null(program$tracks)) {
    stop("Not a Rhylthyme program: it has no `tracks`.", call. = FALSE)
  }
  program
}

new_rhylthyme_program <- function(json, program, source) {
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
