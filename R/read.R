#' Read a Rhylthyme program
#'
#' Reads a Rhylthyme program (JSON), checks it against the program schema and
#' times every step with the same engine rhylthyme.com uses.
#'
#' @param x The program: a path to a JSON file, an `http(s)://` URL (a
#'   rhylthyme.com program page, `.../p/<id>`, or share link, `...?share=<id>`,
#'   is read through the site's public API), a JSON string, or a list (as from [jsonlite::read_json()], or built in R; a
#'   length-one vector becomes a JSON scalar, so wrap a one-element array in
#'   [list()] or [I()]).
#' @param validate If `TRUE`, the default, a program that breaks the schema is
#'   an error listing every problem (see [validate_rhylthyme()]). `FALSE`
#'   reads it anyway, for drafts; steps are then timed as far as the engine
#'   can.
#' @param start_at,finish_at Anchor the program to the clock: when it starts,
#'   or when its last step should end ("everything ready at 6 pm"). A
#'   `POSIXct` or a string [as.POSIXct()] understands, in the local time zone.
#'   Give one at most. [steps()] then adds `start_time` and `end_time`, and
#'   plots have a clock-time axis.
#' @return A `rhylthyme_program`: a list with the program's `name`,
#'   `description`, `schema_version` and `source`, the `steps`, `tracks` and
#'   `dependencies` tables (see [steps()]), the parsed `program` and its
#'   `json` text, and `start_at` when anchored. Times are seconds from
#'   program start.
#' @examples
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
#' p
#' read_rhylthyme('{"programId": "tea", "name": "Tea", "resourceConstraints": [],
#'   "tracks": [{"trackId": "kettle", "name": "Kettle",
#'   "steps": [{"stepId": "boil", "name": "Boil", "duration": {"type": "fixed", "seconds": 180},
#'   "startTrigger": {"type": "programStart"}}]}]}')
#' # Everything ready at 6 pm
#' p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"),
#'   finish_at = "2026-11-26 18:00")
#' steps(p)[c("name", "start_time", "end_time")]
#' @export
read_rhylthyme <- function(x, validate = TRUE, start_at = NULL, finish_at = NULL) {
  if (!is.null(start_at) && !is.null(finish_at)) {
    stop("Give `start_at` or `finish_at`, not both.", call. = FALSE)
  }
  src <- resolve_source(x)
  program <- parse_program(src$json)
  supported_version(program)
  if (isTRUE(validate)) stop_if_invalid(schema_problems(src$json, program))
  p <- new_rhylthyme_program(src$json, program, source = src$source)
  if (!is.null(start_at)) p$start_at <- as_clock_time(start_at, "start_at")
  if (!is.null(finish_at)) p$start_at <- as_clock_time(finish_at, "finish_at") - makespan(p)
  p
}

as_clock_time <- function(x, arg) {
  if (inherits(x, "POSIXct") && length(x) == 1 && !is.na(x)) return(x)
  value <- if (is.character(x) && length(x) == 1) {
    tryCatch(as.POSIXct(x), error = function(e) NA)
  } else {
    NA
  }
  if (length(value) != 1 || is.na(value)) {
    stop(sprintf("`%s` must be a date-time: a POSIXct or a string such as \"2026-11-26 18:00\".", arg),
         call. = FALSE)
  }
  value
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
    api <- rhylthyme_link_api(x)
    if (!is.null(api)) return(list(json = fetch_link(x, api), source = x))
    return(list(json = fetch_url(x), source = x))
  }
  if (!file.exists(x)) {
    stop("No such file: ", x, call. = FALSE)
  }
  json <- paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  list(json = json, source = normalizePath(x))
}

# rhylthyme.com links (any vertical: kitchen., lab., www. ...; and
# appistry.com, the same site): a public program page /p/<id>, or a share
# link ?share=<id>. Returns the public API URL on the same site that serves
# the program's JSON, or NULL for any other URL.
rhylthyme_link_api <- function(url) {
  parts <- regmatches(url, regexec("^(https?://([^/?#:]+)(:[0-9]+)?)([^?#]*)(\\?[^#]*)?", url, ignore.case = TRUE))[[1]]
  if (!length(parts)) return(NULL)
  host <- tolower(parts[3])
  if (!grepl("(^|\\.)(rhylthyme|appistry)\\.com$", host)) return(NULL)
  origin <- parts[2]
  share <- regmatches(parts[6], regexec("[?&]share=([0-9a-fA-F]+)", parts[6]))[[1]]
  if (length(share)) return(paste0(origin, "/api/public/shares/", tolower(share[2])))
  page <- regmatches(parts[5], regexec("^/p/([0-9a-fA-F-]{36})/?$", parts[5]))[[1]]
  if (length(page)) return(paste0(origin, "/api/public/programs/", page[2]))
  NULL
}

# The program JSON behind a rhylthyme.com link, via its public API.
fetch_link <- function(link, api) {
  body <- tryCatch(fetch_url(api), error = function(e) {
    if (grepl("HTTP 404", conditionMessage(e))) {
      stop("That rhylthyme.com program is not public, or does not exist: ", link, call. = FALSE)
    }
    stop(sub(api, link, conditionMessage(e), fixed = TRUE), call. = FALSE)
  })
  row <- tryCatch(jsonlite::parse_json(body, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(row) || !is.list(row$program_json)) {
    stop("rhylthyme.com did not return a program for ", link, call. = FALSE)
  }
  as.character(jsonlite::toJSON(row$program_json, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
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
  tables <- engine_describe(json)
  structure(
    list(
      name = program$name %||% program$programId %||% NA_character_,
      description = program$description %||% NA_character_,
      schema_version = program$schemaVersion %||% NA_character_,
      source = source,
      steps = tables$steps,
      tracks = tables$tracks,
      dependencies = tables$dependencies,
      program = program,
      json = json
    ),
    class = "rhylthyme_program"
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
