# Schema checks. Every supported schemaVersion is validated against the
# 0.3.0-alpha program schema (a copy of rhylthyme-spec's, in inst/schemas):
# it is a superset, and its own schemaVersion description says programs
# declaring 0.1.0, 0.2.0 or 0.2.0-alpha remain valid against it. The MCP
# server validates the same way. A program with no schemaVersion is read as
# 0.1.0, as the rhylthyme.com viewer does.

PROGRAM_SCHEMA <- "program_schema_0.3.0-alpha.json"
SCHEMA_VERSIONS <- c("0.1.0-alpha", "0.1.0", "0.2.0", "0.2.0-alpha", "0.3.0-alpha")

#' Check a Rhylthyme program against the schema
#'
#' Lists every problem the program schema finds, each tied to the step (or
#' track) it is in. [read_rhylthyme()] runs the same check and stops on the
#' first invalid program; use this to see all the problems as a table.
#'
#' @param x A program: anything [read_rhylthyme()] accepts, or a
#'   `rhylthyme_program`.
#' @return A data frame with one row per problem (none when the program is
#'   valid): `track_id`, `step_id` (`NA` where the problem is not inside a
#'   track or step, or the step has no id), `where` (the field, relative to
#'   the step or track), `message` and `path` (a JSON pointer into the
#'   program).
#' @examples
#' validate_rhylthyme('{"name": "Draft", "tracks": [{"trackId": "a", "steps": [{"name": "x"}]}]}')
#' @export
validate_rhylthyme <- function(x) {
  src <- if (inherits(x, "rhylthyme_program")) list(json = x$json) else resolve_source(x)
  program <- parse_program(src$json)
  version <- program$schemaVersion
  if (!is.null(version) && !(is.character(version) && length(version) == 1 && version %in% SCHEMA_VERSIONS)) {
    return(problems_frame(NA, NA, "schemaVersion", unsupported_version_message(version), "/schemaVersion"))
  }
  schema_problems(src$json, program)
}

supported_version <- function(program) {
  version <- program$schemaVersion
  if (is.null(version)) return("0.1.0")
  if (is.character(version) && length(version) == 1 && version %in% SCHEMA_VERSIONS) {
    return(version)
  }
  stop(unsupported_version_message(version), call. = FALSE)
}

unsupported_version_message <- function(version) {
  shown <- if (is.character(version) && length(version) == 1) {
    sprintf("schemaVersion \"%s\"", version)
  } else {
    "a schemaVersion that is not a string"
  }
  sprintf(
    "This program declares %s, which rhylthyme %s does not support (supported: %s). A newer version of the package may.",
    shown, utils::packageVersion("rhylthyme"), paste(SCHEMA_VERSIONS, collapse = ", ")
  )
}

stop_if_invalid <- function(problems, what = "program") {
  if (!nrow(problems)) return(invisible())
  lines <- problem_lines(problems, top = if (what == "program") "program" else "record")
  shown <- utils::head(lines, 20)
  more <- length(lines) - length(shown)
  message <- paste0(
    sprintf("The %s is not valid (%d problem%s):\n", what, nrow(problems), if (nrow(problems) == 1) "" else "s"),
    paste0("* ", shown, collapse = "\n"),
    if (more > 0) sprintf("\n... and %d more; validate_rhylthyme() lists them all.", more) else "",
    "\nUse `validate = FALSE` to read it anyway."
  )
  stop(structure(
    class = c("rhylthyme_invalid", "error", "condition"),
    list(message = message, call = NULL, problems = problems)
  ))
}

problem_lines <- function(problems, top = "program") {
  where <- ifelse(is.na(problems$where) | problems$where == "", "", paste0(", ", problems$where))
  owner <- ifelse(
    !is.na(problems$step_id), sprintf("step '%s'", problems$step_id),
    ifelse(!is.na(problems$track_id), sprintf("track '%s'", problems$track_id), top)
  )
  paste0(owner, where, ": ", problems$message)
}

# A compiled validator and the parsed schema for one of the vendored schema
# files, cached for the session.
schema_validator <- function(file) {
  cache <- .rhylthyme_env$schemas %||% list()
  if (is.null(cache[[file]])) {
    path <- system.file("schemas", file, package = "rhylthyme", mustWork = TRUE)
    text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    cache[[file]] <- list(
      validator = jsonvalidate::json_schema$new(text, engine = "ajv"),
      schema = jsonlite::parse_json(text, simplifyVector = FALSE)
    )
    .rhylthyme_env$schemas <- cache
  }
  cache[[file]]
}

# Problems `json` (parsed as `data`) has against a vendored schema, one per
# mistake, located by track and step.
schema_problems <- function(json, data, file = PROGRAM_SCHEMA) {
  compiled <- schema_validator(file)
  result <- compiled$validator$validate(json, verbose = TRUE)
  if (isTRUE(result)) return(problems_frame())
  program <- data
  errors <- condense_errors(attr(result, "errors"), program, compiled$schema)
  located <- lapply(errors$instancePath, locate, program = program)
  problems_frame(
    vapply(located, `[[`, "", "track_id"),
    vapply(located, `[[`, "", "step_id"),
    vapply(located, `[[`, "", "where"),
    errors$message,
    errors$instancePath
  )
}

problems_frame <- function(track_id = character(), step_id = character(),
                           where = character(), message = character(),
                           path = character()) {
  data.frame(
    track_id = as.character(track_id), step_id = as.character(step_id),
    where = as.character(where), message = as.character(message),
    path = as.character(path), stringsAsFactors = FALSE
  )
}

# ajv reports a failure in every branch of a oneOf (a duration is fixed,
# variable or indefinite; a trigger is one of six kinds), so one mistake
# becomes a dozen errors. Keep the branch the author meant: the one whose
# `type` constraint accepts the object's type (else the untyped branch, for
# an object with no type), fewest errors first. When no branch accepts the
# type, say which types are allowed.
condense_errors <- function(errors, program, schema) {
  errors <- errors[!errors$keyword %in% c("oneOf", "anyOf"), , drop = FALSE]
  branch_re <- "^(.*(oneOf|anyOf)/[0-9]+).*$"
  branch <- ifelse(grepl(branch_re, errors$schemaPath), sub(branch_re, "\\1", errors$schemaPath), NA)
  is_type_error <- errors$keyword %in% c("enum", "const") & grepl("/type$", errors$instancePath)
  object <- ifelse(is_type_error, sub("/type$", "", errors$instancePath), errors$instancePath)
  allowed <- allowed_values(errors$params)

  keep <- is.na(branch)
  extra <- list()
  for (p in unique(object[!is.na(branch)])) {
    here <- which(!is.na(branch) & object == p)
    mismatched <- unique(branch[here][is_type_error[here]])
    candidates <- setdiff(unique(branch[here]), mismatched)
    typed <- Filter(function(b) has_type_constraint(schema_at(schema, b)), candidates)
    node <- data_at(program, p)
    data_type <- if (is.list(node)) node$type else NULL
    pool <- if (length(typed)) typed else if (is.null(data_type)) candidates else character()
    if (length(pool)) {
      counts <- vapply(pool, function(b) sum(branch[here] == b), 0)
      keep[here[branch[here] == pool[which.min(counts)]]] <- TRUE
    } else {
      types <- unique(unlist(allowed[here][is_type_error[here]]))
      extra[[length(extra) + 1]] <- data.frame(
        instancePath = paste0(p, "/type"),
        message = sprintf("must be one of: %s", paste(types, collapse = ", ")),
        stringsAsFactors = FALSE
      )
    }
  }
  kept <- errors[keep, c("instancePath", "message"), drop = FALSE]
  out <- do.call(rbind, c(list(kept), extra))
  out <- out[!duplicated(out), , drop = FALSE]
  out[order(out$instancePath), , drop = FALSE]
}

allowed_values <- function(params) {
  n <- nrow(params)
  lapply(seq_len(n), function(i) {
    values <- if (!is.null(params$allowedValues)) params$allowedValues[[i]] else NULL
    if (is.null(values) && !is.null(params$allowedValue)) values <- params$allowedValue[[i]]
    as.character(unlist(values))
  })
}

has_type_constraint <- function(node) {
  type <- node$properties$type
  !is.null(type) && (!is.null(type$enum) || !is.null(type$const))
}

# Follow a JSON pointer ("#/definitions/x/oneOf/0") into the parsed schema,
# resolving a $ref at the end.
schema_at <- function(schema, pointer) {
  node <- walk_pointer(schema, sub("^#", "", pointer))
  if (!is.null(node[["$ref"]])) node <- schema_at(schema, node[["$ref"]])
  node
}

data_at <- function(program, pointer) walk_pointer(program, pointer)

walk_pointer <- function(node, pointer) {
  parts <- strsplit(sub("^/", "", pointer), "/", fixed = TRUE)[[1]]
  for (part in parts) {
    part <- gsub("~0", "~", gsub("~1", "/", part, fixed = TRUE), fixed = TRUE)
    if (!is.list(node)) return(NULL)
    node <- if (is.null(names(node)) && grepl("^[0-9]+$", part)) {
      if (as.integer(part) + 1 <= length(node)) node[[as.integer(part) + 1]] else NULL
    } else {
      node[[part]]
    }
    if (is.null(node)) return(NULL)
  }
  node
}

# The track and step a JSON pointer is in, and the rest of the pointer as a
# readable field ("duration.type", "steps[3]"). Tracks are named by trackId
# (or templateId); a step with no stepId by its position in the track.
locate <- function(pointer, program) {
  parts <- if (pointer == "") character() else strsplit(sub("^/", "", pointer), "/", fixed = TRUE)[[1]]
  track_id <- NA_character_
  step_id <- NA_character_
  node <- program
  i <- 1
  while (i < length(parts) && grepl("^[0-9]+$", parts[i + 1])) {
    key <- parts[i]
    index <- as.integer(parts[i + 1]) + 1
    if (key %in% c("tracks", "trackTemplates")) {
      track <- node[[key]][[index]]
      track_id <- as.character(track$trackId %||% track$templateId %||% sprintf("%s[%d]", key, index))
      node <- track
    } else if (key == "steps" && !is.na(track_id)) {
      step <- node$steps[[index]]
      step_id <- if (is.null(step$stepId)) NA_character_ else as.character(step$stepId)
      if (is.na(step_id)) {
        rest <- parts[-seq_len(i + 1)]
        return(list(track_id = track_id, step_id = NA_character_,
                    where = field_path(c(sprintf("steps[%d]", index), rest))))
      }
      node <- step
    } else {
      break
    }
    i <- i + 2
  }
  list(track_id = track_id, step_id = step_id, where = field_path(parts[seq_along(parts) >= i]))
}

field_path <- function(parts) {
  if (!length(parts)) return("")
  index <- grepl("^[0-9]+$", parts)
  parts[index] <- sprintf("[%d]", as.integer(parts[index]) + 1)
  sub("^\\.", "", gsub("\\.\\[", "[", paste0(".", parts, collapse = "")))
}
