breakfast_json <- function() {
  paste(readLines(example_program(), warn = FALSE), collapse = "\n")
}

serve <- function(body, status = 200L) {
  function(url) list(status_code = status, content = charToRaw(body))
}

test_that("a path, a JSON string, a list and a URL give the same program", {
  from_path <- read_rhylthyme(example_program())
  from_string <- read_rhylthyme(breakfast_json())
  from_list <- read_rhylthyme(jsonlite::read_json(example_program()))
  local_mocked_bindings(http_get = serve(breakfast_json()))
  from_url <- read_rhylthyme("https://example.org/breakfast.json")

  for (p in list(from_string, from_list, from_url)) {
    expect_equal(p$steps, from_path$steps)
    expect_equal(p$program, from_path$program)
    expect_equal(p$name, from_path$name)
  }
  expect_equal(from_string$source, "string")
  expect_equal(from_list$source, "list")
  expect_equal(from_url$source, "https://example.org/breakfast.json")
})

test_that("a JSON string may start with whitespace", {
  p <- read_rhylthyme(paste0("\n  ", breakfast_json()))
  expect_equal(nrow(p$steps), 6)
})

test_that("a program built in R reads", {
  p <- read_rhylthyme(list(
    programId = "tea", name = "Tea", resourceConstraints = list(),
    tracks = list(list(
      trackId = "kettle", name = "Kettle",
      steps = list(
        list(stepId = "boil", name = "Boil", duration = list(type = "fixed", seconds = 180),
             startTrigger = list(type = "programStart")),
        list(stepId = "steep", name = "Steep", duration = list(type = "fixed", seconds = "4m"),
             startTrigger = list(type = "afterStep", stepId = "boil"))
      )
    ))
  ))
  expect_equal(p$steps$start, c(0, 180))
  expect_equal(p$steps$end, c(180, 420))
})

test_that("URL failures say what went wrong", {
  local_mocked_bindings(http_get = serve("not found", status = 404L))
  expect_error(read_rhylthyme("https://example.org/missing.json"), "Could not fetch .*HTTP 404")
  local_mocked_bindings(http_get = function(url) stop("Could not resolve host"))
  expect_error(read_rhylthyme("https://nowhere.invalid/p.json"), "Could not fetch .*resolve host")
  local_mocked_bindings(http_get = serve("<html>hello</html>"))
  expect_error(read_rhylthyme("https://example.org/page"), "Not valid JSON")
})

test_that("every supported schema version reads", {
  versions <- vapply(fixture_programs(), function(f) {
    jsonlite::read_json(f)$schemaVersion %||% "(none)"
  }, "")
  for (v in unique(versions)) {
    path <- fixture_programs()[versions == v][1]
    p <- read_rhylthyme(path)
    expect_gt(nrow(p$steps), 0)
  }
  expect_true(all(c("0.1.0", "0.1.0-alpha", "0.2.0", "0.2.0-alpha", "0.3.0-alpha", "(none)") %in% versions))
})

test_that("an unsupported schema version is refused with the supported list", {
  program <- jsonlite::read_json(example_program())
  program$schemaVersion <- "0.9.0"
  expect_error(read_rhylthyme(program), "schemaVersion \"0.9.0\".*supported: 0.1.0-alpha, 0.1.0, 0.2.0, 0.2.0-alpha, 0.3.0-alpha")
  expect_error(read_rhylthyme(program, validate = FALSE), "does not support")
  program$schemaVersion <- 2
  expect_error(read_rhylthyme(program), "not a string")
  expect_equal(validate_rhylthyme(program)$where, "schemaVersion")
})
