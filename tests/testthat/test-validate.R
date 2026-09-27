broken_breakfast <- function() {
  p <- jsonlite::read_json(example_program())
  p$tracks[[1]]$steps[[2]]$duration$type <- "fixd"
  p$tracks[[1]]$steps[[3]]$stepId <- NULL
  p$tracks[[2]]$steps[[1]]$startTrigger <- list(type = "afterStep")
  p$tracks[[3]]$steps[[1]]$startTrigger <- list(offsetSeconds = 5)
  p$name <- NULL
  p
}

test_that("a valid program has no problems", {
  expect_equal(nrow(validate_rhylthyme(example_program())), 0)
  expect_equal(nrow(validate_rhylthyme(read_rhylthyme(example_program()))), 0)
})

test_that("each problem is reported once, tied to its step", {
  problems <- validate_rhylthyme(broken_breakfast())
  expect_equal(nrow(problems), 5)
  expect_equal(problems$step_id, c(NA, "eggs-heat-pan", NA, "bacon-prep", "toast-cook"))
  expect_equal(problems$track_id, c(NA, "scrambled-eggs", "scrambled-eggs", "bacon", "toast"))
  expect_equal(problems$where, c("", "duration.type", "steps[3]", "startTrigger", "startTrigger"))
  expect_equal(problems$message, c(
    "must have required property 'name'",
    "must be one of: fixed, variable, indefinite",
    "must have required property 'stepId'",
    "must have required property 'stepId'",
    "must have required property 'type'"
  ))
  expect_equal(problems$path[2], "/tracks/0/steps/1/duration/type")
})

test_that("reading an invalid program lists every problem", {
  err <- expect_error(read_rhylthyme(broken_breakfast()), class = "rhylthyme_invalid")
  expect_match(conditionMessage(err), "not valid (5 problems)", fixed = TRUE)
  expect_match(conditionMessage(err), "step 'eggs-heat-pan', duration.type: must be one of: fixed, variable, indefinite", fixed = TRUE)
  expect_match(conditionMessage(err), "track 'scrambled-eggs', steps[3]: must have required property 'stepId'", fixed = TRUE)
  expect_match(conditionMessage(err), "validate = FALSE", fixed = TRUE)
  expect_equal(nrow(err$problems), 5)
})

test_that("a long list of problems is cut short in the error", {
  p <- jsonlite::read_json(example_program())
  for (i in seq_along(p$tracks)) {
    for (j in seq_along(p$tracks[[i]]$steps)) p$tracks[[i]]$steps[[j]]$name <- NULL
  }
  p$tracks <- rep(p$tracks, 5)
  err <- expect_error(read_rhylthyme(p), class = "rhylthyme_invalid")
  expect_match(conditionMessage(err), "not valid (30 problems)", fixed = TRUE)
  expect_match(conditionMessage(err), "... and 10 more; validate_rhylthyme() lists them all.", fixed = TRUE)
})

test_that("validate = FALSE reads and plots a draft", {
  p <- read_rhylthyme(broken_breakfast(), validate = FALSE)
  expect_equal(p$name, "breakfast-schedule")
  # The step with no stepId is not timed; the rest are.
  expect_equal(nrow(p$steps), 5)
  built <- ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme())
  expect_equal(nrow(built), 5)
})
