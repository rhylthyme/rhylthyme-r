test_that("the corpus and the engine's timings cover the same programs", {
  expect_setequal(basename(fixture_programs()), names(engine_timings()))
  expect_gte(length(fixture_programs()), 40)
})

test_that("every step of every corpus program is timed exactly as the engine times it", {
  expected <- engine_timings()
  mismatches <- character()
  compared <- 0
  for (path in fixture_programs()) {
    name <- basename(path)
    steps <- read_rhylthyme(path)$steps
    want <- expected[[name]]
    if (!setequal(steps$step_id, names(want)) || anyDuplicated(steps$step_id)) {
      mismatches <- c(mismatches, sprintf(
        "%s: steps %s, engine %s", name,
        paste(steps$step_id, collapse = ","), paste(names(want), collapse = ",")
      ))
      next
    }
    for (i in seq_len(nrow(steps))) {
      w <- want[[steps$step_id[i]]]
      got <- c(steps$track_id[i], steps$start[i], steps$end[i], steps$resolved[i])
      exp <- c(w$trackId, w$start, w$end, w$resolved)
      if (!identical(as.character(got), as.character(exp))) {
        mismatches <- c(mismatches, sprintf(
          "%s:%s R=%s engine=%s", name, steps$step_id[i],
          paste(got, collapse = "/"), paste(exp, collapse = "/")
        ))
      }
      compared <- compared + 1
    }
  }
  expect_equal(mismatches, character())
  expect_gt(compared, 600)
})

test_that("every corpus program plots", {
  for (path in fixture_programs()) {
    p <- read_rhylthyme(path)
    built <- ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme())
    expect_equal(nrow(built), nrow(p$steps), info = basename(path))
    expect_true(all(is.finite(built$xmin) & is.finite(built$xmax)), info = basename(path))
  }
})

test_that("every trigger kind in the corpus is exercised", {
  kinds <- character()
  collect <- function(trigger) {
    if (!is.list(trigger)) return()
    kinds <<- c(kinds, trigger$type %||% character())
    if (!is.null(trigger$offsetSeconds) && is.numeric(trigger$offsetSeconds) &&
        trigger$offsetSeconds < 0) kinds <<- c(kinds, "negative-offset")
    for (t in trigger$triggers %||% list()) collect(t)
  }
  for (path in fixture_programs()) {
    program <- read_rhylthyme(path)$program
    for (track in c(program$tracks, program$trackTemplates)) {
      for (step in track$steps) collect(step$startTrigger)
    }
  }
  expect_true(all(c("programStart", "programStartOffset", "afterStep",
                    "afterStepWithBuffer", "manual", "negative-offset") %in% kinds))
})
