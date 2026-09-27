test_that("print shows name, tracks, steps and length", {
  p <- read_rhylthyme(example_program())
  expect_output(print(p), "<rhylthyme_program> Breakfast Schedule: 3 tracks, 6 steps, 11:30", fixed = TRUE)
})

test_that("summary lists tracks, resources and the makespan", {
  s <- summary(read_rhylthyme(example_program()))
  expect_s3_class(s, "summary.rhylthyme_program")
  expect_equal(s$makespan, 690)
  expect_equal(s$n_steps, 6)
  out <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(out, "Breakfast Schedule (schema 0.1.0)", fixed = TRUE)
  expect_match(out, "Makespan 11:30: 3 tracks, 6 steps", fixed = TRUE)
  expect_match(out, "Toast           1 step   8:00 - 11:30", fixed = TRUE)
  expect_match(out, "toaster       max 1  (1 step)", fixed = TRUE)
})

test_that("summary counts estimated steps", {
  p <- read_rhylthyme(test_path("fixtures", "programs", "comprehensive_manual_demo.json"))
  s <- summary(p)
  expect_equal(s$n_estimated, sum(steps(p)$estimate))
  expect_gt(s$n_estimated, 0)
  expect_match(paste(capture.output(print(s)), collapse = "\n"), sprintf("(%d estimated)", s$n_estimated), fixed = TRUE)
})

test_that("the steps table has every documented column, typed", {
  s <- tidy(read_rhylthyme(example_program()))
  types <- vapply(s, typeof, "")
  expect_equal(types, c(
    track_id = "character", track_name = "character", track_index = "integer",
    step_id = "character", name = "character", task = "character",
    start = "double", end = "double", duration = "double", resolved = "logical",
    duration_type = "character", min_seconds = "double", max_seconds = "double",
    estimate = "logical", trigger_type = "character", instance_of = "character",
    instance_index = "integer"
  ))
  expect_equal(s$task, c("prep-work", "stove-burner", "stove-burner", "prep-work", "stove-burner", "toaster"))
  # eggs-heat-pan is variable 60-120 s, planned at its 90 s default.
  expect_equal(unlist(s[2, c("min_seconds", "max_seconds", "duration")], use.names = FALSE), c(60, 120, 90))
  expect_equal(s$trigger_type, c("programStart", "afterStep", "afterStep", "programStart", "afterStep", "programStartOffset"))
  expect_identical(as.data.frame(read_rhylthyme(example_program())), s)
  expect_identical(steps(read_rhylthyme(example_program())), s)
})

test_that("the steps table works with base and dplyr verbs", {
  s <- steps(read_rhylthyme(example_program()))
  by_track <- aggregate(duration ~ track_name, data = s, FUN = sum)
  expect_equal(by_track$duration[by_track$track_name == "Bacon"], 660)
  skip_if_not_installed("dplyr")
  n <- dplyr::count(s, task)
  expect_equal(n$n[n$task == "stove-burner"], 3)
})

test_that("dependencies follow the triggers, with relation, offset and logic", {
  d <- dependencies(read_rhylthyme(example_program()))
  expect_equal(d$from_step, c("eggs-crack-whisk", "eggs-heat-pan", "bacon-prep"))
  expect_equal(d$to_step, c("eggs-heat-pan", "eggs-cook", "bacon-cook"))
  expect_equal(unique(d$relation), "end_to_start")

  d <- dependencies(read_rhylthyme(test_path("fixtures", "programs", "compound_each_pairing.json")))
  combine <- d[d$to_step == "combine-r1", ]
  expect_setequal(combine$from_step, c("lyse-r1", "digest-r1"))
  expect_equal(unique(combine$logic), "all")
  expect_equal(d$offset[d$from_step == "thaw" & d$to_step == "digest-r2"], 900)

  d <- dependencies(read_rhylthyme(test_path("fixtures", "programs", "negative_offset_demo.json")))
  expect_true(any(d$offset < 0))
})

test_that("start-to-start, buffer and abort triggers get their own relation", {
  program <- jsonlite::read_json(example_program())
  program$tracks[[2]]$steps[[2]]$startTrigger <-
    list(type = "afterStep", stepId = "bacon-prep", event = "start", offsetSeconds = 30)
  program$tracks[[3]]$steps[[1]]$startTrigger <-
    list(type = "afterStepWithBuffer", stepId = "bacon-cook", bufferSeconds = 20, offsetSeconds = 10)
  program$tracks[[1]]$steps[[3]]$startTrigger <- list(type = "onAbort", stepId = "eggs-heat-pan")
  d <- dependencies(read_rhylthyme(program, validate = FALSE))
  row <- function(to) d[d$to_step == to, ]
  expect_equal(row("bacon-cook")$relation, "start_to_start")
  expect_equal(row("bacon-cook")$offset, 30)
  expect_equal(row("toast-cook")$relation, "end_to_start")
  expect_equal(row("toast-cook")$offset, 30)
  expect_equal(row("toast-cook")$trigger_type, "afterStepWithBuffer")
  expect_equal(row("eggs-cook")$relation, "on_abort")
})

test_that("dependencies name steps that exist, replicate instances included", {
  for (path in fixture_programs()) {
    p <- read_rhylthyme(path)
    d <- dependencies(p)
    expect_true(all(d$to_step %in% p$steps$step_id), info = basename(path))
  }
})

test_that("resources match the program's constraints, plus unconstrained tasks", {
  r <- resources(read_rhylthyme(example_program()))
  expect_equal(r$task, c("stove-burner", "prep-work", "toaster"))
  expect_equal(r$max_concurrent, c(2L, 2L, 1L))
  expect_equal(r$n_steps, c(3L, 2L, 1L))
  program <- jsonlite::read_json(example_program())
  program$tracks[[3]]$steps[[1]]$task <- "grill"
  r <- resources(read_rhylthyme(program))
  expect_equal(r$task[4], "grill")
  expect_true(is.na(r$max_concurrent[4]))
  expect_equal(r$n_steps[r$task == "toaster"], 0L)
})

test_that("tracks list every lane the engine lays out, with their span", {
  t <- tracks(read_rhylthyme(example_program()))
  expect_equal(t$track_id, c("scrambled-eggs", "bacon", "toast"))
  expect_equal(t$n_steps, c(3L, 2L, 1L))
  expect_equal(t$start, c(0, 0, 480))
  expect_equal(t$end, c(300, 660, 690))
  t <- tracks(read_rhylthyme(test_path("fixtures", "programs", "replicates_lab_samples.json")))
  expect_equal(t$parent_track_id, c(NA, rep("sample-processing", 3)))
  t <- tracks(read_rhylthyme(test_path("fixtures", "programs", "airport_batch_example.json")))
  expect_true(all(!is.na(t$template_id)))
})

test_that("a program converts back to its JSON", {
  for (path in fixture_programs()) {
    p <- read_rhylthyme(path)
    json <- jsonlite::toJSON(as.list(p), auto_unbox = TRUE, null = "null", digits = NA)
    # expect_equal: JSON has one number type, so a `1.0` in the file comes
    # back as the integer 1.
    expect_equal(jsonlite::parse_json(json, simplifyVector = FALSE), jsonlite::read_json(path), info = basename(path))
    expect_identical(read_rhylthyme(as.list(p))$steps, p$steps, info = basename(path))
  }
})

test_that("the engine and schema versions are reported", {
  expect_match(rhylthyme_engine_version(), "^[0-9]+\\.[0-9]+\\.[0-9]+")
  expect_true("0.3.0-alpha" %in% rhylthyme_schema_versions())
})
