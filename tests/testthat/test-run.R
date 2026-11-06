run_path <- function() test_path("fixtures", "runs", "thanksgiving_one_oven.run.json")
dinner <- function() read_rhylthyme(test_path("fixtures", "programs", "thanksgiving_one_oven.json"))

test_that("a run's actual times are the engine's run translation", {
  run <- read_rhylthyme_run(run_path(), program = dinner())
  expect_s3_class(run, "rhylthyme_run")
  s <- steps(run)
  record <- jsonlite::read_json(run_path())
  expect_equal(nrow(s), length(record$steps))
  engine_actual <- engine()$call("Rhylthyme.actualFromRun", record, dinner()$program)
  for (i in seq_len(nrow(s))) {
    a <- engine_actual[[s$step_id[i]]]
    expect_equal(c(s$start[i], s$end[i]), c(a$start, a$end), info = s$step_id[i])
  }
  expect_equal(s$planned_start[2], 1200)
  expect_equal(unique(s$status), "completed")
  expect_equal(run$outcome, "completed")
  expect_equal(run$run_id, record$runId)
})

test_that("the run is anchored at startedAt, or kept in elapsed seconds", {
  run <- read_rhylthyme_run(run_path(), program = dinner())
  expect_equal(as.numeric(run$started_at), as.numeric(as.POSIXct("2023-11-14 22:13:20", tz = "UTC")))
  s <- steps(run)
  expect_equal(as.numeric(s$start_time), as.numeric(run$started_at) + s$start)
  elapsed <- read_rhylthyme_run(run_path(), program = dinner(), clock = FALSE)
  expect_false("start_time" %in% names(steps(elapsed)))
  expect_equal(parse_iso_time("2023-11-15T01:13:20+03:00"), parse_iso_time("2023-11-14T22:13:20Z"))
})

test_that("with the program, steps get their track, name and task", {
  s <- steps(read_rhylthyme_run(run_path(), program = dinner()))
  p <- steps(dinner())
  expect_equal(s$track_id, p$track_id[match(s$step_id, p$step_id)])
  expect_equal(s$name[1], "Season and truss")
  bare <- steps(read_rhylthyme_run(run_path()))
  expect_equal(unique(bare$track_id), "run")
  expect_equal(bare$name, bare$step_id)
})

test_that("skipped, aborted, failed, unfinished and unstarted steps say so", {
  record <- jsonlite::read_json(run_path())
  record$outcome <- "aborted"
  record$steps[[1]]$instrument <- list(tool = "oven", command = "preheat", replies = list(
    list(attempt = 1L, at = 10, code = "DRIVER_ERROR", errorMessage = "door open"),
    list(attempt = 2L, at = 40, code = "DRIVER_ERROR")
  ))
  record$steps[[1]]$endedBy <- "skipped"
  record$steps[[2]]$endedBy <- "abort"
  record$steps[[3]]$actual <- list(start = 12400.5)
  record$steps[[3]]$endedBy <- NULL
  record$steps[[4]]$actual <- NULL
  record$steps[[4]]$endedBy <- NULL
  run <- read_rhylthyme_run(record, program = dinner())
  s <- steps(run)
  expect_equal(s$status[1:5], c("skipped", "aborted", "unfinished", "not_started", "completed"))
  expect_equal(s$failed[1:2], c(TRUE, FALSE))
  expect_equal(s$last_code[1], "DRIVER_ERROR")
  expect_true(is.na(s$start[4]))
  expect_output(print(run), "aborted, 13 steps")
  expect_output(print(run), "1 failed, 1 skipped, 1 aborted, 1 unfinished, 1 not started")
  # steps that never started, or never ended, are not drawn
  built <- ggplot2::layer_data(ggplot2::ggplot(run) + geom_rhylthyme())
  expect_equal(nrow(built), 11)
})

test_that("replicate instances map to the program's step ids", {
  p <- read_rhylthyme(test_path("fixtures", "programs", "replicates_lab_samples.json"))
  ps <- steps(p)
  entries <- lapply(seq_len(nrow(ps)), function(i) {
    base <- if (is.na(ps$instance_of[i])) ps$step_id[i] else ps$instance_of[i]
    list(stepId = base, instance = if (is.na(ps$instance_index[i])) 1L else ps$instance_index[i],
         planned = list(start = ps$start[i], end = ps$end[i], durationType = "fixed"),
         actual = list(start = ps$start[i] + 5, end = ps$end[i] + 5), endedBy = "timer")
  })
  record <- list(runId = "2026-01-01T09:00:00Z-ab12", programId = p$program$programId,
                 programVersion = paste0("sha256:", strrep("0", 64)),
                 runtime = list(kind = "cli", version = "0.3.0a0", clockMode = "wall"),
                 startedAt = "2026-01-01T09:00:00Z",
                 outcome = "completed", steps = entries)
  s <- steps(read_rhylthyme_run(record, program = p))
  expect_setequal(s$step_id, ps$step_id)
  expect_equal(s$start, ps$start[match(s$step_id, ps$step_id)] + 5)
  expect_equal(s$track_id, ps$track_id[match(s$step_id, ps$step_id)])
})

test_that("a run of another program warns; a broken record is refused", {
  other <- read_rhylthyme(example_program())
  expect_warning(read_rhylthyme_run(run_path(), program = other), "is of program 'thanksgiving-one-oven'")
  record <- jsonlite::read_json(run_path())
  record$outcome <- "finished"
  expect_error(read_rhylthyme_run(record), class = "rhylthyme_invalid")
  expect_error(read_rhylthyme_run(record), "run record is not valid (1 problem)", fixed = TRUE)
  expect_error(read_rhylthyme_run(record), "record, outcome: must be equal to one of the allowed values", fixed = TRUE)
  expect_s3_class(read_rhylthyme_run(record, validate = FALSE), "rhylthyme_run")
  expect_error(read_rhylthyme_run(list(steps = list())), "Not a Rhylthyme run record")
})

# The runs schema gained video timeline values on 2026-11-05: a record made
# from a video has runtime kind "video" and clockMode "media" (times are
# seconds into the video), and its steps are ended by "annotation" (a dataset
# label) or "observer" (a person marking while watching).
video_record <- function() {
  record <- jsonlite::read_json(run_path())
  record$runtime <- list(kind = "video", version = "0.1.0", clockMode = "media", speed = 1L)
  record$endedAt <- NULL
  record$steps[[1]]$endedBy <- "annotation"
  record$steps[[1]]$notes <- "Measurement error: too little seasoning"
  record$steps[[2]]$endedBy <- "observer"
  # A step that never happened in the video has no actual and no ending.
  record$steps[[3]]$actual <- NULL
  record$steps[[3]]$endedBy <- NULL
  record
}

test_that("a video timeline record is read: video runtime, media clock, label and observer ends", {
  record <- video_record()
  # validate = TRUE is the default: the record must pass the runs schema.
  run <- read_rhylthyme_run(record, program = dinner())
  s <- steps(run)
  expect_equal(s$ended_by[1:3], c("annotation", "observer", NA))
  expect_equal(s$status[1:4], c("completed", "completed", "not_started", "completed"))
  expect_equal(s$start[1:2], c(record$steps[[1]]$actual$start, record$steps[[2]]$actual$start))
  expect_equal(run$record$runtime$clockMode, "media")
  expect_equal(run$outcome, "completed")
})

test_that("unknown runtime and endedBy values in a video record are still refused", {
  bad <- list(
    list(c("runtime", "kind"), "videos", "runtime.kind"),
    list(c("runtime", "kind"), "Video", "runtime.kind"),
    list(c("runtime", "clockMode"), "Media", "runtime.clockMode"),
    list(c("runtime", "clockMode"), "frames", "runtime.clockMode"),
    list(c("steps", "1", "endedBy"), "label", "steps[1].endedBy"),
    list(c("steps", "2", "endedBy"), "Observer", "steps[2].endedBy")
  )
  # The unchanged record is valid, so each refusal is about the one value changed.
  expect_s3_class(read_rhylthyme_run(video_record()), "rhylthyme_run")
  for (b in bad) {
    record <- video_record()
    path <- b[[1]]
    if (path[1] == "steps") {
      record$steps[[as.integer(path[2])]][[path[3]]] <- b[[2]]
    } else {
      record[[path[1]]][[path[2]]] <- b[[2]]
    }
    expect_error(read_rhylthyme_run(record), class = "rhylthyme_invalid", info = b[[2]])
    expect_error(read_rhylthyme_run(record), "run record is not valid (1 problem)", fixed = TRUE, info = b[[2]])
    expect_error(
      read_rhylthyme_run(record),
      sprintf("record, %s: must be equal to one of the allowed values", b[[3]]),
      fixed = TRUE, info = b[[2]]
    )
  }
})

test_that("a run plots its actual timeline, alone or with others", {
  run <- read_rhylthyme_run(run_path(), program = dinner())
  plot <- ggplot2::ggplot(run) + geom_rhylthyme()
  built <- ggplot2::layer_data(plot)
  expect_equal(nrow(built), 13)
  expect_equal(max(built$xmax), as.numeric(run$started_at) + max(steps(run)$end))
  expect_s3_class(ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]], "ScaleContinuousDatetime")
  both <- c(Planned = dinner(), Actual = read_rhylthyme_run(run_path(), program = dinner(), clock = FALSE))
  expect_equal(levels(ggplot2::fortify(both)$program), c("Planned", "Actual"))
})

test_that("a run looks right", {
  skip_if_not_installed("vdiffr")
  # Clock times display in the session's time zone; pin it so the drawing is
  # the same on every machine.
  withr::local_timezone("UTC")
  vdiffr::expect_doppelganger("run with program", autoplot(read_rhylthyme_run(run_path(), program = dinner())))
})
