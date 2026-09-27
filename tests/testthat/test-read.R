test_that("read_rhylthyme times every step with the engine", {
  p <- read_rhylthyme(example_program())
  expect_s3_class(p, "rhylthyme_program")
  expect_equal(p$name, "Breakfast Schedule")
  expect_equal(p$schema_version, "0.1.0")

  s <- p$steps
  expect_equal(s$step_id, c("eggs-crack-whisk", "eggs-heat-pan", "eggs-cook",
                            "bacon-prep", "bacon-cook", "toast-cook"))
  # The engine's times for this fixture (rhylthyme-timeline's
  # test/fixtures/python-timings.json): a variable step plans at its
  # defaultSeconds, and toast waits for the bacon with a negative offset.
  expect_equal(s$start, c(0, 60, 150, 0, 60, 480))
  expect_equal(s$end, c(60, 150, 300, 60, 660, 690))
  expect_equal(s$duration, s$end - s$start)
  expect_true(all(s$resolved))
  expect_equal(unique(s$track_id), c("scrambled-eggs", "bacon", "toast"))
})

test_that("read_rhylthyme keeps the original JSON", {
  path <- example_program()
  p <- read_rhylthyme(path)
  expect_equal(p$json, paste(readLines(path, warn = FALSE), collapse = "\n"))
  expect_equal(p$program$programId, "breakfast-schedule")
})

test_that("read_rhylthyme explains bad input", {
  expect_error(read_rhylthyme(c("a", "b")), "path, URL or JSON string, or a list")
  expect_error(read_rhylthyme(42), "path, URL or JSON string, or a list")
  expect_error(read_rhylthyme(tempfile(fileext = ".json")), "No such file")
  bad <- withr::local_tempfile(fileext = ".json")
  writeLines("{not json", bad)
  expect_error(read_rhylthyme(bad), "Not valid JSON")
  writeLines('{"name": "no tracks"}', bad)
  expect_error(read_rhylthyme(bad), "no `tracks`")
})

test_that("printing a program summarises it", {
  p <- read_rhylthyme(example_program())
  expect_output(print(p), "Breakfast Schedule: 3 tracks, 6 steps, 11:30")
})
