estimates_program <- function() {
  list(
    schemaVersion = "0.2.0-alpha", programId = "estimates", name = "Estimates",
    resourceConstraints = list(),
    tracks = list(list(
      trackId = "bench", name = "Bench",
      steps = list(
        list(stepId = "fixed", name = "Fixed", duration = list(type = "fixed", seconds = 100),
             startTrigger = list(type = "programStart")),
        list(stepId = "shake", name = "Shake",
             instrument = list(tool = "shaker", command = "start_shake", params = list(duration = 300)),
             startTrigger = list(type = "afterStep", stepId = "fixed")),
        list(stepId = "read", name = "Read",
             instrument = list(tool = "reader", command = "start_read", params = list(plate = "p1")),
             startTrigger = list(type = "afterStep", stepId = "shake")),
        list(stepId = "wait", name = "Wait", duration = list(type = "indefinite", defaultSeconds = 45),
             startTrigger = list(type = "afterStep", stepId = "read")),
        list(stepId = "rest", name = "Rest",
             duration = list(type = "variable", minSeconds = 10, maxSeconds = 50, defaultSeconds = 20),
             startTrigger = list(type = "afterStep", stepId = "wait")),
        list(stepId = "planned", name = "Planned", duration = list(type = "fixed", seconds = 30),
             metadata = list(durationEstimate = TRUE),
             startTrigger = list(type = "afterStep", stepId = "rest"))
      )
    ))
  )
}

test_that("planning durations follow the web: instrument params, 60 s default, indefinite default", {
  s <- read_rhylthyme(program_file(estimates_program()))$steps
  expect_equal(s$duration, c(100, 300, 60, 45, 20, 30))
  expect_equal(s$start, c(0, 100, 400, 460, 505, 525))
})

test_that("each step says how its duration is known and whether it is an estimate", {
  s <- read_rhylthyme(program_file(estimates_program()))$steps
  expect_equal(s$duration_type, c("fixed", "instrument", "instrument", "indefinite", "variable", "fixed"))
  expect_equal(s$estimate, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
})

test_that("replicates expand to one row per instance with the engine's ids", {
  s <- read_rhylthyme(test_path("fixtures", "programs", "replicates_lab_samples.json"))$steps
  inst <- s[!is.na(s$instance_of), ]
  expect_equal(inst$step_id, c("centrifuge-r1", "centrifuge-r2", "centrifuge-r3"))
  expect_equal(inst$instance_of, rep("centrifuge", 3))
  expect_equal(inst$instance_index, 1:3)
  expect_true(all(is.na(s$instance_index[is.na(s$instance_of)])))
})

test_that("an empty program gives an empty table with every column", {
  s <- read_rhylthyme(program_file(list(name = "Empty", tracks = list())), validate = FALSE)$steps
  expect_equal(nrow(s), 0)
  expect_true(all(c("start", "end", "duration", "estimate", "instance_of") %in% names(s)))
})
