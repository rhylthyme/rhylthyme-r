breakfast <- function(...) read_rhylthyme(example_program(), ...)
compound <- function() read_rhylthyme(test_path("fixtures", "programs", "compound_each_pairing.json"))

test_that("programs combine, labelled by argument name or program name", {
  both <- rhylthyme_programs(Saturday = breakfast(), Sunday = breakfast())
  expect_s3_class(both, "rhylthyme_programs")
  expect_equal(names(both$programs), c("Saturday", "Sunday"))
  same <- c(breakfast(), breakfast())
  expect_equal(names(same$programs), c("Breakfast Schedule", "Breakfast Schedule (2)"))
  three <- c(same, compound())
  expect_equal(length(three$programs), 3)
  expect_output(print(three), "<rhylthyme_programs> 3 programs")
  expect_error(rhylthyme_programs(breakfast(), 42), "takes programs")
})

test_that("tables stack with a program column", {
  both <- c(A = breakfast(), B = compound())
  s <- steps(both)
  expect_equal(names(s)[1], "program")
  expect_equal(nrow(s), nrow(steps(breakfast())) + nrow(steps(compound())))
  expect_equal(unique(s$program), c("A", "B"))
  expect_equal(nrow(dependencies(both)), nrow(dependencies(breakfast())) + nrow(dependencies(compound())))
  expect_equal(unique(tracks(both)$program), c("A", "B"))
  expect_equal(unique(resources(both)$program), c("A", "B"))
  expect_identical(tidy(both), s)
})

test_that("plotting data keeps step keys unique and each program's lanes its own", {
  d <- ggplot2::fortify(c(Saturday = breakfast(), Sunday = breakfast()))
  expect_false(anyDuplicated(d$step_key) > 0)
  expect_equal(levels(d$program), c("Saturday", "Sunday"))
  expect_equal(nlevels(d$lane), 6)
  # Saturday's first track is the top lane
  expect_equal(tail(levels(d$lane), 1), "Saturday\tScrambled Eggs")
  # tracks with the same name share one fill level
  expect_equal(levels(d$track), c("Scrambled Eggs", "Bacon", "Toast"))
  expect_equal(lane_labels(levels(d$lane))[1:3], c("Toast", "Bacon", "Scrambled Eggs"))
})

test_that("one program's plotting data is unchanged by the program column", {
  d <- ggplot2::fortify(breakfast())
  expect_equal(d$step_key, d$step_id)
  expect_equal(levels(d$lane), c("Toast", "Bacon", "Scrambled Eggs"))
  expect_equal(as.character(unique(d$program)), "Breakfast Schedule")
})

test_that("a set plots in one panel with every lane, or faceted by program", {
  both <- c(Saturday = breakfast(), Sunday = breakfast())
  built <- ggplot2::ggplot_build(ggplot2::ggplot(both) + geom_rhylthyme())
  expect_equal(length(unique(built$data[[1]]$y)), 6)
  expect_equal(built$layout$panel_scales_y[[1]]$get_labels()[1:3], c("Toast", "Bacon", "Scrambled Eggs"))
  faceted <- ggplot2::ggplot(both) + geom_rhylthyme() + ggplot2::facet_wrap(ggplot2::vars(program), scales = "free_y")
  data <- ggplot2::layer_data(faceted)
  expect_equal(length(unique(data$PANEL)), 2)
  expect_equal(as.vector(tapply(data$y, data$PANEL, function(y) length(unique(y)))), c(3, 3))
})

test_that("arrows never join steps of different programs", {
  one <- panel_grob(ggplot2::ggplot(compound()) + geom_rhylthyme())
  two <- panel_grob(ggplot2::ggplot(c(compound(), compound())) + geom_rhylthyme())
  expect_equal(length(unique(two$arrows$id)), 2 * length(unique(one$arrows$id)))
})

test_that("anchoring must be all or none", {
  mixed <- c(breakfast(), breakfast(start_at = "2026-11-26 08:00"))
  expect_error(ggplot2::fortify(mixed), "Anchor all the programs")
  both <- c(breakfast(start_at = "2026-11-26 08:00"), breakfast(start_at = "2026-11-26 09:00"))
  d <- ggplot2::fortify(both)
  expect_s3_class(d$start, "POSIXct")
  expect_equal(as.numeric(min(d$start[d$program == levels(d$program)[2]])),
               as.numeric(as.POSIXct("2026-11-26 09:00")))
})

test_that("replicates collapse within each program", {
  built <- ggplot2::layer_data(ggplot2::ggplot(c(compound(), compound())) + geom_rhylthyme(collapse_replicates = TRUE))
  single <- ggplot2::layer_data(ggplot2::ggplot(compound()) + geom_rhylthyme(collapse_replicates = TRUE))
  expect_equal(nrow(built), 2 * nrow(single))
})

test_that("a set of programs looks right", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("programs faceted", autoplot(c(Breakfast = breakfast(), Compound = compound())))
})
