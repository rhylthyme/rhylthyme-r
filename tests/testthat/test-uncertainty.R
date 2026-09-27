manual_demo <- function() read_rhylthyme(test_path("fixtures", "programs", "comprehensive_manual_demo.json"))
replicates <- function() read_rhylthyme(test_path("fixtures", "programs", "replicates_lab_samples.json"))

test_that("variable steps carry their range in the plotting data", {
  d <- ggplot2::fortify(read_rhylthyme(example_program()))
  heat <- d[d$step_id == "eggs-heat-pan", ]
  # starts at 60, may take 60 to 120 s
  expect_equal(c(heat$range_min, heat$range_max), c(120, 180))
  expect_true(all(is.na(d$range_min[d$duration_type == "fixed"])))
})

test_that("ranges are drawn for variable steps and can be hidden", {
  p <- read_rhylthyme(example_program())
  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme())
  n_variable <- sum(steps(p)$duration_type == "variable")
  expect_equal(length(g$ranges$children$whiskers$x0), 3 * n_variable)
  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme(ranges = FALSE))
  expect_null(g$ranges)
})

test_that("estimated lengths get a dotted outline", {
  p <- manual_demo()
  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme())
  expect_false(is.null(g$estimates))
  expect_null(panel_grob(ggplot2::ggplot(p) + geom_rhylthyme(estimates = FALSE))$estimates)
  expect_null(panel_grob(ggplot2::ggplot(read_rhylthyme(example_program())) + geom_rhylthyme())$estimates)
})

test_that("collapsing replicates gives one row per authored step in its own track", {
  p <- replicates()
  out <- collapse_replicates(ggplot2::fortify(p), p)
  d <- out$data
  expect_equal(d$step_id, c("label-tubes", "centrifuge", "analyze"))
  expect_equal(unique(d$track_id), "sample-processing")
  centrifuge <- d[d$step_id == "centrifuge", ]
  expect_equal(centrifuge$name, "Centrifuge \u00d73")
  expect_equal(centrifuge$n_instances, 3L)
  instances <- steps(p)[!is.na(steps(p)$instance_of), ]
  expect_equal(c(centrifuge$start, centrifuge$end), c(min(instances$start), max(instances$end)))
  pairs <- paste(out$dependencies$from_step, "->", out$dependencies$to_step)
  expect_setequal(pairs, c("label-tubes -> centrifuge", "centrifuge -> analyze"))
})

test_that("collapsed dependencies drop links between instances of one step", {
  p <- read_rhylthyme(test_path("fixtures", "programs", "compound_each_pairing.json"))
  out <- collapse_replicates(ggplot2::fortify(p), p)
  d <- out$dependencies
  expect_false(any(d$from_step == d$to_step))
  expect_false(anyDuplicated(d[c("from_step", "to_step", "relation")]) > 0)
  expect_true(all(c(d$from_step, d$to_step) %in% out$data$step_id))
  # digestion's only step was replicated, so the engine dropped the track;
  # the collapsed step goes back to it, between lysis and assembly.
  digest <- out$data[out$data$step_id == "digest", ]
  expect_equal(digest$track_id, "digestion")
  expect_equal(unique(out$data$track_id), c("lysis", "digestion", "assembly"))
})

test_that("collapse_replicates redraws the plot without touching the steps table", {
  p <- replicates()
  before <- steps(p)
  built <- ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme(collapse_replicates = TRUE))
  expect_equal(nrow(built), 3)
  expect_equal(length(unique(built$y)), 1)
  expect_identical(steps(p), before)
  expect_equal(nrow(ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme())), nrow(before))
})

test_that("collapse_replicates on plain data warns and does nothing", {
  df <- data.frame(s = 0, e = 1, lane = "a")
  plot <- ggplot2::ggplot(df, ggplot2::aes(xmin = s, xmax = e, y = lane))
  expect_warning(plot + geom_rhylthyme(collapse_replicates = TRUE), "needs a Rhylthyme program")
})

test_that("ranges, estimates and collapsed replicates look right", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("ranges and estimates", ggplot2::ggplot(manual_demo()) + geom_rhylthyme(colour = "white"))
  vdiffr::expect_doppelganger("replicates collapsed", ggplot2::ggplot(replicates()) + geom_rhylthyme(colour = "white", collapse_replicates = TRUE))
})
