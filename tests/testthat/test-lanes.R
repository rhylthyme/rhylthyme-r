test_that("lanes = \"step\" gives every step its own lane and no bar labels", {
  p <- read_rhylthyme(example_program())
  plot <- ggplot2::ggplot(p) + geom_rhylthyme(lanes = "step")
  expect_equal(rlang::as_label(plot$layers[[1]]$mapping$y), "step_lane")
  expect_null(plot$layers[[1]]$mapping$label)
  expect_equal(length(unique(ggplot2::layer_data(plot)$y)), nrow(steps(p)))
  expect_null(panel_grob(plot)$labels)
})

test_that("lanes = \"task\" groups by resource and stacks concurrent use", {
  p <- read_rhylthyme(example_program())
  plot <- ggplot2::ggplot(p) + geom_rhylthyme(lanes = "task")
  built <- ggplot2::layer_data(plot)
  expect_equal(length(unique(built$y)), 3)
  d <- ggplot2::fortify(p)
  stove <- built[d$task == "stove-burner", ]
  # eggs-cook (150-300) and bacon-cook (60-660) share the stove: two sub-rows
  expect_equal(unique(stove$n_sub), 2L)
  expect_equal(unique(stove$ymax - stove$ymin), 0.4)
})

test_that("lanes is ignored, with a warning, when y is mapped or the data is plain", {
  p <- read_rhylthyme(example_program())
  expect_warning(ggplot2::ggplot(p, ggplot2::aes(y = task)) + geom_rhylthyme(lanes = "task"), "`lanes` is ignored")
  df <- data.frame(s = 0, e = 1, lane = "a")
  expect_warning(
    ggplot2::ggplot(df, ggplot2::aes(xmin = s, xmax = e, y = lane)) + geom_rhylthyme(lanes = "step"),
    "needs a Rhylthyme program"
  )
  expect_error(geom_rhylthyme(lanes = "resource"), "should be one of")
})

test_that("a plain data frame plots with labels and any aesthetic", {
  df <- data.frame(
    what = c("Mix", "Bake", "Cool"), from = c(0, 600, 2400), to = c(600, 2400, 3000),
    who = c("Ana", "Oven", "Ana"), kind = c("hands", "machine", "hands")
  )
  plot <- ggplot2::ggplot(df, ggplot2::aes(xmin = from, xmax = to, y = who, label = what, fill = kind)) +
    geom_rhylthyme()
  built <- ggplot2::layer_data(plot)
  expect_equal(built$xmin, df$from)
  expect_equal(length(unique(built$fill)), 2)
  expect_equal(panel_grob(plot)$labels$bars$label, df$what)
})

test_that("mapping fill (or colour, alpha, linetype) to a column gives a legend", {
  p <- read_rhylthyme(example_program())
  plot <- ggplot2::ggplot(p) + geom_rhylthyme(ggplot2::aes(fill = task))
  expect_setequal(ggplot2::get_guide_data(plot, "fill")$.label, c("prep-work", "stove-burner", "toaster"))
  plot <- ggplot2::ggplot(p) + geom_rhylthyme(ggplot2::aes(colour = duration_type, alpha = duration))
  built <- ggplot2::layer_data(plot)
  expect_equal(length(unique(built$colour)), 2)
  expect_false(is.null(ggplot2::get_guide_data(plot, "alpha")))
})

test_that("each lanes mode looks right", {
  skip_if_not_installed("vdiffr")
  p <- read_rhylthyme(example_program())
  vdiffr::expect_doppelganger("lanes step", ggplot2::ggplot(p) + geom_rhylthyme(colour = "white", lanes = "step"))
  vdiffr::expect_doppelganger("lanes task", ggplot2::ggplot(p) + geom_rhylthyme(colour = "white", lanes = "task"))
})
