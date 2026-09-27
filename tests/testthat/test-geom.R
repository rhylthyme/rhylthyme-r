test_that("fortify gives one row per step with track and lane factors", {
  p <- read_rhylthyme(example_program())
  d <- ggplot2::fortify(p)
  expect_equal(nrow(d), 6)
  expect_equal(levels(d$track), c("Scrambled Eggs", "Bacon", "Toast"))
  # Lanes are reversed so the first track is drawn at the top.
  expect_equal(levels(d$lane), c("Toast", "Bacon", "Scrambled Eggs"))
})

test_that("bars span each step's start to end, one lane per track", {
  p <- read_rhylthyme(example_program())
  built <- ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme())
  expect_equal(built$xmin, p$steps$start)
  expect_equal(built$xmax, p$steps$end)
  # Scrambled Eggs is the top lane (3 of 3), Toast the bottom (1).
  expect_equal(as.numeric(built$y), c(3, 3, 3, 2, 2, 1))
  expect_equal(built$ymax - built$ymin, rep(0.8, 6))
})

test_that("mappings the user gives win over the program defaults", {
  p <- read_rhylthyme(example_program())
  plot <- ggplot2::ggplot(p, ggplot2::aes(fill = name)) + geom_rhylthyme()
  mapped <- names(plot$layers[[1]]$mapping)
  expect_setequal(mapped, c("xmin", "xmax", "y"))
  plot <- ggplot2::ggplot(p) + geom_rhylthyme(ggplot2::aes(y = name))
  expect_equal(rlang::as_label(plot$layers[[1]]$mapping$y), "name")
})

test_that("geom = \"rhylthyme\" works in any layer, on any data", {
  df <- data.frame(a = c(0, 30), b = c(20, 50), who = c("x", "y"))
  plot <- ggplot2::ggplot(df, ggplot2::aes(xmin = a, xmax = b, y = who)) +
    ggplot2::layer(geom = "rhylthyme", stat = "identity", position = "identity")
  built <- ggplot2::layer_data(plot)
  expect_equal(built$xmin, c(0, 30))
  expect_equal(built$ymax - built$ymin, c(0.8, 0.8))

  plot <- ggplot2::ggplot(df, ggplot2::aes(xmin = a, xmax = b, y = who)) +
    ggplot2::layer(geom = "rhylthyme", stat = "rhylthyme", position = "identity")
  expect_equal(nrow(ggplot2::layer_data(plot)), 2)
})

test_that("a program plots as a timeline", {
  skip_if_not_installed("vdiffr")
  p <- read_rhylthyme(example_program())
  vdiffr::expect_doppelganger(
    "breakfast default",
    ggplot2::ggplot(p) + geom_rhylthyme(colour = "white")
  )
})
