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
  expect_setequal(mapped, c("xmin", "xmax", "y", "step", "label"))
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

panel_grob <- function(plot) {
  grob <- ggplot2::layer_grob(plot)[[1]]
  names <- vapply(grob$children, function(g) g$name, "")
  stats::setNames(grob$children, names)
}

test_that("a program's dependencies between lanes are drawn as arrows", {
  p <- read_rhylthyme(test_path("fixtures", "programs", "compound_each_pairing.json"))
  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme())
  expect_s3_class(g$arrows, "polyline")
  lanes <- ggplot2::fortify(p)
  lane_of <- stats::setNames(as.character(lanes$lane), lanes$step_id)
  d <- dependencies(p)
  crossing <- sum(lane_of[d$from_step] != lane_of[d$to_step])
  expect_equal(length(unique(g$arrows$id)), crossing)

  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme(arrows = FALSE))
  expect_null(g$arrows)
})

test_that("labels are the step names, and can be turned off", {
  p <- read_rhylthyme(example_program())
  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme())
  expect_s3_class(g$labels, "rhylthyme_labels")
  expect_equal(g$labels$bars$label, p$steps$name)
  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme(labels = FALSE))
  expect_null(g$labels)
  g <- panel_grob(ggplot2::ggplot(p) + geom_rhylthyme(ggplot2::aes(label = step_id)))
  expect_equal(g$labels$bars$label, p$steps$step_id)
})

test_that("your own data can bring its own dependencies", {
  df <- data.frame(id = c("a", "b"), s = c(0, 20), e = c(10, 30), lane = c("x", "y"))
  plot <- ggplot2::ggplot(df, ggplot2::aes(xmin = s, xmax = e, y = lane, step = id, label = id)) +
    geom_rhylthyme(dependencies = data.frame(from_step = "a", to_step = "b", relation = "end_to_start", offset = 10))
  g <- panel_grob(plot)
  expect_equal(length(unique(g$arrows$id)), 1)
})

test_that("arrows and labels look right", {
  skip_if_not_installed("vdiffr")
  p <- read_rhylthyme(test_path("fixtures", "programs", "compound_each_pairing.json"))
  vdiffr::expect_doppelganger("compound arrows and labels", ggplot2::ggplot(p) + geom_rhylthyme(colour = "white"))
  vdiffr::expect_doppelganger(
    "compound plain",
    ggplot2::ggplot(p) + geom_rhylthyme(colour = "white", arrows = FALSE, labels = FALSE)
  )
  q <- read_rhylthyme(test_path("fixtures", "programs", "negative_offset_demo.json"))
  vdiffr::expect_doppelganger("negative offset hand-off", ggplot2::ggplot(q) + geom_rhylthyme(colour = "white"))
})
