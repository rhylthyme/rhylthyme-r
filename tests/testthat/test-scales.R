thanksgiving <- function(...) read_rhylthyme(test_path("fixtures", "programs", "thanksgiving_one_oven.json"), ...)

x_scale <- function(plot) ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]

test_that("the palettes are the engine's", {
  pal <- rhylthyme_palettes()
  expect_true(all(c("classic", "vivid", "muted", "tableau", "google", "pastel", "d3",
                    "cookbook", "okabe-ito", "grayscale") %in% names(pal)))
  expect_equal(pal$classic, c("#3b82f6", "#10b981", "#f59e0b", "#ef4444",
                              "#8b5cf6", "#ec4899", "#14b8a6", "#f97316"))
  expect_identical(pal, lapply(engine()$get("Rhylthyme.PALETTES"), as.character))
})

test_that("a program is filled by track in the classic palette by default", {
  p <- read_rhylthyme(example_program())
  built <- ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme())
  by_track <- unique(built$fill[order(match(steps(p)$track_id, unique(steps(p)$track_id)))])
  expect_equal(by_track, rhylthyme_palettes()$classic[1:3])
})

test_that("any palette can be chosen, and colours repeat past the end as on the web", {
  p <- read_rhylthyme(example_program())
  built <- ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme() + scale_fill_rhylthyme("cookbook"))
  expect_true(all(built$fill %in% rhylthyme_palettes()$cookbook))
  expect_error(scale_fill_rhylthyme("rainbow"), "must be one of: classic")
  s <- scale_fill_rhylthyme("grayscale")
  s$train(letters[1:8])
  expect_equal(unname(s$map(letters[7:8])), rhylthyme_palettes()$grayscale[1:2])
  expect_s3_class(scale_colour_rhylthyme("vivid"), "ScaleDiscrete")
  expect_identical(scale_color_rhylthyme, scale_colour_rhylthyme)
})

test_that("a user fill scale replaces the default without a message", {
  p <- read_rhylthyme(example_program())
  expect_no_message(ggplot2::ggplot_build(ggplot2::ggplot(p) + geom_rhylthyme() + ggplot2::scale_fill_grey()))
  expect_no_message(ggplot2::ggplot_build(ggplot2::ggplot(p) + geom_rhylthyme() + ggplot2::scale_x_continuous()))
})

test_that("elapsed breaks are round and labels read m:ss or h:mm", {
  expect_equal(elapsed_breaks(c(0, 690)), seq(0, 600, by = 120))
  expect_equal(elapsed_breaks(c(0, 14100)), seq(0, 14400, by = 1800)[1:8])
  expect_equal(elapsed_breaks(c(0, 50)), seq(0, 50, by = 10))
  expect_equal(format_axis_elapsed(c(0, 90, 750), hours = FALSE), c("0:00", "1:30", "12:30"))
  expect_equal(format_axis_elapsed(c(0, 5400, 36000), hours = TRUE), c("0:00", "1:30", "10:00"))
})

test_that("the time axis is titled with its unit and labelled to match", {
  short <- x_scale(ggplot2::ggplot(read_rhylthyme(example_program())) + geom_rhylthyme())
  expect_equal(short$make_title(label_title = "start"), "Elapsed (m:ss)")
  expect_equal(short$get_labels(c(0, 120)), c("0:00", "2:00"))
  long <- x_scale(ggplot2::ggplot(thanksgiving()) + geom_rhylthyme())
  expect_equal(long$make_title(label_title = "start"), "Elapsed (h:mm)")
  expect_equal(long$get_labels(c(0, 5400)), c("0:00", "1:30"))
  # the default title is for the bottom axis only
  expect_true(is.null(short$make_sec_title()) || inherits(short$make_sec_title(), "waiver"))
})

test_that("labs() and scale names win over the default title", {
  plot <- ggplot2::ggplot(read_rhylthyme(example_program())) + geom_rhylthyme() + ggplot2::labs(x = "Kitchen time")
  expect_equal(x_scale(plot)$make_title(label_title = "Kitchen time"), "Kitchen time")
  s <- scale_x_rhylthyme(name = "Since start")
  expect_equal(s$make_title(scale_title = "Since start", label_title = "start"), "Since start")
})

test_that("finish_at puts the last step's end at that time", {
  finish <- as.POSIXct("2026-11-26 18:00")
  p <- thanksgiving(finish_at = finish)
  s <- steps(p)
  expect_equal(as.numeric(max(s$end_time)), as.numeric(finish))
  expect_equal(p$start_at, finish - max(s$end))
  expect_equal(s$start_time, p$start_at + s$start)
  expect_output(print(p), "Runs 2026-11-26 14:05 to 18:00", fixed = TRUE)
  expect_equal(thanksgiving(finish_at = "2026-11-26 18:00")$start_at, p$start_at)
})

test_that("start_at anchors the start; bad or doubled anchors are refused", {
  p <- read_rhylthyme(example_program(), start_at = "2026-11-26 08:00")
  expect_equal(as.numeric(min(steps(p)$start_time)), as.numeric(as.POSIXct("2026-11-26 08:00")))
  expect_false("start_time" %in% names(steps(read_rhylthyme(example_program()))))
  expect_error(read_rhylthyme(example_program(), start_at = "noon", finish_at = "18:00"), "not both")
  expect_error(read_rhylthyme(example_program(), start_at = "noonish"), "must be a date-time")
  expect_error(read_rhylthyme(example_program(), finish_at = 5), "must be a date-time")
})

test_that("an anchored program plots on a clock-time axis", {
  p <- thanksgiving(finish_at = "2026-11-26 18:00")
  sc <- x_scale(ggplot2::ggplot(p) + geom_rhylthyme())
  expect_s3_class(sc, "ScaleContinuousDatetime")
  expect_equal(sc$make_title(label_title = "start"), "Clock time")
  built <- ggplot2::layer_data(ggplot2::ggplot(p) + geom_rhylthyme())
  expect_equal(max(built$xmax), as.numeric(as.POSIXct("2026-11-26 18:00")))
})

test_that("autoplot gives a finished plot, and ggplot2 additions still work", {
  p <- thanksgiving()
  plot <- autoplot(p)
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title %||% ggplot2::get_labs(plot)$title, "Thanksgiving with One Oven")
  expect_null(ggplot2::get_guide_data(plot, "fill"))
  expect_false(is.null(ggplot2::get_guide_data(autoplot(p, lanes = "task"), "fill")))
  more <- plot + ggplot2::labs(x = "Minutes in") + ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(xlim = c(0, 7200)) + ggplot2::annotate("text", x = 3600, y = 1, label = "halfway") +
    scale_fill_rhylthyme("cookbook")
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(more)))
  expect_s3_class(theme_rhylthyme(), "theme")
})

test_that("scales, clock axis and autoplot look right", {
  skip_if_not_installed("vdiffr")
  p <- read_rhylthyme(example_program())
  vdiffr::expect_doppelganger("palette cookbook", ggplot2::ggplot(p) + geom_rhylthyme() + scale_fill_rhylthyme("cookbook"))
  vdiffr::expect_doppelganger("clock axis", ggplot2::ggplot(thanksgiving(finish_at = "2026-11-26 18:00")) + geom_rhylthyme(colour = "white"))
  vdiffr::expect_doppelganger("autoplot", autoplot(thanksgiving()))
})
