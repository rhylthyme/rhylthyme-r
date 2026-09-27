bars <- function(...) {
  rows <- list(...)
  out <- do.call(rbind, lapply(rows, function(r) data.frame(
    step = r[[1]], xmin = r[[2]], xmax = r[[3]], y = r[[4]], stringsAsFactors = FALSE
  )))
  layout_bars(out)
}

deps <- function(from, to, relation = "end_to_start", offset = 0) {
  data.frame(from_step = from, to_step = to, relation = relation, offset = offset,
             stringsAsFactors = FALSE)
}

test_that("bars fill 0.8 of their lane", {
  b <- bars(list("a", 0, 10, 2))
  expect_equal(c(b$ymin, b$ymax), c(1.6, 2.4))
})

test_that("an end-to-start arrow with a gap is an elbow into the waiting step", {
  # a (top lane, y = 2) ends at 10; b (y = 1) starts at 15.
  a <- layout_arrows(bars(list("a", 0, 10, 2), list("b", 15, 20, 1)), deps("a", "b"))
  expect_equal(a$x, c(10, 10, 15))
  expect_equal(a$y, c(1.6, 1, 1)) # from a's lower edge, down to b's lane, across
})

test_that("with no gap the arrow drops straight onto the waiting step", {
  a <- layout_arrows(bars(list("a", 0, 10, 2), list("b", 10, 20, 1)), deps("a", "b"))
  expect_equal(a$x, c(10, 10))
  expect_equal(a$y, c(1.6, 1.4))
})

test_that("a hand-off (negative offset) drops at the waiting step's start", {
  a <- layout_arrows(bars(list("a", 0, 10, 2), list("b", 7, 20, 1)), deps("a", "b", offset = -3))
  expect_equal(a$x, c(7, 7))
  expect_equal(a$offset[1], -3)
})

test_that("start-to-start arrows leave the other step's start", {
  a <- layout_arrows(bars(list("a", 0, 10, 2), list("b", 5, 20, 1)),
                     deps("a", "b", relation = "start_to_start", offset = 5))
  expect_equal(a$x, c(0, 0, 5))
})

test_that("arrows go up as well as down", {
  a <- layout_arrows(bars(list("a", 0, 10, 1), list("b", 15, 20, 2)), deps("a", "b"))
  expect_equal(a$y, c(1.4, 2, 2)) # from a's upper edge
})

test_that("steps in one lane, and steps not drawn, get no arrow", {
  b <- bars(list("a", 0, 10, 1), list("b", 10, 20, 1), list("c", 20, 30, 2))
  a <- layout_arrows(b, rbind(deps("a", "b"), deps("b", "c"), deps("x", "c"), deps("c", "gone")))
  expect_equal(unique(a$from_step), "b")
  expect_equal(unique(a$arrow), 1)
  expect_equal(nrow(layout_arrows(b, NULL)), 0)
})

label_bars <- function() {
  data.frame(
    xmin = c(0.0, 0.3, 0.5, 0.9), xmax = c(0.2, 0.35, 0.6, 0.95),
    ymin = 0.1, ymax = 0.2, lane = c(1, 1, 1, 2)
  )
}

test_that("a label goes inside its bar when it fits", {
  p <- place_labels(label_bars(), width = c(0.1, 0.01, 0.05, 0.02), height = 0.05, pad = 0.01)
  expect_equal(p$placement, rep("inside", 4))
  expect_equal(p$x, c(0.01, 0.31, 0.51, 0.91))
})

test_that("a label that does not fit goes after the bar when the lane has room", {
  # bar 2 (0.30-0.35) has 0.15 free before bar 3 starts at 0.5.
  p <- place_labels(label_bars(), width = c(0.1, 0.12, 0.05, 0.02), height = 0.05, pad = 0.01)
  expect_equal(p$placement[2], "right")
  expect_equal(p$x[2], 0.36)
})

test_that("a label with no room anywhere is dropped", {
  # bar 1 (0-0.2) is too short for 0.25, and bar 2 starts at 0.3.
  p <- place_labels(label_bars(), width = c(0.25, 0.01, 0.05, 0.02), height = 0.05, pad = 0.01)
  expect_equal(p$placement[1], "none")
  # the panel edge limits the last bar in a lane
  p <- place_labels(label_bars(), width = c(0.1, 0.01, 0.05, 0.2), height = 0.05, pad = 0.01)
  expect_equal(p$placement[4], "none")
  # too tall for the bar
  p <- place_labels(label_bars(), width = rep(0.01, 4), height = 0.2, pad = 0)
  expect_equal(p$placement, rep("none", 4))
})

test_that("label ink contrasts with the fill", {
  expect_equal(label_ink(c("#FFFFFF", "#000000", "#F8766D", NA)), c("grey10", "white", "grey10", "grey15"))
})

lane_steps <- function() {
  data.frame(
    track_id = c("a", "a", "b", "b", "c"), track_name = c("A", "A", "B", "B", "C"),
    track_index = c(1L, 1L, 2L, 2L, 3L), step_id = c("a1", "a2", "b1", "b2", "c1"),
    name = c("Prep", "Cook", "Prep", "Serve", "Wait"),
    task = c("bench", "stove", "bench", NA, "stove"),
    start = c(0, 10, 5, 30, 2), stringsAsFactors = FALSE
  )
}

test_that("track lanes follow authored track order, first at the top", {
  f <- lane_factor(lane_steps(), "track")
  expect_equal(levels(f), c("C", "B", "A"))
  expect_equal(as.character(f), c("A", "A", "B", "B", "C"))
})

test_that("step lanes go by track then start, and disambiguate repeated names", {
  f <- lane_factor(lane_steps(), "step")
  expect_equal(rev(levels(f)), c("Prep [a1]", "Cook", "Prep [b1]", "Serve", "Wait"))
})

test_that("task lanes go by first use, with a lane for steps with no task", {
  f <- lane_factor(lane_steps(), "task")
  expect_equal(rev(levels(f)), c("bench", "stove", "(no task)"))
  expect_equal(as.character(f)[4], "(no task)")
})

test_that("overlapping bars in a lane stack into sub-rows; touching ones do not", {
  d <- data.frame(y = c(1, 1, 1, 1, 2), xmin = c(0, 5, 10, 12, 0), xmax = c(10, 12, 20, 15, 5))
  s <- stack_overlaps(d)
  # 0-10 row 1; 5-12 overlaps it -> row 2; 10-20 reuses row 1; 12-15 reuses row 2
  expect_equal(s$sub_row, c(1L, 2L, 1L, 2L, 1L))
  expect_equal(s$n_sub, c(2L, 2L, 2L, 2L, 1L))
  b <- layout_bars(s)
  expect_equal(b$ymax[1:2], c(1.4, 1.0))
  expect_equal(b$ymax - b$ymin, c(rep(0.4, 4), 0.8))
})
