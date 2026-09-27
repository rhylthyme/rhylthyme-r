# Layout: where bars, arrows and labels go. Pure data frames, no ggplot2 or
# grid, so it is tested on its own; StatRhylthyme and GeomRhylthyme call it.

# The lane each step goes in, as a factor whose levels run bottom to top (so
# the first level ggplot2 draws at the top is the first track or step):
#   "track": one lane per track, in authored order;
#   "step":  one lane per step, by track then start time;
#   "task":  one lane per task (resource type), in order of first use; steps
#            with no task share a "(no task)" lane.
# `data` has the steps() columns (track_id, track_name, track_index,
# step_id, name, task, start).
lane_factor <- function(data, lanes = c("track", "step", "task")) {
  lanes <- match.arg(lanes)
  if (!nrow(data)) return(factor(character()))
  if (lanes == "track") {
    tracks <- unique(data[c("track_id", "track_name", "track_index")])
    tracks <- tracks[order(tracks$track_index), ]
    label <- if (anyDuplicated(tracks$track_name)) tracks$track_id else tracks$track_name
    return(factor(label[match(data$track_id, tracks$track_id)], levels = rev(label)))
  }
  if (lanes == "step") {
    label <- data$name
    twice <- label %in% label[duplicated(label)]
    label[twice] <- sprintf("%s [%s]", label[twice], data$step_id[twice])
    ord <- order(data$track_index, data$start, seq_len(nrow(data)))
    return(factor(label, levels = rev(unique(label[ord]))))
  }
  task <- ifelse(is.na(data$task), "(no task)", data$task)
  ord <- order(data$start, data$track_index, seq_len(nrow(data)))
  factor(task, levels = rev(unique(task[ord])))
}

# Overlapping bars in one lane (steps sharing a task, a hand-off inside a
# track, or any data) are stacked into sub-rows so none hides another:
# adds `sub_row` (1 = top) and `n_sub` (sub-rows in that lane). Greedy
# interval packing: each bar, in start order, takes the first sub-row free
# by its start. Bars that only touch do not overlap.
stack_overlaps <- function(data) {
  data$sub_row <- rep(1L, nrow(data))
  data$n_sub <- rep(1L, nrow(data))
  for (lane in unique(data$y)) {
    rows <- which(data$y == lane)
    rows <- rows[order(data$xmin[rows], data$xmax[rows])]
    ends <- numeric()
    for (i in rows) {
      free <- which(ends <= data$xmin[i] + 1e-9)
      k <- if (length(free)) free[1] else length(ends) + 1
      ends[k] <- data$xmax[i]
      data$sub_row[i] <- k
    }
    data$n_sub[rows] <- length(ends)
  }
  data
}

# `y` is the lane position (ggplot2 has already turned a discrete lane into
# 1, 2, ...). Bars fill `bar_height` of their lane, shared equally between
# the lane's sub-rows when stack_overlaps() made any.
layout_bars <- function(data, bar_height = 0.8) {
  n <- if (is.null(data$n_sub)) 1 else data$n_sub
  row <- if (is.null(data$sub_row)) 1 else data$sub_row
  h <- bar_height / n
  data$ymax <- data$y + bar_height / 2 - (row - 1) * h
  data$ymin <- data$ymax - h
  data
}

# Dependency arrows between bars, as paths in data coordinates: one row per
# point, `arrow` numbering the paths in order. `bars` has one row per step
# (`step`, `xmin`, `xmax`, `y`, `ymin`, `ymax`); `dependencies` has
# `from_step`, `to_step`, `relation` and `offset`, as dependencies() gives.
#
# Like the web timeline, only dependencies between lanes are drawn (steps in
# one lane already follow each other). An arrow leaves the waiting-on step's
# end (its start, for start_to_start) on the edge facing the other lane,
# runs to that lane and ends at the waiting step's start. When the waiting
# step starts at or before that point (a hand-off, or a zero gap) the arrow
# is a straight drop onto the bar.
layout_arrows <- function(bars, dependencies) {
  empty <- data.frame(arrow = integer(), x = numeric(), y = numeric(),
                      from_step = character(), to_step = character(),
                      relation = character(), offset = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(dependencies) || !nrow(dependencies) || is.null(bars$step) || !nrow(bars)) {
    return(empty)
  }
  from <- match(dependencies$from_step, bars$step)
  to <- match(dependencies$to_step, bars$step)
  keep <- !is.na(from) & !is.na(to)
  keep[keep] <- bars$y[from[keep]] != bars$y[to[keep]]
  if (!any(keep)) return(empty)
  deps <- dependencies[keep, , drop = FALSE]
  from <- bars[from[keep], , drop = FALSE]
  to <- bars[to[keep], , drop = FALSE]

  paths <- lapply(seq_len(nrow(deps)), function(i) {
    f <- from[i, ]
    t <- to[i, ]
    down <- t$y < f$y
    x0 <- if (identical(deps$relation[i], "start_to_start")) f$xmin else f$xmax
    y0 <- if (down) f$ymin else f$ymax
    if (t$xmin > x0) {
      x <- c(x0, x0, t$xmin)
      y <- c(y0, t$y, t$y)
    } else {
      x <- c(t$xmin, t$xmin)
      y <- c(y0, if (down) t$ymax else t$ymin)
    }
    data.frame(arrow = i, x = x, y = y, from_step = deps$from_step[i],
               to_step = deps$to_step[i], relation = deps$relation[i],
               offset = deps$offset[i], stringsAsFactors = FALSE)
  })
  do.call(rbind, paths)
}

# Where each step's label goes, given the drawn size of everything (any one
# unit, e.g. npc): `bars` has `xmin`, `xmax`, `ymin`, `ymax` and `lane`;
# `width` and `height` are each label's size. A label goes inside its bar
# (left-aligned) when it fits there, else just after the bar when the lane
# has room before the next bar (or the panel's right edge, `right_edge`),
# else nowhere. Returns `placement` ("inside", "right" or "none") and `x`,
# the label's left edge.
place_labels <- function(bars, width, height, pad = 0, right_edge = 1) {
  n <- nrow(bars)
  placement <- rep("none", n)
  x <- rep(NA_real_, n)
  if (!n) return(data.frame(placement = placement, x = x, stringsAsFactors = FALSE))
  tall_enough <- height <= (bars$ymax - bars$ymin)
  for (i in seq_len(n)) {
    if (is.na(width[i]) || !tall_enough[i]) next
    if (width[i] + 2 * pad <= bars$xmax[i] - bars$xmin[i]) {
      placement[i] <- "inside"
      x[i] <- bars$xmin[i] + pad
      next
    }
    same_lane <- bars$lane == bars$lane[i] & seq_len(n) != i & bars$xmin >= bars$xmax[i]
    next_start <- min(c(bars$xmin[same_lane], right_edge))
    if (width[i] + 2 * pad <= next_start - bars$xmax[i]) {
      placement[i] <- "right"
      x[i] <- bars$xmax[i] + pad
    }
  }
  data.frame(placement = placement, x = x, stringsAsFactors = FALSE)
}

# Black or white text, whichever reads better on `fill`.
label_ink <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  luminance <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(is.na(fill), "grey15", ifelse(luminance > 0.55, "grey10", "white"))
}
