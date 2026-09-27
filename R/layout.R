# Layout: where each step's bar goes. Pure data frames, no ggplot2, so it can
# be tested on its own; StatRhylthyme calls it for each panel.

# `y` is the lane position (ggplot2 has already turned a discrete lane into
# 1, 2, ...). Bars fill `bar_height` of their lane.
layout_bars <- function(data, bar_height = 0.8) {
  data$ymin <- data$y - bar_height / 2
  data$ymax <- data$y + bar_height / 2
  data
}
