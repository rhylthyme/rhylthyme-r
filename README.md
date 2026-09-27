# rhylthyme (R)

Read [Rhylthyme](https://www.rhylthyme.com) programs in R and plot them with
ggplot2.

A Rhylthyme program is JSON describing parallel **tracks** of sequential
**steps** (a recipe, a lab protocol, a run-of-show), each with a duration and
a start trigger. `read_rhylthyme()` times every step with the same engine
rhylthyme.com uses (embedded through V8, so no Node.js is needed), and
`geom_rhylthyme()` draws the schedule.

```r
library(ggplot2)
library(rhylthyme)

p <- read_rhylthyme(system.file("extdata", "breakfast_schedule.json", package = "rhylthyme"))
p
#> <rhylthyme_program> Breakfast Schedule: 3 tracks, 6 steps, 11:30

ggplot(p) + geom_rhylthyme(colour = "white")
autoplot(p)   # the same, finished: theme, title, no redundant legend
```

Bars are filled by track in rhylthyme.com's palette and the time axis reads
as elapsed `m:ss` or `h:mm` by default; `scale_fill_rhylthyme("cookbook")`
picks another web palette, and any ggplot2 scale, theme or `labs()` works on
top. Anchor a program to the clock and the axis shows clock times:

```r
dinner <- read_rhylthyme("thanksgiving.json", finish_at = "2026-11-26 18:00")
autoplot(dinner)                              # 14:05 ... 18:00
steps(dinner)[c("name", "start_time", "end_time")]
```

Lanes can be tracks (the default), steps (a classic Gantt) or resources,
where steps that use a resource at the same time stack up:

```r
ggplot(p) + geom_rhylthyme(lanes = "task", aes(fill = task))
```

The program is also plain data:

```r
summary(p)       # tracks, resources, makespan
steps(p)         # one row per step: start/end (seconds), task, duration type, estimate, ...
dependencies(p)  # which step waits on which, with offsets
tracks(p)
resources(p)
```

`validate_rhylthyme()` lists schema problems by step; `read_rhylthyme()`
also takes a URL, a JSON string or a list.

The geom also works by name, on any data with start, end and lane columns:

```r
ggplot(df, aes(xmin = start, xmax = end, y = lane)) +
  layer(geom = "rhylthyme", stat = "rhylthyme", position = "identity")
```

Status: early development. The plan is in
[issue #1](https://github.com/rhylthyme/rhylthyme-r/issues/1).

## Install

```r
# install.packages("remotes")
remotes::install_github("rhylthyme/rhylthyme-r")
```

## License

Apache License 2.0. The timing engine in `inst/js/index.js` is a copy of
[@rhylthyme/timeline](https://github.com/rhylthyme/rhylthyme-timeline)
(Apache-2.0).
