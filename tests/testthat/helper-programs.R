example_program <- function(name = "breakfast_schedule.json") {
  system.file("extdata", name, package = "rhylthyme", mustWork = TRUE)
}

# The corpus: rhylthyme-timeline's fixture programs, and the engine's own
# timings for each (engine-timings.json). Both are byte copies kept identical
# by the monorepo's tools/check_mirrors.sh.
fixture_programs <- function() {
  dir <- test_path("fixtures", "programs")
  sort(list.files(dir, pattern = "\\.json$", full.names = TRUE))
}

engine_timings <- function() {
  jsonlite::read_json(test_path("fixtures", "engine-timings.json"))$programs
}

# Write `program` (a list) to a temporary JSON file for read_rhylthyme().
program_file <- function(program, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  jsonlite::write_json(program, path, auto_unbox = TRUE, null = "null")
  path
}

# The named parts GeomRhylthyme draws for the first panel (bars, ranges,
# estimates, arrows, labels).
panel_grob <- function(plot) {
  grob <- ggplot2::layer_grob(plot)[[1]]
  names <- vapply(grob$children, function(g) g$name, "")
  stats::setNames(grob$children, names)
}
