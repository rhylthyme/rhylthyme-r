# Collapsing replicates for plotting: one bar per authored step instead of
# one per instance. Works on fortified plotting data; steps() and tidy() are
# never collapsed.

# `data` is fortify(program) output; `program` the rhylthyme_program. Each
# group of instances becomes one row: the authored step's id and name
# (labelled "x n"), spanning the first instance's start to the last one's
# end, in the authored track (the parent of any per-instance tracks the
# engine made). Returns list(data, dependencies), the dependencies re-pointed
# at the authored steps.
collapse_replicates <- function(data, program) {
  deps <- program$dependencies
  instance <- !is.na(data$instance_of)
  if (!any(instance)) return(list(data = data, dependencies = deps))

  tracks <- program$tracks
  parent_of <- ifelse(is.na(tracks$parent_track_id), tracks$track_id, tracks$parent_track_id)
  names(parent_of) <- tracks$track_id
  authored <- authored_step_names(program$program)

  kept <- data[!instance, , drop = FALSE]
  kept$n_instances <- rep(1L, nrow(kept))
  groups <- split(which(instance), factor(data$instance_of[instance], levels = unique(data$instance_of[instance])))
  collapsed <- lapply(names(groups), function(root) {
    rows <- data[groups[[root]], , drop = FALSE]
    out <- rows[1, , drop = FALSE]
    track <- parent_of[[rows$track_id[1]]]
    t <- parent_track(track, tracks, parent_of, program$program)
    out$track_id <- track
    out$track_name <- t$name
    out$track_index <- t$index
    out$step_id <- root
    base <- authored[root]
    if (is.na(base)) base <- sub(" \\([0-9]+ of [0-9]+\\)$", "", rows$name[1])
    out$name <- sprintf("%s \u00d7%d", base, nrow(rows))
    out$start <- min(rows$start, na.rm = TRUE)
    out$end <- max(rows$end, na.rm = TRUE)
    out$duration <- out$end - out$start
    out$resolved <- all(rows$resolved)
    out$estimate <- any(rows$estimate)
    out$min_seconds <- NA_real_
    out$max_seconds <- NA_real_
    out$instance_of <- NA_character_
    out$instance_index <- NA_integer_
    out$n_instances <- nrow(rows)
    out
  })
  data <- do.call(rbind, c(list(kept), collapsed))
  data <- data[order(data$track_index, data$start), , drop = FALSE]
  rownames(data) <- NULL

  root_of <- stats::setNames(program$steps$instance_of, program$steps$step_id)
  lift <- function(ids) ifelse(is.na(root_of[ids]), ids, root_of[ids])
  deps$from_step <- lift(deps$from_step)
  deps$to_step <- lift(deps$to_step)
  deps <- deps[deps$from_step != deps$to_step, , drop = FALSE]
  deps <- deps[!duplicated(deps[c("from_step", "to_step", "relation")]), , drop = FALSE]
  rownames(deps) <- NULL
  list(data = data, dependencies = deps)
}

# Name and position of an authored track. The engine drops a track whose
# only step is replicated into per-instance tracks, so it may exist only in
# the authored program; it then sits where its instance tracks start.
parent_track <- function(track_id, tracks, parent_of, program) {
  t <- tracks[tracks$track_id == track_id, , drop = FALSE]
  if (nrow(t)) return(list(name = t$name[1], index = t$track_index[1]))
  authored <- Filter(function(x) identical(x$trackId, track_id), program$tracks)
  name <- if (length(authored)) authored[[1]]$name %||% track_id else track_id
  list(name = as.character(name), index = min(tracks$track_index[parent_of == track_id]))
}

# stepId -> name for every authored step (tracks and track templates).
authored_step_names <- function(program) {
  steps <- unlist(lapply(c(program$tracks, program$trackTemplates), function(t) t$steps), recursive = FALSE)
  ids <- vapply(steps, function(s) as.character(s$stepId %||% NA), "")
  names <- vapply(steps, function(s) as.character(s$name %||% s$stepId %||% NA), "")
  stats::setNames(names, ids)
}
