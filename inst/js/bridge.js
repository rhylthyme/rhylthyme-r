// Glue between R and the timing engine. index.js next to this file is a
// byte copy of @rhylthyme/timeline (kept identical by the monorepo's
// tools/check_mirrors.sh); nothing here changes timing, it only turns the
// engine's answers into rows R can read as a data frame, in authored order.
var RhylthymeR = (function () {
  'use strict';

  function steps(program) {
    var expanded = Rhylthyme.expandReplicates(program);
    var timings = Rhylthyme.computeStepTimings(program);
    var rows = [];
    ((expanded && expanded.tracks) || []).forEach(function (track, ti) {
      if (!track) return;
      var trackId = track.trackId == null ? String(ti + 1) : String(track.trackId);
      ((track.steps) || []).forEach(function (step) {
        if (!step || !step.stepId) return;
        var t = timings[step.stepId] || {};
        rows.push({
          track_id: trackId,
          track_name: track.name ? String(track.name) : trackId,
          track_index: ti + 1,
          step_id: String(step.stepId),
          name: step.name ? String(step.name) : String(step.stepId),
          start: typeof t.start === 'number' ? t.start : null,
          end: typeof t.end === 'number' ? t.end : null,
          resolved: t.resolved === true
        });
      });
    });
    return rows;
  }

  return {
    version: Rhylthyme.version,
    stepsFromJson: function (text) { return steps(JSON.parse(text)); }
  };
}());
