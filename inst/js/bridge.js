// Glue between R and the timing engine. index.js next to this file is a
// byte copy of @rhylthyme/timeline (kept identical by the monorepo's
// tools/check_mirrors.sh); nothing here changes timing, it only turns the
// engine's answers into rows R can read as a data frame, in authored order.
var RhylthymeR = (function () {
  'use strict';

  // How the step's planning duration is known. "instrument": an instrument
  // step with no authored duration (the engine estimates it).
  function durationType(step) {
    if (Rhylthyme.instrumentEstimate(step)) return 'instrument';
    var d = step.duration;
    if (typeof d === 'string' || typeof d === 'number') return 'fixed';
    if (d && typeof d === 'object') return d.type ? String(d.type) : 'fixed';
    return null;
  }

  // The planned length is a guess rather than authored: the renderer's
  // dotted-outline rule (instrument estimate, or filled in by `rhylthyme
  // plan`), plus indefinite steps, which plan with a placeholder.
  function isEstimate(step) {
    return !!Rhylthyme.instrumentEstimate(step)
      || !!(step.metadata && step.metadata.durationEstimate)
      || !!(step.duration && step.duration.type === 'indefinite');
  }

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
          resolved: t.resolved === true,
          duration_type: durationType(step),
          estimate: isEstimate(step),
          instance_of: step.instanceOf == null ? null : String(step.instanceOf),
          instance_index: typeof step.instanceIndex === 'number' ? step.instanceIndex : null
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
