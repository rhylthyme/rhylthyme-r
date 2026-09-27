// Glue between R and the timing engine. index.js next to this file is a
// byte copy of @rhylthyme/timeline (kept identical by the monorepo's
// tools/check_mirrors.sh); nothing here changes timing, it only turns the
// engine's answers into rows R can read as data frames, in authored order.
// Everything is read off the program as the engine expands it (replicate
// instances, template tracks), so ids match the engine's timings.
var RhylthymeR = (function () {
  'use strict';

  var seconds = Rhylthyme.parseSeconds;

  function num(v) {
    return v === undefined || v === null ? null : seconds(v);
  }

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

  // Shortest and longest the step can take, as authored: a fixed step's
  // length for both; a variable step's min and max; open-ended otherwise.
  function range(step, type) {
    var d = step.duration;
    if (type === 'fixed') {
      var s = Rhylthyme.stepDurationSeconds(step);
      return [s, s];
    }
    if (d && typeof d === 'object' && (type === 'variable' || type === 'indefinite')) {
      return [num(d.minSeconds), type === 'variable' ? num(d.maxSeconds) : null];
    }
    return [null, null];
  }

  function triggerType(step) {
    var t = step.startTrigger;
    if (!t || typeof t !== 'object') return null;
    if (t.logic && Array.isArray(t.triggers)) return 'compound';
    return t.type ? String(t.type) : 'programStart';
  }

  // One edge per step-referencing trigger (inside compound triggers too).
  function edges(step, trigger, logic, out) {
    if (!trigger || typeof trigger !== 'object') return;
    if (trigger.logic && Array.isArray(trigger.triggers)) {
      trigger.triggers.forEach(function (t) { edges(step, t, String(trigger.logic), out); });
      return;
    }
    var type = trigger.type || 'programStart';
    if (!(type === 'afterStep' || type === 'afterStepWithBuffer' || type === 'onAbort') || !trigger.stepId) return;
    var offset = seconds(trigger.offsetSeconds) + (type === 'afterStepWithBuffer' ? seconds(trigger.bufferSeconds) : 0);
    out.push({
      from_step: String(trigger.stepId),
      to_step: String(step.stepId),
      relation: type === 'onAbort' ? 'on_abort' : (trigger.event === 'start' ? 'start_to_start' : 'end_to_start'),
      offset: offset,
      trigger_type: type,
      logic: logic
    });
  }

  function describe(program) {
    var expanded = Rhylthyme.expandReplicates(program);
    var timings = Rhylthyme.computeStepTimings(program);
    var steps = [], tracks = [], dependencies = [];
    ((expanded && expanded.tracks) || []).forEach(function (track, ti) {
      if (!track) return;
      var trackId = track.trackId == null ? String(ti + 1) : String(track.trackId);
      var trackName = track.name ? String(track.name) : trackId;
      tracks.push({
        track_id: trackId,
        name: trackName,
        track_index: ti + 1,
        parent_track_id: track.parentTrackId == null ? null : String(track.parentTrackId),
        template_id: track.templateId == null ? null : String(track.templateId)
      });
      (track.steps || []).forEach(function (step) {
        if (!step || !step.stepId) return;
        var t = timings[step.stepId] || {};
        var type = durationType(step);
        var r = range(step, type);
        steps.push({
          track_id: trackId,
          track_name: trackName,
          track_index: ti + 1,
          step_id: String(step.stepId),
          name: step.name ? String(step.name) : String(step.stepId),
          task: step.task == null ? null : String(step.task),
          start: typeof t.start === 'number' ? t.start : null,
          end: typeof t.end === 'number' ? t.end : null,
          resolved: t.resolved === true,
          duration_type: type,
          min_seconds: r[0],
          max_seconds: r[1],
          estimate: isEstimate(step),
          trigger_type: triggerType(step),
          instance_of: step.instanceOf == null ? null : String(step.instanceOf),
          instance_index: typeof step.instanceIndex === 'number' ? step.instanceIndex : null
        });
        edges(step, step.startTrigger, null, dependencies);
      });
    });
    return { steps: steps, tracks: tracks, dependencies: dependencies };
  }

  return {
    version: Rhylthyme.version,
    supportedSchemaVersions: Rhylthyme.supportedSchemaVersions,
    describeJson: function (text) { return describe(JSON.parse(text)); }
  };
}());
