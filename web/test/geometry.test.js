import { test } from "node:test";
import assert from "node:assert/strict";
import { linearScale, stepPath, bandPath, spreadLabels } from "../js/geometry.js";

const sx = linearScale([0, 10], [0, 100]);
const sy = linearScale([0, 1], [100, 0]);

test("a scale maps its domain onto its range, and survives a flat domain", () => {
  assert.equal(sx(5), 50);
  assert.equal(sy(1), 0);
  assert.equal(linearScale([3, 3], [0, 100])(3), 0);
});

test("a step path moves across, then down", () => {
  assert.equal(stepPath([0, 2, 5], [1, 0.5, 0.2], sx, sy), "M0,0H20V50H50V80");
  assert.equal(stepPath([], [], sx, sy), "");
});

test("a curve can be extended flat to the edge of the chart", () => {
  assert.equal(stepPath([0, 2], [1, 0.5], sx, sy, 10), "M0,0H20V50H100");
});

test("a band is one closed outline", () => {
  const d = bandPath([0, 5, 10], [1, 0.4, 0.2], [1, 0.8, 0.6], sx, sy);
  assert.ok(d.startsWith("M0,0") && d.endsWith("Z"));
  assert.equal(bandPath([], [], [], sx, sy), "");
});

test("labels are pushed apart but stay inside the chart", () => {
  const out = spreadLabels([10, 11, 12], 10, 0, 100);
  assert.deepEqual(out, [10, 20, 30]);
  const tight = spreadLabels([95, 96, 97], 10, 0, 100);
  assert.deepEqual(tight, [80, 90, 100]);
  assert.deepEqual(spreadLabels([], 10, 0, 100), []);
});
