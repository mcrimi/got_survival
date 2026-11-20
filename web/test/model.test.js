import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import {
  referenceProfile, linearPredictor, survivalAt, survivalCurve, interval, band,
  quantile, stepAt, matchingCharacters, profileOf,
} from "../js/model.js";

const load = (path) => JSON.parse(readFileSync(new URL(path, import.meta.url)));
const model = load("../data/model.json");
const characters = load("../data/characters.json");
const fixtures = load("./fixtures.json");
const horizon = model.meta.horizon_hrs;
const close = (a, b, tol = 1e-7) => assert.ok(Math.abs(a - b) <= tol, `${a} vs ${b}`);

test("JavaScript reproduces R's predictions for every fixture", () => {
  assert.ok(fixtures.cases.length > 50);
  for (const c of fixtures.cases) {
    close(linearPredictor(model, c.profile), c.lp);
    fixtures.times.forEach((t, i) => close(survivalAt(model, c.profile, t), c.surv[i]));
    const [lo, hi] = interval(model, c.profile, horizon);
    close(lo, c.interval[0]);
    close(hi, c.interval[1]);
  }
});

test("the reference profile has a linear predictor of exactly zero", () => {
  assert.equal(linearPredictor(model, referenceProfile(model)), 0);
});

test("survival starts at 1, never rises and holds its last value past the finale", () => {
  const profile = referenceProfile(model);
  assert.equal(survivalAt(model, profile, 0), 1);
  assert.equal(survivalAt(model, profile, -5), 1);
  const { surv } = survivalCurve(model, profile);
  for (let i = 1; i < surv.length; i += 1) assert.ok(surv[i] <= surv[i - 1]);
  assert.equal(survivalAt(model, profile, 1e6), surv[surv.length - 1]);
  assert.equal(survivalAt(model, profile, horizon), surv[surv.length - 1]);
});

test("every combination of levels gives finite probabilities and an ordered interval", () => {
  const [sex, status, occupation, religion, allegiance] = model.terms.map((t) => t.levels.map((l) => l.level));
  let count = 0;
  for (const a of sex) for (const b of status) for (const c of occupation) for (const d of religion) for (const e of allegiance) {
    const profile = { sex: a, social_status: b, occupation: c, religion: d, allegiance: e };
    const s = survivalAt(model, profile, horizon);
    const [lo, hi] = interval(model, profile, horizon);
    assert.ok(Number.isFinite(s) && s > 0 && s < 1);
    assert.ok(lo >= 0 && lo <= hi && hi <= 1);
    count += 1;
  }
  assert.equal(count, 2 * 2 * 3 * 7 * 9);
});

test("the band brackets nothing impossible and matches interval() at the finale", () => {
  const profile = referenceProfile(model);
  const b = band(model, profile);
  assert.equal(b.lo[0], 1);
  assert.equal(b.hi[0], 1);
  b.lo.forEach((lo, i) => assert.ok(lo <= b.hi[i]));
  const [lo, hi] = interval(model, profile, horizon);
  assert.equal(b.lo.at(-1), lo);
  assert.equal(b.hi.at(-1), hi);
});

test("an unknown level is an error, not a silent zero", () => {
  const profile = { ...referenceProfile(model), allegiance: "Martell" };
  assert.throws(() => survivalAt(model, profile, 10), /Unknown allegiance: Martell/);
  assert.throws(() => survivalAt(model, { sex: "Male" }, 10), /Unknown/);
});

test("quantile follows R's type 7", () => {
  assert.equal(quantile([1, 2, 3, 4], 0.5), 2.5);
  assert.equal(quantile([1, 2, 3, 4], 0), 1);
  assert.equal(quantile([1, 2, 3, 4], 1), 4);
  close(quantile([10, 20, 30], 0.025), 10.5);
  assert.equal(quantile([7], 0.3), 7);
});

test("stepAt is right-continuous and clamps", () => {
  const t = [0, 1, 2];
  const v = [0, 10, 20];
  assert.equal(stepAt(t, v, 0.99), 0);
  assert.equal(stepAt(t, v, 1), 10);
  assert.equal(stepAt(t, v, 99), 20);
  assert.equal(stepAt(t, v, NaN), 0);
});

test("every real character has a profile the model accepts", () => {
  assert.equal(characters.length, model.meta.n);
  for (const c of characters) {
    const s = survivalAt(model, profileOf(model, c), c.time_hrs);
    assert.ok(s > 0 && s <= 1);
  }
  const jon = characters.find((c) => c.name === "Jon Snow");
  const peers = matchingCharacters(model, characters, profileOf(model, jon));
  assert.ok(peers.includes(jon));
});
