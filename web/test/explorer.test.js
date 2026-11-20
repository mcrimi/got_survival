import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { normalise, searchCharacters, fateText, formatHours, percent } from "../js/explorer.js";

const characters = JSON.parse(readFileSync(new URL("../data/characters.json", import.meta.url)));
const names = (list) => list.map((c) => c.name);

test("an empty query returns everyone", () => {
  assert.equal(searchCharacters(characters).length, characters.length);
  assert.equal(searchCharacters(characters, "   ").length, characters.length);
  assert.equal(searchCharacters(characters, null).length, characters.length);
});

test("search ignores case, accents, apostrophes and word order", () => {
  assert.ok(names(searchCharacters(characters, "JON snow")).includes("Jon Snow"));
  assert.ok(names(searchCharacters(characters, "snow jon")).includes("Jon Snow"));
  assert.equal(normalise("Jaqen H'ghar"), "jaqen hghar");
  assert.equal(normalise("Daenérys"), "daenerys");
  const watch = searchCharacters(characters, "nights watch");
  assert.ok(watch.length > 10 && watch.every((c) => c.allegiance === "Night's Watch"));
});

test("regex and HTML characters in a query cannot break the search", () => {
  for (const q of ["(", "[a-", "*", "\\", "<script>", ".*", "$^"]) {
    assert.doesNotThrow(() => searchCharacters(characters, q));
  }
  assert.equal(searchCharacters(characters, "zzzzqqq").length, 0);
});

test("the fate filter splits the cast into dead and alive", () => {
  const dead = searchCharacters(characters, "", "dead");
  const alive = searchCharacters(characters, "", "alive");
  assert.equal(dead.length, 212);
  assert.equal(dead.length + alive.length, characters.length);
});

test("fates and numbers read naturally", () => {
  assert.equal(formatHours(0.002), "1 min");
  assert.equal(formatHours(0.5), "30 min");
  assert.equal(formatHours(9.14), "9.1 h");
  assert.equal(formatHours(63.99), "64 h");
  assert.match(fateText({ dead: false, time_hrs: 63.99 }), /^Alive at the finale/);
  assert.match(fateText({ dead: true, time_hrs: 9.1, death: { season: 1 } }), /season 1/);
  assert.doesNotThrow(() => fateText({ dead: true, time_hrs: 2, death: null }));
  assert.equal(percent(0.004), "<1%");
  assert.equal(percent(0.996), ">99%");
  assert.equal(percent(0.149), "15%");
  assert.equal(percent(0), "0%");
});
