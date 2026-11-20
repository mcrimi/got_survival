// Wiring for /got/: loads the exported model and draws everything from it.

import {
  referenceProfile, survivalAt, survivalCurve, interval, band, quantile,
  matchingCharacters, profileOf,
} from "./model.js";
import { searchCharacters, fateText, formatHours, percent } from "./explorer.js";
import { drawSurvival, drawForest, flash, DASHES } from "./chart.js";

const $ = (id) => document.getElementById(id);
const esc = (s) => String(s ?? "").replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);
const fill = (key, text) => document.querySelectorAll(`[data-fill="${key}"]`).forEach((el) => { el.textContent = text; });
const SEG_MAX = 3; // covariates with up to this many levels get buttons, the rest a menu
const CAST_PAGE = 30;
const VAGUE = new Set(["Other", "Unknown", "Unclear"]);

const state = { profile: null, marked: null, term: "sex", shown: {}, query: "", fate: "all", castLimit: CAST_PAGE };
let model, story, characters;

/* ---------- Paper settings (shared with the main site) ---------- */

function initPaper() {
  const KEY = "mcrimi-site";
  const read = () => { try { return JSON.parse(localStorage.getItem(KEY) || "{}") || {}; } catch (e) { return {}; } };
  const root = document.documentElement;
  const buttons = [...document.querySelectorAll("[data-paper]")];
  const show = (paper) => buttons.forEach((b) => b.setAttribute("aria-pressed", String(b.dataset.paper === paper)));
  show(read().paper || "warm");
  buttons.forEach((b) => b.addEventListener("click", () => {
    const paper = b.dataset.paper;
    root.dataset.theme = paper === "night" ? "dark" : "light";
    if (paper === "grey") root.dataset.paper = "grey"; else delete root.dataset.paper;
    try { localStorage.setItem(KEY, JSON.stringify({ ...read(), paper })); } catch (e) { /* private mode: keep it for this visit only */ }
    show(paper);
  }));
  const aa = $("aa");
  const panel = $("panel");
  const toggle = (open) => { panel.hidden = !open; aa.setAttribute("aria-expanded", String(open)); };
  aa.addEventListener("click", () => toggle(panel.hidden));
  document.addEventListener("click", (e) => { if (!panel.hidden && !panel.contains(e.target) && e.target !== aa) toggle(false); });
  document.addEventListener("keydown", (e) => { if (e.key === "Escape") toggle(false); });
}

/* ---------- Your odds ---------- */

function buildControls() {
  $("controls").innerHTML = model.terms.map((term) => {
    const id = `ctl-${term.name}`;
    if (term.levels.length <= SEG_MAX) {
      return `<div class="field"><span class="label" id="${id}">${esc(term.label)}</span>
        <div class="seg" role="group" aria-labelledby="${id}" data-term="${term.name}">
        ${term.levels.map((l) => `<button type="button" data-level="${esc(l.level)}">${esc(shortLevel(l.level))}</button>`).join("")}
        </div></div>`;
    }
    return `<label class="field"><span class="label">${esc(term.label)}</span>
      <span class="select"><select name="${term.name}" data-term="${term.name}">
      ${term.levels.map((l) => `<option value="${esc(l.level)}">${esc(l.level)}</option>`).join("")}
      </select></span></label>`;
  }).join("");

  $("controls").addEventListener("click", (e) => {
    const button = e.target.closest("button[data-level]");
    if (button) setLevel(button.parentNode.dataset.term, button.dataset.level);
  });
  $("controls").addEventListener("change", (e) => {
    if (e.target.matches("select[data-term]")) setLevel(e.target.dataset.term, e.target.value);
  });
  $("controls").addEventListener("submit", (e) => e.preventDefault());
}

const shortLevel = (level) => ({ "Boiled leather collar": "Leather", "Silk collar": "Silk" })[level] || level;

function setLevel(term, level) {
  state.profile = { ...state.profile, [term]: level };
  state.marked = null;
  renderPredict(true);
}

function syncControls() {
  document.querySelectorAll("#controls [data-term]").forEach((el) => {
    const value = state.profile[el.dataset.term];
    if (el.tagName === "SELECT") el.value = value;
    else el.querySelectorAll("button").forEach((b) => b.setAttribute("aria-pressed", String(b.dataset.level === value)));
  });
}

function renderPredict(withFlash = false) {
  const horizon = model.meta.horizon_hrs;
  const { profile, marked } = state;
  syncControls();

  const curve = survivalCurve(model, profile);
  drawSurvival($("predict-chart"), {
    xMax: horizon,
    band: band(model, profile),
    series: [
      { label: "All characters", time: story.km.overall.time, surv: story.km.overall.surv, dash: "1.5 3.5", cls: "ghost" },
      { label: marked ? shortName(marked.name) : "You", time: curve.time, surv: curve.surv, cls: "main" },
    ],
    marker: marked ? { t: marked.time_hrs, s: survivalAt(model, profile, marked.time_hrs), dead: marked.dead } : null,
  });
  if (withFlash) flash($("predict-fig"));

  const chance = survivalAt(model, profile, horizon);
  const [lo, hi] = interval(model, profile, horizon);
  const everyone = story.km.overall.surv.at(-1);
  const subject = marked ? `A character like ${esc(marked.name)} has` : "You have";
  $("verdict").innerHTML = `${subject} a <strong>${percent(chance)}</strong> chance of seeing the finale.`;

  const compare = Math.abs(chance - everyone) < 0.015 ? "about the same as" : chance > everyone ? "better than" : "worse than";
  let fine = `Plausible range ${percent(lo)} to ${percent(hi)}; ${compare} the ${percent(everyone)} for all characters.`;
  if (marked) {
    const then = survivalAt(model, profile, marked.time_hrs);
    fine += marked.dead
      ? ` ${esc(marked.name)} died ${formatHours(marked.time_hrs)} in (✕); the model gave ${percent(then)} odds of lasting that long.`
      : ` ${esc(marked.name)} was alive at the end, ${formatHours(marked.time_hrs)} after first appearing (○).`;
  }
  $("verdict-fine").innerHTML = fine;

  const peers = matchingCharacters(model, characters, profile);
  const dead = peers.filter((c) => c.dead).length;
  if (!peers.length) {
    $("peers").textContent = "No character in the show has exactly this profile, so the estimate adds up each trait’s separate effect.";
  } else {
    const names = peers.slice().sort((a, b) => b.time_hrs - a.time_hrs).slice(0, 4).map((c) => c.name);
    const more = peers.length > names.length ? " and others" : "";
    const few = peers.length < 5 ? " That is very few to learn from, so treat the estimate as a rough guide." : "";
    $("peers").textContent = `${peers.length} ${peers.length === 1 ? "character shares" : "characters share"} this exact profile (${names.join(", ")}${more}); ${dead} of them ${dead === 1 ? "dies" : "die"}.${few}`;
  }
}

const shortName = (name) => (name.length > 16 ? `${name.slice(0, 15)}…` : name);

/* ---------- Findings ---------- */

function medianOf(curve) {
  const i = curve.surv.findIndex((s) => s <= 0.5);
  return i < 0 ? null : curve.time[i];
}

function defaultLevels(term) {
  if (term.levels.length <= 4) return term.levels.map((l) => l.level);
  return term.levels.filter((l) => !VAGUE.has(l.level)).slice(0, 4).map((l) => l.level);
}

function findingText(name) {
  const by = story.km.by[name];
  const hours = (level) => { const m = medianOf(by[level]); return m === null ? "more than 60 hours" : `${Math.round(m)} hours`; };
  const p = story.logrank[name];
  const test = ` (Log-rank test: p ${p < 0.001 ? "< 0.001" : `= ${p.toFixed(3)}`}.)`;
  const text = {
    sex: () => `Women last longer. Half the men are dead within ${hours("Male")} of first appearing; for women it takes ${hours("Female")}.`,
    social_status: () => `Rank buys time, for a while. The median highborn character lasts ${hours("Highborn")}, the median lowborn one ${hours("Lowborn")}. By the finale the gap has closed.`,
    occupation: () => "Occupation matters least. “Silk collar” covers the court, the clergy and merchants; “boiled leather collar” covers soldiers, servants and labourers. Their curves cross, and the long-lived group is the one whose occupation was never stated.",
    religion: () => "Faith is unknown for two characters in three, so read this one with care. Followers of the Drowned God and of the Great Stallion tend to die soon after they appear.",
    allegiance: () => "Allegiance separates the curves most. Starks and Targaryens endure. Boltons and Greyjoys mostly do not, and many Greyjoys are dead within hours of first appearing.",
  }[name];
  return (text ? text() : "") + test;
}

function renderFindings(withFlash = false) {
  const term = model.terms.find((t) => t.name === state.term);
  $("term-tabs").innerHTML = model.terms.map((t) => `<button type="button" data-term="${t.name}" aria-pressed="${t.name === term.name}">${esc(t.label)}</button>`).join("");
  const shown = state.shown[term.name] || (state.shown[term.name] = defaultLevels(term));
  $("level-chips").innerHTML = term.levels.length > 4
    ? term.levels.map((l) => `<button type="button" data-level="${esc(l.level)}" aria-pressed="${shown.includes(l.level)}">${esc(l.level)} <span class="num">${l.n}</span></button>`).join("")
    : "";
  const by = story.km.by[term.name];
  drawSurvival($("km-chart"), {
    xMax: model.meta.horizon_hrs,
    series: term.levels.filter((l) => shown.includes(l.level)).map((l) => ({
      label: l.level, time: by[l.level].time, surv: by[l.level].surv,
      dash: DASHES[term.levels.indexOf(l) % DASHES.length],
    })),
  });
  if (withFlash) flash($("km-chart").parentNode);
  $("km-text").textContent = findingText(term.name);
}

function hazardGroups() {
  const reference = innerWidth < 560 ? "ref." : "reference";
  const tail = (1 - model.meta.interval_level) / 2;
  return model.terms.map((term) => ({
    label: term.label,
    rows: term.levels.map((l, i) => {
      if (i === 0) return { label: `${shortLevel(l.level) === l.level ? l.level : l.level.replace(" collar", "")} (${reference})`, hr: 1, lo: 1, hi: 1, reference: true };
      const col = model.boot.columns.indexOf(term.name + l.level);
      const draws = model.boot.coef.map((row) => row[col]).sort((a, b) => a - b);
      return { label: l.level, hr: Math.exp(l.coef), lo: Math.exp(quantile(draws, tail)), hi: Math.exp(quantile(draws, 1 - tail)) };
    }),
  }));
}

/* ---------- The trap ---------- */

function renderTrap() {
  const sim = story.trap.simulation;
  const horizon = Math.max(...Object.values(sim.curves).map((c) => c.time.at(-1)));
  drawSurvival($("sim-chart"), {
    xMax: Math.ceil(horizon),
    xLabel: "Hours",
    series: [
      { label: "Ever switched", time: sim.curves.Yes.time, surv: sim.curves.Yes.surv },
      { label: "Never switched", time: sim.curves.No.time, surv: sim.curves.No.surv, dash: "7 3" },
    ],
  });
}

function fillTrapTable() {
  const sim = story.trap.simulation.results;
  const real = story.trap.real;
  const naive = real.find((r) => r.analysis === "Naive");
  const landmark = real.find((r) => r.analysis === "Landmark at 10 h");
  const rows = [
    ["Simulation, naive: ever switched", sim[0], "Wrong. The truth is 1.0"],
    ["Simulation, counted from the switch", sim[1], "Recovers the truth"],
    ["Real data, naive", naive, "Same flaw as the first row"],
    ["Real data, alive at 10 hours only", landmark, "Bias partly removed"],
  ];
  $("trap-table").querySelector("tbody").innerHTML = rows.map(([label, r, verdict]) =>
    `<tr><th scope="row">${label}</th><td class="n">${r.hr.toFixed(2)}</td><td class="n">${r.lo.toFixed(2)} to ${r.hi.toFixed(2)}</td><td>${verdict}</td></tr>`).join("");
  fill("naive-cut", Math.round(100 * (1 - naive.hr)));
  fill("sim-n", story.trap.simulation.n.toLocaleString("en-GB"));
}

/* ---------- The cast ---------- */

function renderCast() {
  const found = searchCharacters(characters, state.query, state.fate);
  const shown = found.slice(0, state.castLimit);
  $("cast-count").textContent = found.length
    ? `${found.length} ${found.length === 1 ? "character" : "characters"}`
    : "Nobody by that name. Try a house, such as Stark.";
  $("cast-list").innerHTML = shown.map((c) => `<li><button type="button" data-id="${c.id}">
      <span class="who">${esc(c.name)}</span>
      <span class="side">${esc(c.allegiance)} · ${esc(c.social_status)}</span>
      <span class="fate">${c.dead ? "✕" : "○"} ${formatHours(c.time_hrs)}</span>
      ${state.marked && state.marked.id === c.id ? `<span class="how">${esc(fateText(c))}${c.dead && c.death && c.death.how ? `. ${esc(c.death.how)}.` : "."}</span>` : ""}
    </button></li>`).join("");
  $("cast-more").hidden = found.length <= state.castLimit;
}

function initCast() {
  $("cast-search").addEventListener("input", (e) => { state.query = e.target.value; state.castLimit = CAST_PAGE; renderCast(); });
  $("cast-fate").addEventListener("click", (e) => {
    const b = e.target.closest("button[data-fate]");
    if (!b) return;
    state.fate = b.dataset.fate;
    state.castLimit = CAST_PAGE;
    $("cast-fate").querySelectorAll("button").forEach((x) => x.setAttribute("aria-pressed", String(x === b)));
    renderCast();
  });
  $("cast-more").addEventListener("click", () => { state.castLimit += CAST_PAGE * 2; renderCast(); });
  $("cast-list").addEventListener("click", (e) => {
    const b = e.target.closest("button[data-id]");
    if (!b) return;
    const c = characters.find((x) => String(x.id) === b.dataset.id);
    state.marked = c;
    state.profile = profileOf(model, c);
    renderCast();
    renderPredict(true);
    $("predict-fig").scrollIntoView({ block: "center" });
  });
}

/* ---------- Method ---------- */

function fillMetrics() {
  const names = { cox: "Cox", enet: "Elastic net", rsf: "Random forest", "Null model": "No covariates" };
  const order = ["Null model", "cox", "enet", "rsf"];
  const get = (m, metric, horizon) => story.metrics.find((r) => r.model === m && r.metric === metric && (horizon === undefined || r.horizon === horizon));
  const cols = [["C-index", "c_index"], ["AUC at 30 h", "auc", 30], ["Brier, 0 to 50 h", "ibs"]];
  const table = $("metrics-table");
  table.querySelector("thead").innerHTML = `<tr><th scope="col">Model</th>${cols.map((c) => `<th scope="col" class="n">${c[0]}</th>`).join("")}</tr>`;
  table.querySelector("tbody").innerHTML = order.map((m) => {
    const cells = cols.map(([, metric, horizon]) => { const r = get(m, metric, horizon); return `<td class="n">${r ? r.mean.toFixed(3) : "–"}</td>`; }).join("");
    const picked = m === story.deployed;
    return `<tr${picked ? ' class="picked"' : ""}><th scope="row">${names[m]}${picked ? " (on this page)" : ""}</th>${cells}</tr>`;
  }).join("");
  fill("cindex", Math.round(100 * get(story.deployed, "c_index").mean));
}

/* ---------- Start ---------- */

async function load(path) {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`${path}: ${response.status}`);
  return response.json();
}

async function start() {
  initPaper();
  try {
    [model, story, characters] = await Promise.all([load("data/model.json"), load("data/story.json"), load("data/characters.json")]);
  } catch (error) {
    $("verdict").innerHTML = 'The model could not be loaded. Try reloading, or read the <a href="report/">full analysis</a>.';
    console.error(error);
    return;
  }
  state.profile = referenceProfile(model);
  fill("horizon", Math.round(model.meta.horizon_hrs));
  fill("n", model.meta.n);
  fill("deaths", model.meta.deaths);
  fill("median", Math.round(story.median_hrs));

  buildControls();
  renderPredict();
  renderFindings();
  drawForest($("hr-chart"), hazardGroups());
  renderTrap();
  fillTrapTable();
  fillMetrics();
  initCast();
  renderCast();

  $("term-tabs").addEventListener("click", (e) => {
    const b = e.target.closest("button[data-term]");
    if (b) { state.term = b.dataset.term; renderFindings(true); }
  });
  $("level-chips").addEventListener("click", (e) => {
    const b = e.target.closest("button[data-level]");
    if (!b) return;
    const shown = state.shown[state.term];
    const i = shown.indexOf(b.dataset.level);
    if (i >= 0) { if (shown.length > 1) shown.splice(i, 1); } else shown.push(b.dataset.level);
    renderFindings(true);
  });

  // Charts are sized to their container, so redraw when it changes width.
  let width = innerWidth;
  let timer;
  addEventListener("resize", () => {
    if (innerWidth === width) return;
    width = innerWidth;
    clearTimeout(timer);
    timer = setTimeout(() => { renderPredict(); renderFindings(); drawForest($("hr-chart"), hazardGroups()); renderTrap(); }, 120);
  });
}

start();
