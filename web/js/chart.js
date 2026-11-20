// Ink-only SVG charts. Colours come from CSS variables (see got.css), so the
// same drawing works on Warm, Grey and Night paper.

import { linearScale, stepPath, bandPath, spreadLabels } from "./geometry.js";

// Line patterns that tell groups apart without colour.
export const DASHES = ["", "7 3", "1.5 3.5", "9 3 1.5 3", "4 3", "12 4", "1.5 6", "12 3 4 3", "5 2 1 2"];

const esc = (s) => String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);
const r = (n) => Math.round(n * 10) / 10;

function size(svg, ratio, minHeight) {
  const width = Math.max(280, Math.round(svg.getBoundingClientRect().width || svg.parentNode.clientWidth || 640));
  const height = Math.max(minHeight, Math.round(width * ratio));
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
  return { width, height };
}

const hatch = `<defs><pattern id="hatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)"><line class="hatch-line" x1="0" y1="0" x2="0" y2="5"/></pattern></defs>`;

/**
 * Survival curves as step lines with labels at their right-hand ends.
 * series: [{ label, time, surv, dash, cls }]
 * band:   { time, lo, hi } hatched region, optional
 * marker: { t, s, dead } a real character's fate, optional
 */
export function drawSurvival(svg, { xMax, series, band = null, marker = null, xLabel = "Hours on screen since first appearing" }) {
  const narrow = (svg.getBoundingClientRect().width || 640) < 520;
  const { width, height } = size(svg, narrow ? 0.72 : 0.5, 240);
  const m = { top: 10, right: narrow ? 96 : 132, bottom: 38, left: 38 };
  const sx = linearScale([0, xMax], [m.left, width - m.right]);
  const sy = linearScale([0, 1], [height - m.bottom, m.top]);
  let out = hatch;

  for (const p of [0.25, 0.5, 0.75, 1]) out += `<line class="grid" x1="${m.left}" x2="${r(sx(xMax))}" y1="${r(sy(p))}" y2="${r(sy(p))}"/>`;
  for (const p of [0, 0.5, 1]) out += `<text x="${m.left - 7}" y="${r(sy(p)) + 4}" text-anchor="end">${p * 100}%</text>`;
  const step = xMax > 40 ? 10 : xMax > 15 ? 5 : 2;
  for (let t = 0; t <= xMax; t += step) {
    out += `<line class="axis" x1="${r(sx(t))}" x2="${r(sx(t))}" y1="${height - m.bottom}" y2="${height - m.bottom + 4}"/>`;
    out += `<text x="${r(sx(t))}" y="${height - m.bottom + 16}" text-anchor="middle">${t}</text>`;
  }
  out += `<text x="${m.left}" y="${height - 4}">${esc(xLabel)}</text>`;
  out += `<path class="axis" d="M${m.left},${m.top}V${height - m.bottom}H${r(sx(xMax))}"/>`;

  if (band) out += `<path class="band" d="${bandPath(band.time, band.lo, band.hi, sx, sy)}"/>`;

  const ends = series.map((s) => sy(s.surv[s.surv.length - 1]));
  const ys = spreadLabels(ends, 13, m.top + 4, height - m.bottom - 2);
  series.forEach((s, i) => {
    const last = s.time[s.time.length - 1];
    out += `<path class="curve ${s.cls || ""}" d="${stepPath(s.time, s.surv, sx, sy)}"${s.dash ? ` stroke-dasharray="${s.dash}"` : ""}/>`;
    const x = sx(Math.min(last, xMax));
    if (Math.abs(ys[i] - ends[i]) > 2 || x < sx(xMax) - 2) {
      out += `<path class="grid" d="M${r(x + 2)},${r(ends[i])}L${r(sx(xMax) + 5)},${r(ys[i])}"/>`;
    }
    out += `<text class="lbl" x="${r(sx(xMax) + 8)}" y="${r(ys[i] + 4)}">${esc(s.label)}</text>`;
  });

  if (marker) {
    const x = sx(Math.min(marker.t, xMax));
    const y = sy(marker.s);
    out += marker.dead
      ? `<path class="mark-x" d="M${r(x - 5)},${r(y - 5)}L${r(x + 5)},${r(y + 5)}M${r(x - 5)},${r(y + 5)}L${r(x + 5)},${r(y - 5)}"/>`
      : `<circle class="mark-dot" cx="${r(x)}" cy="${r(y)}" r="5"/>`;
  }
  svg.innerHTML = out;
}

/**
 * Hazard ratios on a log axis, one row per level, grouped by covariate.
 * groups: [{ label, rows: [{ label, hr, lo, hi, reference }] }]
 */
export function drawForest(svg, groups) {
  const rowH = 22;
  const head = 26;
  const count = groups.reduce((n, g) => n + g.rows.length, 0);
  const narrow = (svg.getBoundingClientRect().width || 640) < 520;
  const m = { top: 6, right: 14, bottom: 34, left: narrow ? 138 : 200 };
  const total = m.top + groups.length * head + count * rowH + m.bottom;
  const { width } = size(svg, 0, total);
  svg.setAttribute("viewBox", `0 0 ${width} ${total}`);

  const all = groups.flatMap((g) => g.rows.flatMap((row) => [row.lo, row.hi]));
  const min = Math.min(0.5, ...all);
  const max = Math.max(2, ...all);
  const sx = linearScale([Math.log(min), Math.log(max)], [m.left, width - m.right]);
  const bottom = total - m.bottom;
  let out = "";
  let y = m.top;

  for (const tick of [0.25, 0.5, 1, 2, 4, 8].filter((t) => t >= min && t <= max)) {
    const x = r(sx(Math.log(tick)));
    out += `<line class="${tick === 1 ? "ref" : "grid"}" x1="${x}" x2="${x}" y1="${m.top}" y2="${bottom}"/>`;
    out += `<text x="${x}" y="${bottom + 15}" text-anchor="middle">${tick}×</text>`;
  }
  out += `<text x="${m.left}" y="${total - 4}">← safer</text><text x="${width - m.right}" y="${total - 4}" text-anchor="end">deadlier →</text>`;

  for (const g of groups) {
    out += `<text class="grp-lbl" x="0" y="${y + 17}">${esc(g.label)}</text>`;
    y += head;
    for (const row of g.rows) {
      const cy = y + rowH / 2;
      out += `<text class="row-lbl" x="${m.left - 10}" y="${cy + 4}" text-anchor="end">${esc(row.label)}</text>`;
      if (!row.reference) out += `<line class="whisker" x1="${r(sx(Math.log(row.lo)))}" x2="${r(sx(Math.log(row.hi)))}" y1="${cy}" y2="${cy}"/>`;
      const x = sx(Math.log(row.hr));
      out += row.reference
        ? `<rect class="mark-dot" x="${r(x - 3.5)}" y="${cy - 3.5}" width="7" height="7" style="stroke-width:1"/>`
        : `<rect class="sq" x="${r(x - 3.5)}" y="${cy - 3.5}" width="7" height="7"/>`;
      y += rowH;
    }
  }
  svg.innerHTML = out;
}

/** Briefly invert a figure, like an e-ink panel refreshing. No easing. */
export function flash(figure) {
  if (!figure || matchMedia("(prefers-reduced-motion: reduce)").matches) return;
  try { if (JSON.parse(localStorage.getItem("mcrimi-site") || "{}").refresh === "off") return; } catch (e) { /* storage unavailable */ }
  figure.classList.add("flash");
  setTimeout(() => figure.classList.remove("flash"), 90);
}
