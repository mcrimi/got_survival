// Pure geometry for the charts: scales and SVG path strings.

export function linearScale([d0, d1], [r0, r1]) {
  const k = d1 === d0 ? 0 : (r1 - r0) / (d1 - d0);
  return (v) => r0 + (v - d0) * k;
}

const f = (n) => (Math.round(n * 10) / 10).toString();

/** A survival curve drawn as steps: horizontal, then vertical at each drop. */
export function stepPath(time, values, sx, sy, until = Infinity) {
  if (!time.length) return "";
  let d = `M${f(sx(time[0]))},${f(sy(values[0]))}`;
  for (let i = 1; i < time.length && time[i] <= until; i += 1) {
    d += `H${f(sx(time[i]))}V${f(sy(values[i]))}`;
  }
  const end = Math.min(until, Infinity);
  if (Number.isFinite(end) && end > time[0]) d += `H${f(sx(end))}`;
  return d;
}

/** Closed outline between a lower and an upper step curve on a shared grid. */
export function bandPath(time, lo, hi, sx, sy) {
  if (!time.length) return "";
  let d = `M${f(sx(time[0]))},${f(sy(hi[0]))}`;
  for (let i = 1; i < time.length; i += 1) d += `H${f(sx(time[i]))}V${f(sy(hi[i]))}`;
  const last = time.length - 1;
  d += `V${f(sy(lo[last]))}`;
  for (let i = last; i > 0; i -= 1) d += `H${f(sx(time[i]))}V${f(sy(lo[i - 1]))}`;
  return `${d}Z`;
}

/** Spread label positions apart so they do not overlap; keeps their order. */
export function spreadLabels(ys, gap, min, max) {
  const order = ys.map((y, i) => [y, i]).sort((a, b) => a[0] - b[0]);
  const out = new Array(ys.length);
  let prev = -Infinity;
  for (const [y, i] of order) {
    const placed = Math.max(y, prev + gap, min);
    out[i] = placed;
    prev = placed;
  }
  const overflow = prev - max;
  if (overflow > 0) {
    let ceiling = max;
    for (let k = order.length - 1; k >= 0; k -= 1) {
      const i = order[k][1];
      out[i] = Math.min(out[i], ceiling);
      ceiling = out[i] - gap;
    }
  }
  return out;
}
