// Searching and describing the real characters.

/** Lower-case, strip accents, drop apostrophes, turn other punctuation into spaces. */
export function normalise(text) {
  return String(text ?? "")
    .normalize("NFD")
    .replace(/[̀-ͯ]/g, "")
    .toLowerCase()
    .replace(/['\u2019]/g, "")
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

/**
 * Characters whose name or allegiance contains every word of the query.
 * `fate` is "all", "dead" or "alive".
 */
export function searchCharacters(characters, query = "", fate = "all") {
  const words = normalise(query).split(" ").filter(Boolean);
  return characters.filter((c) => {
    if (fate === "dead" && !c.dead) return false;
    if (fate === "alive" && c.dead) return false;
    const haystack = normalise(`${c.name} ${c.allegiance}`);
    return words.every((w) => haystack.includes(w));
  });
}

export function formatHours(hours) {
  if (hours < 1) return `${Math.max(1, Math.round(hours * 60))} min`;
  return `${hours < 10 ? hours.toFixed(1) : Math.round(hours)} h`;
}

export function fateText(c) {
  if (!c.dead) return `Alive at the finale, after ${formatHours(c.time_hrs)} on screen`;
  const where = c.death && c.death.season ? ` (season ${c.death.season})` : "";
  return `Died ${formatHours(c.time_hrs)} after first appearing${where}`;
}

export function percent(p) {
  const value = 100 * p;
  if (value > 0 && value < 1) return "<1%";
  if (value > 99 && value < 100) return ">99%";
  return `${Math.round(value)}%`;
}
