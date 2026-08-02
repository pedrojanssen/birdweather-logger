const numberFormat = new Intl.NumberFormat("en-US");
const dateFormat = new Intl.DateTimeFormat("en-US", {
  day: "numeric",
  month: "short",
  year: "numeric",
  timeZone: "UTC",
});
const shortDateFormat = new Intl.DateTimeFormat("en-US", {
  day: "numeric",
  month: "short",
  timeZone: "UTC",
});

const state = {
  dashboard: null,
  periodKey: "7d",
  activityMode: "detections",
  showAllSpecies: false,
};

const byId = (id) => document.getElementById(id);
const escapeHtml = (value) =>
  String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");

function formatDate(value, short = false) {
  if (!value) return "—";
  return (short ? shortDateFormat : dateFormat).format(
    new Date(`${value}T12:00:00Z`),
  );
}

function formatConfidence(value) {
  return value == null ? "n/a" : `${Math.round(value * 100)}%`;
}

function hourLabel(hour) {
  return `${String(hour).padStart(2, "0")}:00`;
}

function signed(value, suffix = "") {
  if (value == null) return "No prior comparison";
  return `${value > 0 ? "+" : ""}${value}${suffix}`;
}

function scaledHeight(value, maximum, minimum = 2) {
  if (!maximum || !value) return minimum;
  return Math.max(minimum, Math.round((value / maximum) * 100));
}

function renderSparkline(id, values, coral = false) {
  const container = byId(id);
  if (!values.length) {
    container.innerHTML = "";
    return;
  }
  const width = 128;
  const height = 50;
  const minimum = Math.min(...values);
  const maximum = Math.max(...values);
  const spread = maximum - minimum || 1;
  const points = values
    .map((value, index) => {
      const x =
        values.length === 1 ? width / 2 : (index / (values.length - 1)) * width;
      const y = height - 5 - ((value - minimum) / spread) * (height - 12);
      return `${x.toFixed(1)},${y.toFixed(1)}`;
    })
    .join(" ");
  container.innerHTML = `<svg viewBox="0 0 ${width} ${height}" preserveAspectRatio="none">
    <polyline class="${coral ? "coral" : ""}" points="${points}"></polyline>
  </svg>`;
}

function renderHero(period) {
  const change = period.comparison.detection_change_percent;
  const periodName = state.periodKey === "7d" ? "week" : "period";
  let lead = `A steady ${periodName}`;
  if (change != null && change <= -3) lead = `A quieter ${periodName}`;
  if (change != null && change >= 3) lead = `A livelier ${periodName}`;
  if (state.periodKey === "all") lead = "The complete field record";
  const visitors = period.new_species_count;
  const visitorPhrase =
    visitors && state.periodKey !== "all"
      ? `, with ${numberFormat.format(visitors)} new visitor${visitors === 1 ? "" : "s"}`
      : "";
  byId("hero-title").textContent = `${lead}${visitorPhrase}`;

  const direction =
    change == null
      ? "Across the record"
      : `${change < 0 ? "Below" : "Above"} the previous ${periodName} by ${Math.abs(change)}%`;
  byId("hero-summary").textContent =
    `${direction}, with ${numberFormat.format(period.species_count)} species heard between ${formatDate(period.start_date, true)} and ${formatDate(period.end_date)}.`;
}

function renderMetrics(period) {
  byId("metric-detections").textContent = numberFormat.format(
    period.total_detections,
  );
  byId("metric-species").textContent = numberFormat.format(
    period.species_count,
  );
  byId("metric-peak-hour").textContent = hourLabel(period.peak_hour.hour);
  byId("metric-new-species").textContent = numberFormat.format(
    period.new_species_count,
  );
  byId("metric-detection-change").textContent =
    period.comparison.detection_change_percent == null
      ? `${numberFormat.format(period.active_days)} active days`
      : `${signed(period.comparison.detection_change_percent, "%")} vs prior ${state.periodKey === "7d" ? "week" : "period"}`;
  byId("metric-species-change").textContent =
    period.comparison.species_change == null
      ? "Distinct species heard"
      : `${signed(period.comparison.species_change)} vs prior ${state.periodKey === "7d" ? "week" : "period"}`;
  byId("metric-peak-calls").textContent =
    `${numberFormat.format(period.peak_hour.detections)} calls`;
  byId("metric-all-species").textContent =
    `${numberFormat.format(state.dashboard.periods.all.species_count)} heard all-time`;

  renderSparkline(
    "spark-detections",
    period.daily_activity.map((row) => row.detections),
  );
  renderSparkline(
    "spark-species",
    period.daily_activity.map((row) => row.species_count),
  );
  renderSparkline(
    "spark-hourly",
    period.hourly_activity.map((row) => row.detections),
  );
  const cumulativeNew = [];
  let running = 0;
  for (const day of period.daily_activity) {
    running += period.new_species.filter(
      (species) => species.first_seen_date === day.date,
    ).length;
    cumulativeNew.push(running);
  }
  renderSparkline("spark-new", cumulativeNew, true);
}

function photoMarkup(species, className = "species-photo") {
  const photos =
    species.photo_urls || (species.photo_url ? [species.photo_url] : []);
  const initial = escapeHtml(species.common_name.charAt(0));
  if (!photos.length) {
    return `<span class="${className} photo-fallback" aria-hidden="true">${initial}</span>`;
  }
  return `<span class="photo-shell">
    <img class="${className}" src="${escapeHtml(photos[0])}" data-photo-fallbacks="${escapeHtml(photos.slice(1).join("|"))}" alt="${escapeHtml(species.common_name)}" loading="lazy" referrerpolicy="no-referrer" />
    <span class="${className} photo-fallback" aria-hidden="true">${initial}</span>
  </span>`;
}

function attachPhotoFallbacks(container) {
  container.querySelectorAll("img[data-photo-fallbacks]").forEach((image) => {
    image.addEventListener("error", () => {
      const fallbacks = image.dataset.photoFallbacks.split("|").filter(Boolean);
      const next = fallbacks.shift();
      if (next) {
        image.dataset.photoFallbacks = fallbacks.join("|");
        image.src = next;
      } else {
        image.remove();
      }
    });
  });
}

function renderTopSpecies(speciesRows) {
  const maximum = Math.max(
    ...speciesRows.map((species) => species.detections),
    1,
  );
  const container = byId("top-species");
  container.innerHTML = speciesRows
    .slice(0, 7)
    .map((species, index) => {
      const change = species.change_percent;
      const trendClass =
        change == null ? "neutral" : change >= 0 ? "up" : "down";
      return `<article class="leader-row">
      <span class="leader-rank">${String(index + 1).padStart(2, "0")}</span>
      ${photoMarkup(species, "leader-photo")}
      <div class="leader-copy">
        <div><h3>${escapeHtml(species.common_name)}</h3><span class="trend ${trendClass}">${change == null ? "new" : signed(change, "%")}</span></div>
        <p>${escapeHtml(species.scientific_name || "Scientific name unavailable")}</p>
        <div class="leader-track"><i style="width:${Math.max(4, (species.detections / maximum) * 100)}%"></i></div>
      </div>
      <strong>${numberFormat.format(species.detections)}</strong>
    </article>`;
    })
    .join("");
  attachPhotoFallbacks(container);
}

function renderHourlyActivity(period) {
  const mode = state.activityMode;
  const rows = period.hourly_activity;
  const values = rows.map((row) => row[mode]);
  const maximum = Math.max(...values, 1);
  byId("hourly-chart").innerHTML = rows
    .map((row) => {
      const value = row[mode];
      const label = mode === "detections" ? "detections" : "species";
      return `<span class="hour-column" style="height:${scaledHeight(value, maximum)}%" title="${hourLabel(row.hour)} · ${numberFormat.format(value)} ${label}"></span>`;
    })
    .join("");

  const center = 120;
  const spokes = rows
    .map((row) => {
      const angle = (row.hour / 24) * Math.PI * 2 - Math.PI / 2;
      const inner = 66;
      const outer = 72 + (row[mode] / maximum) * 34;
      const x1 = center + Math.cos(angle) * inner;
      const y1 = center + Math.sin(angle) * inner;
      const x2 = center + Math.cos(angle) * outer;
      const y2 = center + Math.sin(angle) * outer;
      return `<line x1="${x1.toFixed(1)}" y1="${y1.toFixed(1)}" x2="${x2.toFixed(1)}" y2="${y2.toFixed(1)}"></line>`;
    })
    .join("");
  byId("activity-clock").innerHTML =
    `<svg viewBox="0 0 240 240" aria-hidden="true">
    <circle class="clock-ring" cx="120" cy="120" r="87"></circle>
    <g class="clock-spokes">${spokes}</g>
    <text class="clock-label clock-label--top" x="120" y="19">00</text>
    <text class="clock-label" x="222" y="124">06</text>
    <text class="clock-label" x="120" y="232">12</text>
    <text class="clock-label" x="17" y="124">18</text>
    <text class="clock-peak" x="120" y="111">PEAK</text>
    <text class="clock-time" x="120" y="139">${hourLabel(period.peak_hour.hour)}</text>
  </svg>`;
  byId("rhythm-note").textContent =
    mode === "detections"
      ? `The busiest aggregate hour begins at ${hourLabel(period.peak_hour.hour)} Costa Rica time.`
      : "Species diversity by hour, counted across the selected period.";
}

function renderDailyChart(rows) {
  const container = byId("daily-chart");
  if (!rows.length) {
    container.textContent = "No daily data available.";
    return;
  }
  const width = 840;
  const height = 255;
  const padding = { top: 16, right: 14, bottom: 34, left: 48 };
  const plotWidth = width - padding.left - padding.right;
  const plotHeight = height - padding.top - padding.bottom;
  const maximum = Math.max(...rows.map((row) => row.detections), 1);
  const x = (index) =>
    padding.left +
    (rows.length === 1
      ? plotWidth / 2
      : (index / (rows.length - 1)) * plotWidth);
  const y = (value) =>
    padding.top + plotHeight - (value / maximum) * plotHeight;
  const points = rows
    .map((row, index) => `${x(index)},${y(row.detections)}`)
    .join(" ");
  const area = `${padding.left},${padding.top + plotHeight} ${points} ${x(rows.length - 1)},${padding.top + plotHeight}`;
  const labelIndexes = [
    ...new Set([0, Math.floor((rows.length - 1) / 2), rows.length - 1]),
  ];
  container.innerHTML = `<svg viewBox="0 0 ${width} ${height}" preserveAspectRatio="none" aria-hidden="true">
    <defs><linearGradient id="activity-fill" x1="0" x2="0" y1="0" y2="1"><stop offset="0%" stop-color="#35ded9" stop-opacity=".28"/><stop offset="100%" stop-color="#35ded9" stop-opacity="0"/></linearGradient></defs>
    ${[0, Math.round(maximum / 2), maximum].map((value) => `<line class="grid-line" x1="${padding.left}" x2="${width - padding.right}" y1="${y(value)}" y2="${y(value)}"></line><text class="axis-label" x="${padding.left - 9}" y="${y(value) + 4}" text-anchor="end">${numberFormat.format(value)}</text>`).join("")}
    <polygon class="chart-area" points="${area}"></polygon><polyline class="chart-line" points="${points}"></polyline>
    ${labelIndexes.map((index) => `<circle class="chart-dot" cx="${x(index)}" cy="${y(rows[index].detections)}" r="3"></circle><text class="axis-label" x="${x(index)}" y="${height - 6}" text-anchor="${index === 0 ? "start" : index === rows.length - 1 ? "end" : "middle"}">${escapeHtml(formatDate(rows[index].date, true))}</text>`).join("")}
  </svg>`;
}

function renderHeatmap(speciesRows) {
  const rows = speciesRows.slice(0, 10);
  const maximum = Math.max(
    ...rows.flatMap((species) =>
      species.hourly_activity.map((hour) => hour.detections),
    ),
    1,
  );
  byId("species-heatmap").innerHTML = `<table class="heatmap-table">
    <thead><tr><th>Species</th>${Array.from({ length: 24 }, (_, hour) => `<th>${hour % 3 === 0 ? String(hour).padStart(2, "0") : ""}</th>`).join("")}</tr></thead>
    <tbody>${rows
      .map(
        (species) =>
          `<tr><th title="${escapeHtml(species.common_name)}">${escapeHtml(species.common_name)}</th>${species.hourly_activity
            .map((hour) => {
              const intensity = hour.detections
                ? 0.18 + (hour.detections / maximum) * 0.82
                : 0.035;
              return `<td><i style="--heat:${intensity.toFixed(3)}" title="${escapeHtml(species.common_name)} · ${hourLabel(hour.hour)} · ${numberFormat.format(hour.detections)} detections"></i></td>`;
            })
            .join("")}</tr>`,
      )
      .join("")}</tbody>
  </table>`;
}

function renderNewSpecies(period) {
  const container = byId("new-species");
  const rows = period.new_species;
  byId("visitor-note").textContent = rows.length
    ? `${numberFormat.format(period.new_species_count)} species first heard during this period`
    : "No first-time species in this period";
  if (!rows.length) {
    container.innerHTML = `<p class="empty-state">No new visitors appeared in the selected period. The regular chorus still has plenty to explore.</p>`;
    return;
  }
  container.innerHTML = rows
    .map(
      (species) => `<article class="visitor-card">
    ${photoMarkup(species, "visitor-photo")}
    <div><span>First heard ${formatDate(species.first_seen_date, true)}</span><h3>${escapeHtml(species.common_name)}</h3><p>${escapeHtml(species.scientific_name || "Scientific name unavailable")}</p><strong>${numberFormat.format(species.detections)} detections · ${formatConfidence(species.average_confidence)}</strong></div>
  </article>`,
    )
    .join("");
  attachPhotoFallbacks(container);
}

function renderSpeciesTable(speciesRows) {
  const query = byId("species-search").value.trim().toLocaleLowerCase();
  const filtered = speciesRows.filter((species) =>
    `${species.common_name} ${species.scientific_name || ""}`
      .toLocaleLowerCase()
      .includes(query),
  );
  const visible =
    state.showAllSpecies || query ? filtered : filtered.slice(0, 12);
  byId("species-table").innerHTML = visible
    .map(
      (species) => `<tr>
    <td><span>${escapeHtml(species.common_name)}</span><small>${escapeHtml(species.scientific_name || "—")}</small></td>
    <td>${numberFormat.format(species.detections)}</td><td>${numberFormat.format(species.active_days)}</td>
    <td><span class="confidence"><i style="width:${(species.average_confidence || 0) * 100}%"></i></span>${formatConfidence(species.average_confidence)}</td>
    <td>${formatDate(species.first_seen_date, true)}</td>
  </tr>`,
    )
    .join("");
  byId("species-result-count").textContent =
    `Showing ${numberFormat.format(visible.length)} of ${numberFormat.format(filtered.length)} species`;
  const button = byId("show-all-species");
  button.hidden = Boolean(query) || filtered.length <= 12;
  button.textContent = state.showAllSpecies ? "Show fewer" : "Show all species";
}

function renderReviewCandidates(rows) {
  const container = byId("review-candidates");
  if (!rows.length) {
    container.innerHTML = `<p class="empty-state">No aggregate signals are currently flagged for review.</p>`;
    return;
  }
  container.innerHTML = rows
    .map(
      (species) => `<article>
    <span class="review-pulse" aria-hidden="true"></span>
    <div><h3>${escapeHtml(species.common_name)}</h3><p>${escapeHtml(species.reason)} · ${numberFormat.format(species.detections)} detections</p></div>
    <strong>${formatConfidence(species.average_confidence)}</strong>
  </article>`,
    )
    .join("");
}

function render() {
  const dashboard = state.dashboard;
  const period = dashboard.periods[state.periodKey];
  renderHero(period);
  renderMetrics(period);
  renderTopSpecies(period.top_species);
  renderHourlyActivity(period);
  renderDailyChart(period.daily_activity);
  renderHeatmap(period.top_species);
  renderNewSpecies(period);
  renderSpeciesTable(period.species);
  renderReviewCandidates(period.review_candidates);

  byId("daily-range").textContent =
    `${formatDate(period.start_date)} — ${formatDate(period.end_date)}`;
  byId("freshness-label").textContent =
    `Through ${formatDate(dashboard.latest_observation_date)}`;
  byId("footer-timezone").textContent = dashboard.timezone;
  byId("footer-updated").textContent =
    `Updated ${formatDate(dashboard.generated_date)}`;
  document.querySelectorAll("[data-period]").forEach((button) => {
    const active = button.dataset.period === state.periodKey;
    button.classList.toggle("is-active", active);
    button.setAttribute("aria-pressed", String(active));
  });
  document.querySelectorAll("[data-activity-mode]").forEach((button) => {
    const active = button.dataset.activityMode === state.activityMode;
    button.classList.toggle("is-active", active);
    button.setAttribute("aria-pressed", String(active));
  });
}

async function initialise() {
  try {
    const response = await fetch("data/dashboard.json?schema=2", {
      cache: "no-store",
    });
    if (!response.ok)
      throw new Error(`Dashboard data returned ${response.status}`);
    state.dashboard = await response.json();
    if (state.dashboard.schema_version !== 2)
      throw new Error("Dashboard data schema is still updating");
    state.periodKey = state.dashboard.default_period;
    render();
  } catch (error) {
    const message = byId("error-message");
    message.hidden = false;
    message.textContent =
      "The dashboard data could not be loaded. Please try again later.";
    console.error(error);
  }
}

document.querySelectorAll("[data-period]").forEach((button) => {
  button.addEventListener("click", () => {
    if (!state.dashboard || !state.dashboard.periods[button.dataset.period])
      return;
    state.periodKey = button.dataset.period;
    state.showAllSpecies = false;
    byId("species-search").value = "";
    render();
  });
});
document.querySelectorAll("[data-activity-mode]").forEach((button) => {
  button.addEventListener("click", () => {
    state.activityMode = button.dataset.activityMode;
    if (state.dashboard)
      renderHourlyActivity(state.dashboard.periods[state.periodKey]);
    document
      .querySelectorAll("[data-activity-mode]")
      .forEach((item) => item.classList.toggle("is-active", item === button));
  });
});
byId("species-search").addEventListener("input", () => {
  if (state.dashboard)
    renderSpeciesTable(state.dashboard.periods[state.periodKey].species);
});
byId("show-all-species").addEventListener("click", () => {
  state.showAllSpecies = !state.showAllSpecies;
  renderSpeciesTable(state.dashboard.periods[state.periodKey].species);
});

initialise();
