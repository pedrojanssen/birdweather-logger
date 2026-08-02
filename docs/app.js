const numberFormat = new Intl.NumberFormat("en-US");
const dateFormat = new Intl.DateTimeFormat("en-US", {
  day: "numeric",
  month: "short",
  year: "numeric",
  timeZone: "UTC",
});

const state = {
  dashboard: null,
  periodKey: "7d",
};

const byId = (id) => document.getElementById(id);
const escapeHtml = (value) =>
  String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");

function formatDate(value) {
  return dateFormat.format(new Date(`${value}T12:00:00Z`));
}

function formatConfidence(value) {
  return value == null ? "n/a" : `${Math.round(value * 100)}%`;
}

function scaledHeight(value, maximum, minimum = 2) {
  if (!maximum || !value) return minimum;
  return Math.max(minimum, Math.round((value / maximum) * 100));
}

function renderDailyChart(rows) {
  const container = byId("daily-chart");
  if (!rows.length) {
    container.textContent = "No daily data available.";
    return;
  }

  const width = 800;
  const height = 245;
  const padding = { top: 12, right: 12, bottom: 30, left: 40 };
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
  const gridValues = [0, Math.round(maximum / 2), maximum];
  const labelIndexes = [
    ...new Set([0, Math.floor((rows.length - 1) / 2), rows.length - 1]),
  ];

  container.innerHTML = `
    <svg class="line-chart" viewBox="0 0 ${width} ${height}" preserveAspectRatio="none" aria-hidden="true">
      <defs>
        <linearGradient id="activity-fill" x1="0" x2="0" y1="0" y2="1">
          <stop offset="0%" stop-color="#78cbb8" stop-opacity="0.45"></stop>
          <stop offset="100%" stop-color="#78cbb8" stop-opacity="0.03"></stop>
        </linearGradient>
      </defs>
      ${gridValues
        .map(
          (value) => `
            <line class="grid-line" x1="${padding.left}" x2="${width - padding.right}" y1="${y(value)}" y2="${y(value)}"></line>
            <text x="${padding.left - 8}" y="${y(value) + 4}" text-anchor="end">${numberFormat.format(value)}</text>
          `,
        )
        .join("")}
      <polygon class="area" points="${area}"></polygon>
      <polyline class="line" points="${points}"></polyline>
      ${labelIndexes
        .map(
          (index) => `
            <text x="${x(index)}" y="${height - 5}" text-anchor="${index === 0 ? "start" : index === rows.length - 1 ? "end" : "middle"}">
              ${escapeHtml(formatDate(rows[index].date).replace(/, \d{4}$/, ""))}
            </text>
          `,
        )
        .join("")}
    </svg>`;
}

function renderHourlyChart(rows) {
  const maximum = Math.max(...rows.map((row) => row.detections), 1);
  byId("hourly-chart").innerHTML = rows
    .map(
      (row) => `
        <span
          class="hour-bar"
          style="height:${scaledHeight(row.detections, maximum)}%"
          title="${String(row.hour).padStart(2, "0")}:00–${String((row.hour + 1) % 24).padStart(2, "0")}:00 · ${numberFormat.format(row.detections)} detections"
        ></span>`,
    )
    .join("");
}

function speciesPhoto(species) {
  if (!species.photo_url) {
    return `<div class="species-photo species-photo--fallback" aria-hidden="true">${escapeHtml(species.common_name.charAt(0))}</div>`;
  }
  return `<img class="species-photo" src="${escapeHtml(species.photo_url)}" alt="${escapeHtml(species.common_name)}" loading="lazy" referrerpolicy="no-referrer" />`;
}

function renderTopSpecies(speciesRows) {
  byId("top-species").innerHTML = speciesRows
    .map((species) => {
      const maximum = Math.max(
        ...species.hourly_activity.map((row) => row.detections),
        1,
      );
      const miniHours = species.hourly_activity
        .map(
          (row) =>
            `<span style="height:${scaledHeight(row.detections, maximum, 1)}%" title="Hour ${row.hour}: ${row.detections}"></span>`,
        )
        .join("");
      return `
        <article class="species-card">
          ${speciesPhoto(species)}
          <div class="species-card-body">
            <h3 title="${escapeHtml(species.common_name)}">${escapeHtml(species.common_name)}</h3>
            <p class="scientific-name">${escapeHtml(species.scientific_name || "Scientific name unavailable")}</p>
            <div class="species-stats">
              <span>${numberFormat.format(species.detections)} detections</span>
              <span>${formatConfidence(species.average_confidence)}</span>
            </div>
            <div class="mini-hours" aria-label="Hourly activity pattern">${miniHours}</div>
          </div>
        </article>`;
    })
    .join("");
}

function renderSpeciesTable(speciesRows) {
  const query = byId("species-search").value.trim().toLocaleLowerCase();
  const filtered = speciesRows.filter((species) =>
    `${species.common_name} ${species.scientific_name || ""}`
      .toLocaleLowerCase()
      .includes(query),
  );

  byId("species-table").innerHTML = filtered
    .map(
      (species) => `
        <tr>
          <td>
            <span>${escapeHtml(species.common_name)}</span>
            <small>${escapeHtml(species.scientific_name || "—")}</small>
          </td>
          <td>${numberFormat.format(species.detections)}</td>
          <td>${numberFormat.format(species.active_days)}</td>
          <td>${formatConfidence(species.average_confidence)}</td>
        </tr>`,
    )
    .join("");
}

function render() {
  const dashboard = state.dashboard;
  const period = dashboard.periods[state.periodKey];
  byId("metric-detections").textContent = numberFormat.format(
    period.total_detections,
  );
  byId("metric-species").textContent = numberFormat.format(
    period.species_count,
  );
  byId("metric-days").textContent = numberFormat.format(period.active_days);
  byId("metric-confidence").textContent = formatConfidence(
    period.average_confidence,
  );
  byId("metric-period").textContent = period.label.toLocaleLowerCase();
  byId("daily-range").textContent =
    `${formatDate(period.start_date)} – ${formatDate(period.end_date)}`;
  byId("freshness-label").textContent =
    `Updated ${formatDate(dashboard.generated_date)} · data through ${formatDate(dashboard.latest_observation_date)}`;
  byId("footer-timezone").textContent = dashboard.timezone;

  document.querySelectorAll("[data-period]").forEach((button) => {
    button.classList.toggle(
      "is-active",
      button.dataset.period === state.periodKey,
    );
    button.setAttribute(
      "aria-pressed",
      String(button.dataset.period === state.periodKey),
    );
  });

  renderDailyChart(period.daily_activity);
  renderHourlyChart(period.hourly_activity);
  renderTopSpecies(period.top_species);
  renderSpeciesTable(period.species);
}

async function initialise() {
  try {
    const response = await fetch("data/dashboard.json", { cache: "no-store" });
    if (!response.ok)
      throw new Error(`Dashboard data returned ${response.status}`);
    state.dashboard = await response.json();
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
    byId("species-search").value = "";
    render();
  });
});

byId("species-search").addEventListener("input", () => {
  if (!state.dashboard) return;
  renderSpeciesTable(state.dashboard.periods[state.periodKey].species);
});

initialise();
