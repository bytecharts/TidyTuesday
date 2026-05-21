const width = 1200;
const height = 720;
const margin = { top: 20, right: 320, bottom: 70, left: 80 };
const innerWidth = width - margin.left - margin.right;
const innerHeight = height - margin.top - margin.bottom;

const rootStyles = getComputedStyle(document.documentElement);
const colors = new Map([
  ["East Asia & Pacific", rootStyles.getPropertyValue("--east-asia").trim()],
  ["Europe & Central Asia", rootStyles.getPropertyValue("--europe").trim()],
  ["Latin America & the Caribbean", rootStyles.getPropertyValue("--latin-america").trim()],
  ["Middle East, North Africa, Afghanistan & Pakistan", rootStyles.getPropertyValue("--mena").trim()],
  ["North America", rootStyles.getPropertyValue("--north-america").trim()],
  ["South Asia", rootStyles.getPropertyValue("--south-asia").trim()],
  ["Sub-Saharan Africa", rootStyles.getPropertyValue("--sub-saharan").trim()],
]);
const grayColor = rootStyles.getPropertyValue("--gray").trim();
const grayLightColor = rootStyles.getPropertyValue("--gray-light").trim();
const primaryColor = rootStyles.getPropertyValue("--primary").trim() || colors.get("East Asia & Pacific") || grayColor;

const svg = d3
  .select("#chart")
  .append("svg")
  .attr("viewBox", `0 0 ${width} ${height}`)
  .attr("width", "100%");

const parseColor = (value) => {
  if (!value) return null;
  const trimmed = value.trim();
  if (trimmed.startsWith("#")) {
    const hex = trimmed.slice(1);
    if (hex.length === 3) {
      const r = parseInt(hex[0] + hex[0], 16);
      const g = parseInt(hex[1] + hex[1], 16);
      const b = parseInt(hex[2] + hex[2], 16);
      return [r, g, b];
    }
    if (hex.length === 6) {
      const r = parseInt(hex.slice(0, 2), 16);
      const g = parseInt(hex.slice(2, 4), 16);
      const b = parseInt(hex.slice(4, 6), 16);
      return [r, g, b];
    }
    return null;
  }
  const match = trimmed.match(/rgba?\(([^)]+)\)/i);
  if (!match) return null;
  const parts = match[1].split(",").map((part) => Number.parseFloat(part.trim()));
  if (parts.length < 3 || parts.some((part) => Number.isNaN(part))) return null;
  return parts.slice(0, 3);
};

const brightenColor = (value, amount = 0.55) => {
  const rgb = parseColor(value);
  if (!rgb) return value;
  const [r, g, b] = rgb;
  const mix = (channel) => Math.round(channel + (255 - channel) * amount);
  return `rgb(${mix(r)}, ${mix(g)}, ${mix(b)})`;
};

const defs = svg.append("defs");
const gradientId = (value) => `grad-${value.toLowerCase().replace(/[^a-z0-9]+/g, "-")}`;
const gradientIds = new Map(Array.from(colors.keys(), (key) => [key, gradientId(key)]));
const gradients = defs
  .selectAll("radialGradient")
  .data(Array.from(colors.entries()))
  .join("radialGradient")
  .attr("id", ([region]) => gradientIds.get(region))
  .attr("cx", "35%")
  .attr("cy", "30%")
  .attr("r", "60%");
gradients
  .append("stop")
  .attr("offset", "0%")
  .attr("stop-color", ([, color]) => brightenColor(color));
gradients
  .append("stop")
  .attr("offset", "20%")
  .attr("stop-color", ([, color]) => color)
  .attr("stop-opacity", 1);
gradients
  .append("stop")
  .attr("offset", "100%")
  .attr("stop-color", ([, color]) => color)
  .attr("stop-opacity", 1);

const defaultGradientId = "grad-default";
const mutedGradientId = "grad-muted";

const defaultGradient = defs
  .append("radialGradient")
  .attr("id", defaultGradientId)
  .attr("cx", "35%")
  .attr("cy", "30%")
  .attr("r", "60%");
defaultGradient
  .append("stop")
  .attr("offset", "0%")
  .attr("stop-color", brightenColor(primaryColor));
defaultGradient
  .append("stop")
  .attr("offset", "20%")
  .attr("stop-color", primaryColor)
  .attr("stop-opacity", 1);
defaultGradient
  .append("stop")
  .attr("offset", "100%")
  .attr("stop-color", primaryColor)
  .attr("stop-opacity", 1);

const mutedGradient = defs
  .append("radialGradient")
  .attr("id", mutedGradientId)
  .attr("cx", "35%")
  .attr("cy", "30%")
  .attr("r", "60%");
mutedGradient
  .append("stop")
  .attr("offset", "0%")
  .attr("stop-color", brightenColor(grayLightColor));
mutedGradient
  .append("stop")
  .attr("offset", "20%")
  .attr("stop-color", grayLightColor)
  .attr("stop-opacity", 1);
mutedGradient
  .append("stop")
  .attr("offset", "100%")
  .attr("stop-color", grayLightColor)
  .attr("stop-opacity", 1);

defs
  .append("filter")
  .attr("id", "point-shadow")
  .attr("x", "-25%")
  .attr("y", "-25%")
  .attr("width", "150%")
  .attr("height", "150%")
  .append("feDropShadow")
  .attr("dx", 2)
  .attr("dy", 3)
  .attr("stdDeviation", 2)
  .attr("flood-color", "#2a2f3a")
  .attr("flood-opacity", 0.25);

const tooltip = d3
  .select("body")
  .append("div")
  .attr("class", "tooltip");

const chart = svg
  .append("g")
  .attr("transform", `translate(${margin.left},${margin.top})`);

const gYearStamp = chart.append("g");
const grid = chart.append("g").attr("class", "grid");
const gXAxis = chart.append("g").attr("transform", `translate(0,${innerHeight})`);
const gYAxis = chart.append("g");
const gPoints = chart.append("g");
const gMedian = chart.append("g");
const gLabelLines = chart.append("g");
const gLabels = chart.append("g");

const gLegend = svg
  .append("g")
  .attr("transform", `translate(${width - margin.right + 60},${margin.top + 10})`);

const yearLabel = document.getElementById("year-label");
const yearSlider = document.getElementById("year-slider");
const yearRange = document.getElementById("year-range");
const toggleButton = document.getElementById("toggle");

const formatComma = d3.format(",");
const formatDoi = d3.format(",.0f");
let yearIndex = 0;

const floorSigfig = (value) => {
  if (!Number.isFinite(value) || value <= 0) return null;
  const power = Math.pow(10, Math.floor(Math.log10(value)));
  return Math.floor(value / power) * power;
};

const buildSlopeSvg = (leftValue, rightValue, color, leftLabel, rightLabel) => {
  if (!Number.isFinite(leftValue) || !Number.isFinite(rightValue)) return "";
  const minVal = Math.min(leftValue, rightValue);
  const maxVal = Math.max(leftValue, rightValue);
  const span = maxVal - minVal;
  const pad = span === 0 ? (minVal === 0 ? 1 : Math.abs(minVal) * 0.1) : span * 0.2;
  const height = 44;
  const width = 140;
  const x1 = 12;
  const x2 = width - 12;
  const topPad = 6;
  const bottomPad = 14;
  const yScale = (value) => {
    const domainMin = minVal - pad;
    const domainMax = maxVal + pad;
    const t = (value - domainMin) / (domainMax - domainMin);
    return height - bottomPad - t * (height - topPad - bottomPad);
  };
  const y1 = yScale(leftValue);
  const y2 = yScale(rightValue);
  const lineColor = color || "var(--fg)";
  return [
    `<svg class="slope-graph" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}" aria-hidden="true" focusable="false">`,
    `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${lineColor}" stroke-width="1.5" />`,
    `<circle cx="${x1}" cy="${y1}" r="3" fill="${lineColor}" />`,
    `<circle cx="${x2}" cy="${y2}" r="3" fill="${lineColor}" />`,
    `<text x="${x1}" y="${height - 2}" text-anchor="start" fill="var(--fg-soft)" font-size="9">${leftLabel}</text>`,
    `<text x="${x2}" y="${height - 2}" text-anchor="end" fill="var(--fg-soft)" font-size="9">${rightLabel}</text>`,
    "</svg>",
  ].join("");
};

d3.csv("../../week_20/data/crossref_quadrant_by_year.csv", d3.autoType).then((raw) => {
  const data = raw.filter((d) => d.year >= 2018);
  const years = Array.from(new Set(data.map((d) => d.year))).sort(d3.ascending);
  const minYear = years[0];
  const maxYear = years[years.length - 1];
  const slopeYearLabel = `${minYear} -> ${maxYear}`;
  const doisLookup = new Map(
    Array.from(d3.group(data, (d) => d.iso3_code), ([iso, values]) => [
      iso,
      new Map(values.map((v) => [v.year, v.n_dois])),
    ])
  );
  const membersLookup = new Map(
    Array.from(d3.group(data, (d) => d.iso3_code), ([iso, values]) => [
      iso,
      new Map(values.map((v) => [v.year, v.total_members])),
    ])
  );

  const regionActive = new Map(Array.from(colors.keys(), (key) => [key, true]));

  yearSlider.min = 0;
  yearSlider.max = years.length - 1;
  yearSlider.value = 0;
  yearRange.textContent = `${years[0]} - ${years[years.length - 1]}`;
  const yearTicks = document.getElementById("year-ticks");
  yearTicks.innerHTML = years
    .map((year, index) => `<option value="${index}" label="${year}"></option>`)
    .join("");
  const yearTickLabels = document.getElementById("year-tick-labels");
  yearTickLabels.innerHTML = years
    .map((year) => `<span>${year}</span>`)
    .join("");

  const xExtent = d3.extent(data, (d) => d.total_members);
  const yExtent = d3.extent(data, (d) => d.dois_per_member);
  const sizeExtent = d3.extent(data, (d) => d.dois_per_member);

  const xScale = d3
    .scaleLog()
    .domain([Math.max(1, xExtent[0] * 0.9), xExtent[1] * 1.05])
    .range([0, innerWidth]);
  const xDomainBase = xScale.domain();

  const yScale = d3
    .scaleLog()
    .domain([Math.max(0.1, yExtent[0] * 0.9), yExtent[1] * 1.05])
    .range([innerHeight, 0]);

  const sizeScale = d3
    .scaleSqrt()
    .domain([0, sizeExtent[1]])
    .range([3, 28])
    .clamp(true);

  const quantiles = [0.2, 0.5, 0.8, 0.95]
    .map((p) => d3.quantile(data.map((d) => d.dois_per_member).sort(d3.ascending), p))
    .map(floorSigfig)
    .filter((d) => d !== null);
  const sizeBreaks = Array.from(new Set(quantiles)).sort(d3.ascending);

  const medians = d3.rollup(
    data,
    (values) => ({
      x_median: d3.median(values, (d) => d.total_members),
      y_median: d3.median(values, (d) => d.dois_per_member),
    }),
    (d) => d.year
  );

  const importantLimit = 15;
  const alwaysLabelCountries = new Set([
    "British Virgin Islands",
    "Luxembourg",
    "Nicaragua",
    "Jamaica",
    "Cambodia",
    "Somalia",
    "Pakistan",
  ]);
  const importantCountries = Array.from(
    d3.rollup(
      data,
      (values) => d3.sum(values, (d) => d.n_dois),
      (d) => d.iso3_code
    )
  )
    .sort((a, b) => d3.descending(a[1], b[1]))
    .slice(0, importantLimit)
    .map((d) => d[0]);
  const importantSet = new Set(importantCountries);

  const xAxis = d3.axisBottom(xScale).ticks(6, "~s");
  const yAxis = d3.axisLeft(yScale).ticks(6, "~s");

  gXAxis.call(xAxis).selectAll("text").attr("fill", "var(--fg)");
  gYAxis.call(yAxis).selectAll("text").attr("fill", "var(--fg)");

  gXAxis.selectAll("path").attr("stroke", "var(--gray)");
  gYAxis.selectAll("path").attr("stroke", "var(--gray)");
  gXAxis.selectAll("line").remove();
  gYAxis.selectAll("line").remove();

  grid
    .call(
      d3
        .axisLeft(yScale)
        .ticks(6)
        .tickSize(-innerWidth)
        .tickFormat("")
    )
    .selectAll("line")
    .attr("stroke", "var(--bg-soft)");
  grid.selectAll("path").remove();
  grid.selectAll("line").remove();

  chart
    .append("text")
    .attr("x", innerWidth / 2)
    .attr("y", innerHeight + 50)
    .attr("text-anchor", "middle")
    .attr("fill", "var(--fg-soft)")
    .text("Total members (log scale)");

  chart
    .append("text")
    .attr("transform", `translate(-55, ${innerHeight / 2}) rotate(-90)`)
    .attr("text-anchor", "middle")
    .attr("fill", "var(--fg-soft)")
    .text("DOIs per member (log scale)");

  const xMedianLine = gMedian
    .append("line")
    .attr("y1", 0)
    .attr("y2", innerHeight)
    .attr("stroke", "var(--gray)")
    .attr("stroke-dasharray", "4 4")
    .attr("stroke-width", 1);

  const yMedianLine = gMedian
    .append("line")
    .attr("x1", 0)
    .attr("x2", innerWidth)
    .attr("stroke", "var(--gray)")
    .attr("stroke-dasharray", "4 4")
    .attr("stroke-width", 1);

  const yearStamp = gYearStamp
    .append("text")
    .attr("x", innerWidth)
    .attr("y", innerHeight - 10)
    .attr("text-anchor", "end")
    .attr("fill", "var(--gray)")
    .attr("font-family", "SpaceGrotesk, sans-serif")
    .attr("font-size", 120)
    .attr("font-weight", 700)
    .attr("opacity", 0.35)
    .attr("pointer-events", "none");

  const labelCandidates = (yearData, activeRegions) => {
    const filtered = yearData.filter((d) => (activeRegions ? activeRegions.get(d.region) !== false : true));
    const mustLabels = filtered.filter((d) => alwaysLabelCountries.has(d.country));
    const ranked = filtered
      .filter((d) => importantSet.has(d.iso3_code))
      .sort((a, b) => d3.descending(a.n_dois, b.n_dois))
      .slice(0, importantLimit);
    const merged = new Map(
      [...ranked, ...mustLabels].map((d) => [d.iso3_code, d])
    );
    return Array.from(merged.values());
  };

  const layoutLabels = (labels) => {
    const nodes = labels.map((d) => ({
      ...d,
      x: xScale(d.total_members),
      y: yScale(d.dois_per_member),
    }));
    const sim = d3
      .forceSimulation(nodes)
      .force("x", d3.forceX((d) => xScale(d.total_members)).strength(0.6))
      .force("y", d3.forceY((d) => yScale(d.dois_per_member)).strength(0.6))
      .force("collide", d3.forceCollide(12))
      .stop();
    for (let i = 0; i < 120; i += 1) sim.tick();
    return nodes;
  };

  const renderLegend = () => {
    const sizeLegend = gLegend.append("g");
    sizeLegend
      .append("text")
      .attr("x", 0)
      .attr("y", 0)
      .attr("fill", "var(--fg-soft)")
      .attr("font-size", 16)
      .text("DOIs per member");

    const sizeItems = sizeLegend
      .selectAll("g")
      .data(sizeBreaks)
      .join("g")
      .attr("class", "size-item")
      .attr("transform", (d, i) => `translate(0, ${20 + i * 26})`);

    sizeItems
      .append("circle")
      .attr("cx", 10)
      .attr("cy", 0)
      .attr("r", (d) => sizeScale(d))
      .attr("fill", `url(#${defaultGradientId})`)
      .attr("fill-opacity", 0.9);

    sizeItems
      .append("text")
      .attr("x", 30)
      .attr("y", 4)
      .attr("fill", "var(--fg-soft)")
      .attr("font-size", 13)
      .text((d) => formatComma(d));

    const colorLegend = gLegend.append("g").attr("transform", `translate(0, ${20 + sizeBreaks.length * 26 + 30})`);
    colorLegend
      .append("text")
      .attr("x", 0)
      .attr("y", 0)
      .attr("fill", "var(--fg-soft)")
      .attr("font-size", 16)
      .text("Region");

    const regionEntries = Array.from(colors.keys());
    const colorItems = colorLegend
      .selectAll("g")
      .data(regionEntries)
      .join("g")
      .attr("class", "legend-item")
      .attr("transform", (d, i) => `translate(0, ${20 + i * 22})`)
      .on("click", (event, region) => {
        regionActive.set(region, !regionActive.get(region));
        update(years[yearIndex]);
        d3.select(event.currentTarget).classed("is-muted", !regionActive.get(region));
      })
      .classed("is-muted", (d) => !regionActive.get(d));

    colorItems
      .append("circle")
      .attr("cx", 10)
      .attr("cy", 0)
      .attr("r", 6)
      .attr("fill", (d) => {
        const gradient = gradientIds.get(d) || defaultGradientId;
        return `url(#${gradient})`;
      });

    colorItems
      .append("text")
      .attr("x", 24)
      .attr("y", 4)
      .attr("fill", "var(--fg-soft)")
      .attr("font-size", 13)
      .text((d) => d);
  };

  renderLegend();

  const update = (year) => {
    yearLabel.textContent = year;
    yearStamp.text(year);
    const yearData = data.filter((d) => d.year === year);
    const median = medians.get(year);

    xScale.domain(xDomainBase);

    gXAxis.call(xAxis).selectAll("text").attr("fill", "var(--fg)");
    gXAxis.selectAll("path").attr("stroke", "var(--gray)");
    gXAxis.selectAll("line").remove();

    xMedianLine.attr("x1", xScale(median.x_median)).attr("x2", xScale(median.x_median));
    yMedianLine.attr("y1", yScale(median.y_median)).attr("y2", yScale(median.y_median));

    const points = gPoints
      .selectAll("circle")
      .data(yearData, (d) => d.iso3_code);

    points
      .join(
        (enter) =>
          enter
            .append("circle")
            .attr("cx", (d) => xScale(d.total_members))
            .attr("cy", (d) => yScale(d.dois_per_member))
            .attr("r", 0)
            .attr("fill", (d) => {
              const isActive = regionActive.get(d.region) ?? true;
              if (!isActive) return `url(#${mutedGradientId})`;
              const gradient = gradientIds.get(d.region) || defaultGradientId;
              return `url(#${gradient})`;
            })
            .attr("filter", "url(#point-shadow)")
            .attr("fill-opacity", 0.7),
        (updateSel) => updateSel,
        (exit) => exit.transition().duration(400).attr("r", 0).remove()
      )
      .on("mousemove", (event, d) => {
        const slopeColor = colors.get(d.region) || "var(--fg)";
        const doisSeries = doisLookup.get(d.iso3_code);
        const membersSeries = membersLookup.get(d.iso3_code);
        const doisStart = doisSeries ? doisSeries.get(minYear) : null;
        const doisEnd = doisSeries ? doisSeries.get(maxYear) : null;
        const membersStart = membersSeries ? membersSeries.get(minYear) : null;
        const membersEnd = membersSeries ? membersSeries.get(maxYear) : null;
        const slopeDois = buildSlopeSvg(doisStart, doisEnd, slopeColor, minYear, maxYear);
        const slopeMembers = buildSlopeSvg(membersStart, membersEnd, slopeColor, minYear, maxYear);
        const title = d.country;
        tooltip
          .style("opacity", 1)
          .style("left", `${event.pageX + 12}px`)
          .style("top", `${event.pageY - 16}px`)
          .html(
            `<div class="tooltip-title">${title}</div>` +
              `<div class="tooltip-meta">Year: ${d.year}</div>` +
              `<div class="tooltip-row"><span>Total members</span><span class="tooltip-value">${formatComma(d.total_members)}</span></div>` +
              `<div class="tooltip-row"><span>DOIs per member</span><span class="tooltip-value">${formatDoi(d.dois_per_member)}</span></div>` +
              `<div class="tooltip-section">` +
              `${slopeDois ? `${slopeDois}<span class="slope-label">DOIs trend (${slopeYearLabel})</span>` : ""}` +
              `${slopeMembers ? `${slopeMembers}<span class="slope-label">Members trend (${slopeYearLabel})</span>` : ""}` +
              `</div>`
          );
      })
      .on("mouseleave", () => {
        tooltip.style("opacity", 0);
      })
      .transition()
      .duration(700)
      .attr("cx", (d) => xScale(d.total_members))
      .attr("cy", (d) => yScale(d.dois_per_member))
      .attr("r", (d) => sizeScale(d.dois_per_member));

    points
      .attr("fill", (d) => {
        const isActive = regionActive.get(d.region) ?? true;
        if (!isActive) return `url(#${mutedGradientId})`;
        const gradient = gradientIds.get(d.region) || defaultGradientId;
        return `url(#${gradient})`;
      })
      .attr("filter", "url(#point-shadow)");

    gLegend
      .selectAll(".size-item circle")
      .attr("r", (d) => sizeScale(d));

    const labels = layoutLabels(labelCandidates(yearData, regionActive));

    const lines = gLabelLines.selectAll("line").data(labels, (d) => d.iso3_code);
    lines
      .join(
        (enter) =>
          enter
            .append("line")
            .attr("x1", (d) => xScale(d.total_members))
            .attr("y1", (d) => yScale(d.dois_per_member))
            .attr("x2", (d) => xScale(d.total_members))
            .attr("y2", (d) => yScale(d.dois_per_member))
            .attr("stroke", "var(--gray)")
            .attr("stroke-width", 1)
            .attr("stroke-opacity", 0.6),
        (updateSel) => updateSel,
        (exit) => exit.remove()
      )
      .transition()
      .duration(700)
      .attr("x1", (d) => xScale(d.total_members))
      .attr("y1", (d) => yScale(d.dois_per_member))
      .attr("x2", (d) => d.x)
      .attr("y2", (d) => d.y);

    const texts = gLabels.selectAll("text").data(labels, (d) => d.iso3_code);
    texts
      .join(
        (enter) =>
          enter
            .append("text")
            .attr("x", (d) => xScale(d.total_members))
            .attr("y", (d) => yScale(d.dois_per_member))
            .attr("font-size", 11)
            .attr("fill", (d) => colors.get(d.region) || "var(--gray)")
            .text((d) => d.country),
        (updateSel) => updateSel,
        (exit) => exit.remove()
      )
      .transition()
      .duration(700)
      .attr("x", (d) => d.x + 4)
      .attr("y", (d) => d.y - 4)
      .attr("fill", (d) => colors.get(d.region) || "var(--gray)");
  };

  update(years[yearIndex]);

  let running = true;
  const timer = d3.interval(() => {
    if (!running) return;
    yearIndex += 1;
    if (yearIndex >= years.length) {
      yearIndex = years.length - 1;
      yearSlider.value = yearIndex;
      update(years[yearIndex]);
      running = false;
      toggleButton.textContent = "Play";
      return;
    }
    yearSlider.value = yearIndex;
    update(years[yearIndex]);
  }, 1200);

  const togglePlayback = () => {
    if (running) {
      running = false;
      toggleButton.textContent = "Play";
      return;
    }
    if (yearIndex >= years.length - 1) {
      yearIndex = 0;
      yearSlider.value = yearIndex;
      update(years[yearIndex]);
    }
    running = true;
    toggleButton.textContent = "Pause";
  };

  toggleButton.addEventListener("click", togglePlayback);

  window.addEventListener("keydown", (event) => {
    if (event.code !== "Space") return;
    if (event.target && (event.target.tagName === "INPUT" || event.target.tagName === "TEXTAREA")) return;
    event.preventDefault();
    togglePlayback();
  });

  yearSlider.addEventListener("input", (event) => {
    const idx = Number(event.target.value);
    yearIndex = idx;
    update(years[yearIndex]);
  });
});
