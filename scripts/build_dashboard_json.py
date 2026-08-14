#!/usr/bin/env python3
"""Build the public, aggregate-only BirdWeather dashboard dataset.

The source CSV contains sensitive detection-level fields. This script deliberately
maps the input to a small allowlist of aggregate fields and validates the resulting
payload before it is written to the public ``docs`` directory.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import os
import re
import tempfile
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Iterable
from urllib.parse import urlparse
from zoneinfo import ZoneInfo


DEFAULT_TIMEZONE = "America/Costa_Rica"
DEFAULT_INPUT = Path("data/master_detections.csv")
DEFAULT_OUTPUT = Path("docs/data/dashboard.json")
TOP_SPECIES_LIMIT = 12
NEW_SPECIES_LIMIT = 12
REVIEW_CANDIDATE_LIMIT = 8
EXPECTED_ARRIVAL_LIMIT = 8
SEASONAL_WEEK_COUNT = 48
OUTLOOK_HORIZON_WEEKS = 12
MAX_CURRENT_PROBABILITY = 0.15
MIN_FUTURE_PROBABILITY = 0.08
MIN_ABSOLUTE_RISE = 0.08
MIN_RISE_RATIO = 2.0

# Locations are separated by date only. Coordinates from the sensitive source
# are deliberately never needed for, or copied into, the public dashboard.
LOCATION_RANGES = (
    {
        "key": "guapiles",
        "label": "Guápiles, Limón",
        "start_date": date(2026, 8, 11),
        "end_date": None,
    },
    {
        "key": "santo_domingo",
        "label": "Santo Domingo, Heredia",
        "start_date": date(2026, 5, 28),
        "end_date": date(2026, 7, 31),
    },
    {
        "key": "wageningen",
        "label": "Wageningen, Netherlands",
        "start_date": None,
        "end_date": date(2026, 5, 11),
    },
)

FORBIDDEN_KEYS = {
    "id",
    "detection_id",
    "station_id",
    "timestamp",
    "latitude",
    "longitude",
    "lat",
    "lon",
    "coordinates",
    "audio",
    "audio_url",
    "soundscape",
    "soundscape_url",
}
FORBIDDEN_KEY_SUFFIXES = ("_id",)
EXACT_TIME_PATTERN = re.compile(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}")
AUDIO_PATTERN = re.compile(r"(?:/soundscapes/|\.(?:flac|wav|mp3|m4a)(?:\?|$))", re.I)


@dataclass
class SpeciesBucket:
    detections: int = 0
    confidence_sum: float = 0.0
    confidence_count: int = 0
    hourly: list[int] = field(default_factory=lambda: [0] * 24)
    photo_urls: list[str] = field(default_factory=list)


@dataclass
class DayBucket:
    detections: int = 0
    confidence_sum: float = 0.0
    confidence_count: int = 0
    hourly: list[int] = field(default_factory=lambda: [0] * 24)
    species: dict[tuple[str, str], SpeciesBucket] = field(default_factory=dict)


@dataclass
class SpeciesAggregate(SpeciesBucket):
    active_days: int = 0


def parse_timestamp(value: str, local_timezone: ZoneInfo) -> datetime | None:
    """Parse an ISO-8601 timestamp and return it in the requested timezone."""

    value = (value or "").strip()
    if not value:
        return None
    if value.endswith("Z"):
        value = f"{value[:-1]}+00:00"
    try:
        parsed = datetime.fromisoformat(value)
    except ValueError:
        return None
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(local_timezone)


def parse_confidence(value: str) -> float | None:
    try:
        confidence = float(value)
    except (TypeError, ValueError):
        return None
    if not math.isfinite(confidence) or not 0 <= confidence <= 1:
        return None
    return confidence


def birdweather_photos(row: dict[str, str]) -> list[str]:
    """Return only BirdWeather-hosted species images, never soundscape URLs."""

    photos: list[str] = []
    for column in ("species.imageUrl", "species.pngUrl", "species.thumbnailUrl"):
        candidate = (row.get(column) or "").strip()
        if not candidate:
            continue
        parsed = urlparse(candidate)
        if (
            parsed.scheme == "https"
            and parsed.netloc == "media.birdweather.com"
            and parsed.path.startswith("/species/")
        ):
            photos.append(candidate)
    return list(dict.fromkeys(photos))


def row_identity(row: dict[str, str]) -> tuple[str, ...] | None:
    """Return a transient identity used only to avoid double counting inputs."""

    for column in ("id", "detectionId", "detection_id"):
        value = (row.get(column) or "").strip()
        if value:
            return (column, value)

    timestamp = (row.get("timestamp") or "").strip()
    common_name = (row.get("species.commonName") or "").strip()
    if timestamp and common_name:
        return (
            "fallback",
            timestamp,
            common_name,
            (row.get("confidence") or "").strip(),
        )
    return None


def read_aggregates(
    input_paths: Iterable[Path], local_timezone: ZoneInfo
) -> tuple[dict[date, DayBucket], int]:
    """Read sensitive rows and retain aggregate counters only.

    Multiple inputs allow the near-live workflow to combine the committed history
    with a temporary current-day download. Detection identities are retained only
    in memory long enough to de-duplicate overlapping inputs and never enter the
    public payload.
    """

    days: dict[date, DayBucket] = {}
    invalid_rows = 0
    seen_rows: set[tuple[str, ...]] = set()

    for input_path in input_paths:
        with input_path.open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.DictReader(handle)
            required = {"timestamp", "species.commonName"}
            missing = required.difference(reader.fieldnames or [])
            if missing:
                raise ValueError(
                    f"Missing required CSV columns in {input_path}: "
                    f"{', '.join(sorted(missing))}"
                )

            for row in reader:
                identity = row_identity(row)
                if identity is not None and identity in seen_rows:
                    continue
                if identity is not None:
                    seen_rows.add(identity)

                observed_at = parse_timestamp(row.get("timestamp", ""), local_timezone)
                common_name = (row.get("species.commonName") or "").strip()
                scientific_name = (row.get("species.scientificName") or "").strip()
                if observed_at is None or not common_name:
                    invalid_rows += 1
                    continue

                confidence = parse_confidence(row.get("confidence", ""))
                observed_date = observed_at.date()
                hour = observed_at.hour
                day_bucket = days.setdefault(observed_date, DayBucket())
                species_key = (common_name, scientific_name)
                species_bucket = day_bucket.species.setdefault(
                    species_key, SpeciesBucket()
                )

                day_bucket.detections += 1
                day_bucket.hourly[hour] += 1
                species_bucket.detections += 1
                species_bucket.hourly[hour] += 1

                if confidence is not None:
                    day_bucket.confidence_sum += confidence
                    day_bucket.confidence_count += 1
                    species_bucket.confidence_sum += confidence
                    species_bucket.confidence_count += 1

                if not species_bucket.photo_urls:
                    species_bucket.photo_urls = birdweather_photos(row)

    return days, invalid_rows


def mean_confidence(total: float, count: int) -> float | None:
    return round(total / count, 3) if count else None


def hourly_rows(
    counts: Iterable[int], species_counts: Iterable[int] | None = None
) -> list[dict[str, int]]:
    detections = list(counts)
    diversity = list(species_counts) if species_counts is not None else None
    return [
        {
            "hour": hour,
            "detections": detection_count,
            **({"species_count": diversity[hour]} if diversity is not None else {}),
        }
        for hour, detection_count in enumerate(detections)
    ]


def period_dates(period_key: str, first_date: date, last_date: date) -> list[date]:
    if period_key == "7d":
        start_date = max(first_date, last_date - timedelta(days=6))
    elif period_key == "30d":
        start_date = max(first_date, last_date - timedelta(days=29))
    elif period_key == "all":
        start_date = first_date
    else:
        raise ValueError(f"Unknown period: {period_key}")

    return [
        start_date + timedelta(days=offset)
        for offset in range((last_date - start_date).days + 1)
    ]


def build_period(
    period_key: str,
    label: str,
    days: dict[date, DayBucket],
    first_date: date,
    last_date: date,
    first_seen: dict[tuple[str, str], date],
) -> dict[str, Any]:
    selected_dates = period_dates(period_key, first_date, last_date)
    period_hourly = [0] * 24
    total_detections = 0
    confidence_sum = 0.0
    confidence_count = 0
    active_days = 0
    daily_activity: list[dict[str, Any]] = []
    species_totals: dict[tuple[str, str], SpeciesAggregate] = defaultdict(SpeciesAggregate)

    for observed_date in selected_dates:
        day_bucket = days.get(observed_date)
        if day_bucket is None:
            daily_activity.append(
                {"date": observed_date.isoformat(), "detections": 0, "species_count": 0}
            )
            continue

        active_days += 1
        total_detections += day_bucket.detections
        confidence_sum += day_bucket.confidence_sum
        confidence_count += day_bucket.confidence_count
        period_hourly = [a + b for a, b in zip(period_hourly, day_bucket.hourly)]
        daily_activity.append(
            {
                "date": observed_date.isoformat(),
                "detections": day_bucket.detections,
                "species_count": len(day_bucket.species),
            }
        )

        for species_key, bucket in day_bucket.species.items():
            aggregate = species_totals[species_key]
            aggregate.detections += bucket.detections
            aggregate.confidence_sum += bucket.confidence_sum
            aggregate.confidence_count += bucket.confidence_count
            aggregate.hourly = [a + b for a, b in zip(aggregate.hourly, bucket.hourly)]
            aggregate.active_days += 1
            if not aggregate.photo_urls and bucket.photo_urls:
                aggregate.photo_urls = bucket.photo_urls

    previous_species: dict[tuple[str, str], int] = defaultdict(int)
    previous_total = 0
    if period_key != "all":
        previous_end = selected_dates[0] - timedelta(days=1)
        previous_start = previous_end - timedelta(days=len(selected_dates) - 1)
        for observed_date in (
            previous_start + timedelta(days=offset)
            for offset in range(len(selected_dates))
        ):
            day_bucket = days.get(observed_date)
            if day_bucket is None:
                continue
            previous_total += day_bucket.detections
            for species_key, bucket in day_bucket.species.items():
                previous_species[species_key] += bucket.detections
    else:
        previous_start = None
        previous_end = None

    ranked_species = sorted(
        species_totals.items(),
        key=lambda item: (-item[1].detections, item[0][0].casefold()),
    )

    species_summary = [
        {
            "common_name": common_name,
            "scientific_name": scientific_name or None,
            "detections": aggregate.detections,
            "active_days": aggregate.active_days,
            "average_confidence": mean_confidence(
                aggregate.confidence_sum, aggregate.confidence_count
            ),
            "first_seen_date": first_seen[(common_name, scientific_name)].isoformat(),
        }
        for (common_name, scientific_name), aggregate in ranked_species
    ]

    top_species: list[dict[str, Any]] = []
    for (common_name, scientific_name), aggregate in ranked_species[:TOP_SPECIES_LIMIT]:
        previous_detections = previous_species.get((common_name, scientific_name), 0)
        top_species.append(
            {
                "common_name": common_name,
                "scientific_name": scientific_name or None,
                "detections": aggregate.detections,
                "active_days": aggregate.active_days,
                "average_confidence": mean_confidence(
                    aggregate.confidence_sum, aggregate.confidence_count
                ),
                "photo_url": aggregate.photo_urls[0] if aggregate.photo_urls else None,
                "photo_urls": aggregate.photo_urls,
                "hourly_activity": hourly_rows(aggregate.hourly),
                "change_percent": (
                    round(
                        (aggregate.detections - previous_detections)
                        / previous_detections
                        * 100,
                        1,
                    )
                    if previous_detections
                    else None
                ),
            }
        )

    new_species_ranked = [
        (species_key, aggregate)
        for species_key, aggregate in ranked_species
        if selected_dates[0] <= first_seen[species_key] <= selected_dates[-1]
    ]
    new_species = [
        {
            "common_name": common_name,
            "scientific_name": scientific_name or None,
            "detections": aggregate.detections,
            "first_seen_date": first_seen[(common_name, scientific_name)].isoformat(),
            "average_confidence": mean_confidence(
                aggregate.confidence_sum, aggregate.confidence_count
            ),
            "photo_url": aggregate.photo_urls[0] if aggregate.photo_urls else None,
            "photo_urls": aggregate.photo_urls,
        }
        for (common_name, scientific_name), aggregate in new_species_ranked[
            :NEW_SPECIES_LIMIT
        ]
    ]

    review_ranked = sorted(
        (
            (species_key, aggregate)
            for species_key, aggregate in ranked_species
            if aggregate.confidence_count
            and (
                mean_confidence(aggregate.confidence_sum, aggregate.confidence_count)
                < 0.75
                or aggregate.detections <= 3
            )
        ),
        key=lambda item: (
            mean_confidence(item[1].confidence_sum, item[1].confidence_count),
            item[1].detections,
            item[0][0].casefold(),
        ),
    )
    review_candidates = [
        {
            "common_name": common_name,
            "scientific_name": scientific_name or None,
            "detections": aggregate.detections,
            "average_confidence": mean_confidence(
                aggregate.confidence_sum, aggregate.confidence_count
            ),
            "reason": (
                "Lower aggregate confidence"
                if mean_confidence(
                    aggregate.confidence_sum, aggregate.confidence_count
                )
                < 0.75
                else "Rare in this period"
            ),
        }
        for (common_name, scientific_name), aggregate in review_ranked[
            :REVIEW_CANDIDATE_LIMIT
        ]
    ]

    period_species_hourly = [
        sum(1 for aggregate in species_totals.values() if aggregate.hourly[hour])
        for hour in range(24)
    ]
    peak_hour = max(range(24), key=lambda hour: period_hourly[hour])
    detection_change = (
        round((total_detections - previous_total) / previous_total * 100, 1)
        if previous_total
        else None
    )

    return {
        "label": label,
        "start_date": selected_dates[0].isoformat(),
        "end_date": selected_dates[-1].isoformat(),
        "total_detections": total_detections,
        "species_count": len(species_totals),
        "active_days": active_days,
        "average_confidence": mean_confidence(confidence_sum, confidence_count),
        "daily_activity": daily_activity,
        "hourly_activity": hourly_rows(period_hourly, period_species_hourly),
        "peak_hour": {"hour": peak_hour, "detections": period_hourly[peak_hour]},
        "comparison": {
            "previous_start_date": previous_start.isoformat()
            if previous_start
            else None,
            "previous_end_date": previous_end.isoformat() if previous_end else None,
            "previous_detections": previous_total if previous_start else None,
            "detection_change_percent": detection_change,
            "previous_species_count": len(previous_species) if previous_start else None,
            "species_change": (
                len(species_totals) - len(previous_species) if previous_start else None
            ),
        },
        "new_species_count": len(new_species_ranked),
        "new_species": new_species,
        "review_candidates": review_candidates,
        "top_species": top_species,
        "species": species_summary,
    }


def validate_public_payload(payload: Any) -> None:
    """Reject fields or values that could expose detection-level private data."""

    def walk(value: Any, path: str = "root") -> None:
        if isinstance(value, dict):
            for key, child in value.items():
                normalised = key.casefold()
                if normalised in FORBIDDEN_KEYS or normalised.endswith(FORBIDDEN_KEY_SUFFIXES):
                    raise ValueError(f"Forbidden public JSON key at {path}.{key}")
                walk(child, f"{path}.{key}")
        elif isinstance(value, list):
            for index, child in enumerate(value):
                walk(child, f"{path}[{index}]")
        elif isinstance(value, str):
            if EXACT_TIME_PATTERN.search(value):
                raise ValueError(f"Exact time found in public JSON at {path}")
            if AUDIO_PATTERN.search(value):
                raise ValueError(f"Audio link found in public JSON at {path}")

    walk(payload)


def seasonal_week_index(value: date) -> int:
    """Map a date to BirdNET's 48-week calendar (four weeks per month)."""

    week_in_month = min((value.day - 1) // 7, 3)
    return (value.month - 1) * 4 + week_in_month


def read_probability_input(input_path: Path | None) -> list[dict[str, Any]]:
    """Read the privacy-safe intermediate seasonal probability response."""

    if input_path is None:
        return []
    with input_path.open("r", encoding="utf-8") as handle:
        payload = json.load(handle)
    rows = payload.get("species") if isinstance(payload, dict) else None
    if not isinstance(rows, list):
        raise ValueError("Seasonal probability input has no species list")
    return [row for row in rows if isinstance(row, dict)]


def build_expected_arrivals(
    probability_rows: Iterable[dict[str, Any]],
    reference_date: date,
) -> list[dict[str, Any]]:
    """Rank species that are unlikely now but rise strongly within 12 weeks."""

    current_week = seasonal_week_index(reference_date)
    candidates: list[tuple[float, float, int, str, dict[str, Any]]] = []
    for row in probability_rows:
        common_name = str(row.get("common_name") or "").strip()
        scientific_name = str(row.get("scientific_name") or "").strip()
        weeks = row.get("weeks")
        if not common_name or not isinstance(weeks, list) or len(weeks) != SEASONAL_WEEK_COUNT:
            continue
        try:
            values = [float(value) for value in weeks]
        except (TypeError, ValueError):
            continue
        if any(not math.isfinite(value) or not 0 <= value <= 1 for value in values):
            continue

        current_probability = values[current_week]
        future = [
            values[(current_week + offset) % SEASONAL_WEEK_COUNT]
            for offset in range(1, OUTLOOK_HORIZON_WEEKS + 1)
        ]
        projected_probability = max(future)
        peak_in_weeks = future.index(projected_probability) + 1
        absolute_rise = projected_probability - current_probability
        rises_enough = (
            current_probability <= 0.01
            or projected_probability / current_probability >= MIN_RISE_RATIO
        )
        if not (
            current_probability <= MAX_CURRENT_PROBABILITY
            and projected_probability >= MIN_FUTURE_PROBABILITY
            and absolute_rise >= MIN_ABSOLUTE_RISE
            and rises_enough
        ):
            continue

        photo_urls = [
            photo
            for photo in row.get("photo_urls") or []
            if isinstance(photo, str)
            and (parsed := urlparse(photo)).scheme == "https"
            and parsed.netloc == "media.birdweather.com"
            and parsed.path.startswith("/species/")
        ]
        trend = [
            {
                "weeks_ahead": offset,
                "probability": round(
                    values[(current_week + offset) % SEASONAL_WEEK_COUNT], 4
                ),
            }
            for offset in range(OUTLOOK_HORIZON_WEEKS + 1)
        ]
        public_row = {
            "common_name": common_name,
            "scientific_name": scientific_name or None,
            "photo_url": photo_urls[0] if photo_urls else None,
            "photo_urls": list(dict.fromkeys(photo_urls)),
            "current_probability": round(current_probability, 4),
            "projected_probability": round(projected_probability, 4),
            "increase_percentage_points": round(absolute_rise * 100, 1),
            "peak_in_weeks": peak_in_weeks,
            "weekly_probability": trend,
        }
        candidates.append(
            (
                absolute_rise,
                projected_probability,
                -peak_in_weeks,
                common_name.casefold(),
                public_row,
            )
        )

    candidates.sort(key=lambda item: (-item[0], -item[1], -item[2], item[3]))
    return [item[-1] for item in candidates[:EXPECTED_ARRIVAL_LIMIT]]


def build_dashboard(
    input_path: Path,
    output_path: Path,
    timezone_name: str = DEFAULT_TIMEZONE,
    generated_date: date | None = None,
    additional_input_paths: Iterable[Path] | None = None,
    probabilities_input_path: Path | None = None,
) -> dict[str, Any]:
    local_timezone = ZoneInfo(timezone_name)
    input_paths = [input_path, *(additional_input_paths or [])]
    days, invalid_rows = read_aggregates(input_paths, local_timezone)
    if not days:
        raise ValueError("No valid detections found in the master CSV")

    first_date = min(days)
    last_date = max(days)
    first_seen: dict[tuple[str, str], date] = {}
    for observed_date in sorted(days):
        for species_key in days[observed_date].species:
            first_seen.setdefault(species_key, observed_date)
    generated_now = datetime.now(local_timezone)
    generated_date = generated_date or generated_now.date()
    probability_rows = read_probability_input(probabilities_input_path)
    expected_arrivals = build_expected_arrivals(probability_rows, generated_date)
    period_labels = {"7d": "Last 7 days", "30d": "Last 30 days", "all": "All data"}

    def build_periods(
        location_days: dict[date, DayBucket],
    ) -> dict[str, dict[str, Any]]:
        location_first = min(location_days)
        location_last = max(location_days)
        location_first_seen: dict[tuple[str, str], date] = {}
        for observed_date in sorted(location_days):
            for species_key in location_days[observed_date].species:
                location_first_seen.setdefault(species_key, observed_date)
        return {
            key: build_period(
                key,
                label,
                location_days,
                location_first,
                location_last,
                location_first_seen,
            )
            for key, label in period_labels.items()
        }

    all_periods = {
        key: build_period(key, label, days, first_date, last_date, first_seen)
        for key, label in period_labels.items()
    }
    locations: dict[str, dict[str, Any]] = {
        "all": {
            "label": "All locations",
            "first_observation_date": first_date.isoformat(),
            "latest_observation_date": last_date.isoformat(),
            "outlook_status": "select_current_location",
            "expected_arrivals": [],
            "periods": all_periods,
        }
    }
    for location in LOCATION_RANGES:
        location_days = {
            observed_date: bucket
            for observed_date, bucket in days.items()
            if (location["start_date"] is None or observed_date >= location["start_date"])
            and (location["end_date"] is None or observed_date <= location["end_date"])
        }
        if not location_days:
            continue
        locations[location["key"]] = {
            "label": location["label"],
            "first_observation_date": min(location_days).isoformat(),
            "latest_observation_date": max(location_days).isoformat(),
            "outlook_status": (
                "available"
                if location["key"] == "guapiles" and probability_rows
                else "historical_location"
                if location["end_date"] is not None
                else "unavailable"
            ),
            "expected_arrivals": (
                expected_arrivals if location["key"] == "guapiles" else []
            ),
            "periods": build_periods(location_days),
        }

    default_location = next(
        (
            location["key"]
            for location in LOCATION_RANGES
            if location["key"] in locations
        ),
        "all",
    )

    payload = {
        "schema_version": 2,
        "generated_date": generated_date.isoformat(),
        "generated_hour": generated_now.hour,
        "timezone": timezone_name,
        "latest_observation_date": last_date.isoformat(),
        "default_period": "7d",
        "default_location": default_location,
        "seasonal_outlook": {
            "source": "BirdWeather / BirdNET seasonal probability",
            "basis": "Historical eBird occurrence likelihood by week",
            "forecast_horizon_weeks": OUTLOOK_HORIZON_WEEKS,
            "current_seasonal_week": seasonal_week_index(generated_date) + 1,
        },
        "privacy": {
            "aggregation": "daily and hourly totals only",
            "excluded": [
                "coordinates",
                "audio links",
                "detection identifiers",
                "exact detection times",
            ],
        },
        "quality": {"rows_skipped": invalid_rows},
        "periods": all_periods,
        "locations": locations,
    }

    validate_public_payload(payload)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", dir=output_path.parent, delete=False
    ) as handle:
        json.dump(payload, handle, ensure_ascii=False, indent=2, allow_nan=False)
        handle.write("\n")
        temporary_path = Path(handle.name)
    os.replace(temporary_path, output_path)
    return payload


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--timezone", default=DEFAULT_TIMEZONE)
    parser.add_argument(
        "--additional-input",
        action="append",
        type=Path,
        default=[],
        help="Additional detection CSV to aggregate and de-duplicate in memory",
    )
    parser.add_argument(
        "--probabilities-input",
        type=Path,
        help="Intermediate BirdWeather 48-week probability JSON",
    )
    args = parser.parse_args()

    payload = build_dashboard(
        args.input,
        args.output,
        args.timezone,
        additional_input_paths=args.additional_input,
        probabilities_input_path=args.probabilities_input,
    )
    default = payload["periods"][payload["default_period"]]
    print(
        f"Wrote {args.output}: {default['total_detections']} detections, "
        f"{default['species_count']} species ({payload['timezone']})"
    )


if __name__ == "__main__":
    main()
