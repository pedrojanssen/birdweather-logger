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
TOP_SPECIES_LIMIT = 10

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
    photo_url: str | None = None


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


def birdweather_photo(row: dict[str, str]) -> str | None:
    """Return only a BirdWeather-hosted species image, never a soundscape URL."""

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
            return candidate
    return None


def read_aggregates(
    input_path: Path, local_timezone: ZoneInfo
) -> tuple[dict[date, DayBucket], int]:
    """Read sensitive rows and retain aggregate counters only."""

    days: dict[date, DayBucket] = {}
    invalid_rows = 0

    with input_path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        required = {"timestamp", "species.commonName"}
        missing = required.difference(reader.fieldnames or [])
        if missing:
            raise ValueError(f"Missing required CSV columns: {', '.join(sorted(missing))}")

        for row in reader:
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
            species_bucket = day_bucket.species.setdefault(species_key, SpeciesBucket())

            day_bucket.detections += 1
            day_bucket.hourly[hour] += 1
            species_bucket.detections += 1
            species_bucket.hourly[hour] += 1

            if confidence is not None:
                day_bucket.confidence_sum += confidence
                day_bucket.confidence_count += 1
                species_bucket.confidence_sum += confidence
                species_bucket.confidence_count += 1

            if species_bucket.photo_url is None:
                species_bucket.photo_url = birdweather_photo(row)

    return days, invalid_rows


def mean_confidence(total: float, count: int) -> float | None:
    return round(total / count, 3) if count else None


def hourly_rows(counts: Iterable[int]) -> list[dict[str, int]]:
    return [
        {"hour": hour, "detections": detections}
        for hour, detections in enumerate(counts)
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
            if aggregate.photo_url is None and bucket.photo_url:
                aggregate.photo_url = bucket.photo_url

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
        }
        for (common_name, scientific_name), aggregate in ranked_species
    ]

    top_species = []
    for (common_name, scientific_name), aggregate in ranked_species[:TOP_SPECIES_LIMIT]:
        top_species.append(
            {
                "common_name": common_name,
                "scientific_name": scientific_name or None,
                "detections": aggregate.detections,
                "active_days": aggregate.active_days,
                "average_confidence": mean_confidence(
                    aggregate.confidence_sum, aggregate.confidence_count
                ),
                "photo_url": aggregate.photo_url,
                "hourly_activity": hourly_rows(aggregate.hourly),
            }
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
        "hourly_activity": hourly_rows(period_hourly),
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


def build_dashboard(
    input_path: Path,
    output_path: Path,
    timezone_name: str = DEFAULT_TIMEZONE,
    generated_date: date | None = None,
) -> dict[str, Any]:
    local_timezone = ZoneInfo(timezone_name)
    days, invalid_rows = read_aggregates(input_path, local_timezone)
    if not days:
        raise ValueError("No valid detections found in the master CSV")

    first_date = min(days)
    last_date = max(days)
    generated_date = generated_date or datetime.now(local_timezone).date()
    period_labels = {"7d": "Last 7 days", "30d": "Last 30 days", "all": "All data"}

    payload = {
        "schema_version": 1,
        "generated_date": generated_date.isoformat(),
        "timezone": timezone_name,
        "latest_observation_date": last_date.isoformat(),
        "default_period": "7d",
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
        "periods": {
            key: build_period(key, label, days, first_date, last_date)
            for key, label in period_labels.items()
        },
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
    args = parser.parse_args()

    payload = build_dashboard(args.input, args.output, args.timezone)
    default = payload["periods"][payload["default_period"]]
    print(
        f"Wrote {args.output}: {default['total_detections']} detections, "
        f"{default['species_count']} species ({payload['timezone']})"
    )


if __name__ == "__main__":
    main()
