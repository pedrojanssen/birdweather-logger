#!/usr/bin/env python3
"""Download the current Costa Rica day for a near-live dashboard build.

The output is temporary workflow input. It may contain sensitive detection-level
fields and must never be committed or uploaded as a public artifact.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urlencode
from urllib.request import Request, urlopen
from zoneinfo import ZoneInfo


DEFAULT_TIMEZONE = "America/Costa_Rica"
API_ROOT = "https://app.birdweather.com/api/v1"
PAGE_LIMIT = 100
REQUIRED_COLUMNS = (
    "id",
    "timestamp",
    "confidence",
    "species.commonName",
    "species.scientificName",
    "species.imageUrl",
    "species.thumbnailUrl",
    "species.pngUrl",
)


def flatten(value: dict[str, Any], prefix: str = "") -> dict[str, Any]:
    """Flatten nested BirdWeather objects using the existing dotted CSV names."""

    flattened: dict[str, Any] = {}
    for key, child in value.items():
        name = f"{prefix}.{key}" if prefix else key
        if isinstance(child, dict):
            flattened.update(flatten(child, name))
        elif isinstance(child, list):
            flattened[name] = json.dumps(child, separators=(",", ":"))
        elif child is None:
            flattened[name] = ""
        else:
            flattened[name] = child
    return flattened


def request_page(url: str, attempts: int = 4) -> dict[str, Any]:
    """Request one API page without ever logging the station token URL."""

    for attempt in range(attempts):
        try:
            request = Request(
                url,
                headers={
                    "Accept": "application/json",
                    "User-Agent": "birdweather-logger-near-live/1.0",
                },
            )
            with urlopen(request, timeout=45) as response:
                return json.load(response)
        except HTTPError as error:
            if error.code in {400, 401, 403, 404} or attempt == attempts - 1:
                raise RuntimeError(
                    f"BirdWeather API request failed with HTTP {error.code}"
                ) from error
        except (URLError, TimeoutError, json.JSONDecodeError) as error:
            if attempt == attempts - 1:
                raise RuntimeError("BirdWeather API request failed after retries") from error
        time.sleep(2**attempt)
    raise RuntimeError("BirdWeather API request failed")


def fetch_current_day(
    station_token: str,
    timezone_name: str = DEFAULT_TIMEZONE,
) -> list[dict[str, Any]]:
    local_timezone = ZoneInfo(timezone_name)
    now = datetime.now(local_timezone)
    start = now.replace(hour=0, minute=0, second=0, microsecond=0)
    from_time = start.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")
    to_time = now.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")
    endpoint = f"{API_ROOT}/stations/{quote(station_token, safe='')}/detections"

    detections: list[dict[str, Any]] = []
    cursor: str | None = None
    seen_cursors: set[str] = set()
    while True:
        query = {
            "limit": PAGE_LIMIT,
            "from": from_time,
            "to": to_time,
            "order": "desc",
            "classification": "avian",
        }
        if cursor is not None:
            query["cursor"] = cursor
        payload = request_page(f"{endpoint}?{urlencode(query)}")
        page = payload.get("detections") or []
        if not isinstance(page, list):
            raise RuntimeError("BirdWeather API returned an unexpected detections shape")
        detections.extend(item for item in page if isinstance(item, dict))
        if len(page) < PAGE_LIMIT:
            break
        last_id = page[-1].get("id") if isinstance(page[-1], dict) else None
        if last_id is None:
            raise RuntimeError("BirdWeather API pagination response has no detection id")
        cursor = str(last_id)
        if cursor in seen_cursors:
            raise RuntimeError("BirdWeather API returned a repeated pagination cursor")
        seen_cursors.add(cursor)

    return detections


def write_csv(detections: list[dict[str, Any]], output_path: Path) -> None:
    rows = [flatten(detection) for detection in detections]
    extra_columns = sorted(
        {key for row in rows for key in row}.difference(REQUIRED_COLUMNS)
    )
    fieldnames = [*REQUIRED_COLUMNS, *extra_columns]
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--timezone", default=DEFAULT_TIMEZONE)
    args = parser.parse_args()

    station_token = os.environ.get("BW_DEVICE_ID", "").strip()
    if not station_token:
        raise SystemExit("Missing BW_DEVICE_ID environment variable")
    detections = fetch_current_day(station_token, args.timezone)
    write_csv(detections, args.output)
    print(f"Downloaded {len(detections)} current-day detections")


if __name__ == "__main__":
    main()
