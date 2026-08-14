#!/usr/bin/env python3
"""Fetch BirdWeather's 48-week seasonal probabilities for dashboard input.

The station reference is read from private detection CSV input and is used only
for the GraphQL request. The generated intermediate JSON deliberately omits the
station reference, coordinates, detections, audio and timestamps.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import tempfile
import time
from pathlib import Path
from typing import Any, Iterable
from urllib.error import HTTPError, URLError
from urllib.parse import urlparse
from urllib.request import Request, urlopen


GRAPHQL_ENDPOINT = "https://app.birdweather.com/graphql"
DEFAULT_INPUT = Path("data/master_detections.csv")
STATION_COLUMNS = ("stationId", "station.id", "station_id")
QUERY = """
query SeasonalProbabilities($id: ID!) {
  station(id: $id) {
    timezone
    probabilities {
      weeks
      species {
        commonName
        scientificName
        imageUrl
        thumbnailUrl
      }
    }
  }
}
"""


def infer_station_reference(input_paths: Iterable[Path]) -> str:
    """Read the private station reference without returning any row data."""

    references: set[str] = set()
    for input_path in input_paths:
        if not input_path.exists() or input_path.stat().st_size == 0:
            continue
        with input_path.open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.DictReader(handle)
            station_column = next(
                (column for column in STATION_COLUMNS if column in (reader.fieldnames or [])),
                None,
            )
            if station_column is None:
                continue
            for row in reader:
                value = (row.get(station_column) or "").strip()
                if value:
                    references.add(value)

    if not references:
        raise ValueError("No station reference found in the detection CSV input")
    if len(references) != 1:
        raise ValueError("Multiple station references found; refusing to guess")
    return references.pop()


def request_probabilities(station_reference: str, attempts: int = 4) -> dict[str, Any]:
    """Request only timezone, species metadata and seasonal probability curves."""

    body = json.dumps(
        {"query": QUERY, "variables": {"id": station_reference}},
        separators=(",", ":"),
    ).encode("utf-8")
    for attempt in range(attempts):
        try:
            request = Request(
                GRAPHQL_ENDPOINT,
                data=body,
                headers={
                    "Accept": "application/json",
                    "Content-Type": "application/json",
                    "User-Agent": "birdweather-logger-seasonal-outlook/1.0",
                },
                method="POST",
            )
            with urlopen(request, timeout=45) as response:
                payload = json.load(response)
            errors = payload.get("errors")
            if errors:
                raise RuntimeError("BirdWeather GraphQL returned an error")
            station = (payload.get("data") or {}).get("station")
            if not isinstance(station, dict):
                raise RuntimeError("BirdWeather returned no station probability data")
            return station
        except HTTPError as error:
            if error.code in {400, 401, 403, 404} or attempt == attempts - 1:
                raise RuntimeError(
                    f"BirdWeather probability request failed with HTTP {error.code}"
                ) from error
        except (URLError, TimeoutError, json.JSONDecodeError) as error:
            if attempt == attempts - 1:
                raise RuntimeError(
                    "BirdWeather probability request failed after retries"
                ) from error
        time.sleep(2**attempt)
    raise RuntimeError("BirdWeather probability request failed")


def birdweather_photo_urls(species: dict[str, Any]) -> list[str]:
    """Keep only HTTPS species images hosted by BirdWeather."""

    photos: list[str] = []
    for key in ("imageUrl", "thumbnailUrl"):
        candidate = str(species.get(key) or "").strip()
        parsed = urlparse(candidate)
        if (
            parsed.scheme == "https"
            and parsed.netloc == "media.birdweather.com"
            and parsed.path.startswith("/species/")
        ):
            photos.append(candidate)
    return list(dict.fromkeys(photos))


def public_probability_input(station: dict[str, Any]) -> dict[str, Any]:
    """Reduce a GraphQL station response to a privacy-safe intermediate shape."""

    species_rows: list[dict[str, Any]] = []
    for probability in station.get("probabilities") or []:
        if not isinstance(probability, dict):
            continue
        species = probability.get("species") or {}
        common_name = str(species.get("commonName") or "").strip()
        scientific_name = str(species.get("scientificName") or "").strip()
        weeks = probability.get("weeks")
        if not common_name or not isinstance(weeks, list) or len(weeks) != 48:
            continue
        try:
            values = [float(value) for value in weeks]
        except (TypeError, ValueError):
            continue
        if any(not math.isfinite(value) or not 0 <= value <= 1 for value in values):
            continue
        species_rows.append(
            {
                "common_name": common_name,
                "scientific_name": scientific_name or None,
                "photo_urls": birdweather_photo_urls(species),
                "weeks": values,
            }
        )

    if not species_rows:
        raise ValueError("BirdWeather returned no valid 48-week probability curves")
    return {
        "timezone": str(station.get("timezone") or ""),
        "species": species_rows,
    }


def write_probability_input(payload: dict[str, Any], output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", dir=output_path.parent, delete=False
    ) as handle:
        json.dump(payload, handle, ensure_ascii=False, separators=(",", ":"))
        handle.write("\n")
        temporary_path = Path(handle.name)
    temporary_path.replace(output_path)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--input",
        action="append",
        type=Path,
        default=[],
        help="Private detection CSV used only to infer the station reference",
    )
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    input_paths = args.input or [DEFAULT_INPUT]
    station_reference = infer_station_reference(input_paths)
    station = request_probabilities(station_reference)
    payload = public_probability_input(station)
    write_probability_input(payload, args.output)
    print(f"Downloaded {len(payload['species'])} seasonal probability curves")


if __name__ == "__main__":
    main()
