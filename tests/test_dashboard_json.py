import csv
import json
import sys
import tempfile
import unittest
from datetime import date
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))

from build_dashboard_json import build_dashboard, validate_public_payload  # noqa: E402


class DashboardJsonTest(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.input_path = self.root / "master.csv"
        self.output_path = self.root / "dashboard.json"

        fieldnames = [
            "id",
            "stationId",
            "timestamp",
            "confidence",
            "lat",
            "lon",
            "species.commonName",
            "species.scientificName",
            "species.imageUrl",
            "soundscape.url",
        ]
        rows = [
            {
                "id": "detect-secret-123",
                "stationId": "station-secret-9",
                "timestamp": "2026-07-31T06:30:00Z",
                "confidence": "0.8",
                "lat": "9.9798",
                "lon": "-84.0874",
                "species.commonName": "Clay-colored Thrush",
                "species.scientificName": "Turdus grayi",
                "species.imageUrl": "https://media.birdweather.com/species/547/thrush.jpg",
                "soundscape.url": "https://media.birdweather.com/soundscapes/private.flac",
            },
            {
                "id": "detect-secret-456",
                "stationId": "station-secret-9",
                "timestamp": "2026-07-31T07:45:00Z",
                "confidence": "0.6",
                "lat": "9.9798",
                "lon": "-84.0874",
                "species.commonName": "Clay-colored Thrush",
                "species.scientificName": "Turdus grayi",
                "species.imageUrl": "https://media.birdweather.com/species/547/thrush.jpg",
                "soundscape.url": "https://media.birdweather.com/soundscapes/private.flac",
            },
            {
                "id": "detect-secret-789",
                "stationId": "station-secret-9",
                "timestamp": "2026-07-31T11:00:00+00:00",
                "confidence": "0.9",
                "lat": "9.9798",
                "lon": "-84.0874",
                "species.commonName": "Lesson's Motmot",
                "species.scientificName": "Momotus lessonii",
                "species.imageUrl": "https://media.birdweather.com/species/2200/motmot.jpg",
                "soundscape.url": "https://media.birdweather.com/soundscapes/private.flac",
            },
        ]
        with self.input_path.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(rows)

    def tearDown(self):
        self.tempdir.cleanup()

    def test_builds_aggregate_only_payload_in_costa_rica_time(self):
        payload = build_dashboard(
            self.input_path,
            self.output_path,
            generated_date=date(2026, 8, 1),
        )
        period = payload["periods"]["7d"]

        self.assertEqual(payload["timezone"], "America/Costa_Rica")
        self.assertEqual(period["total_detections"], 3)
        self.assertEqual(period["species_count"], 2)
        self.assertEqual(period["average_confidence"], 0.767)
        self.assertEqual(period["hourly_activity"][0]["detections"], 1)
        self.assertEqual(period["hourly_activity"][1]["detections"], 1)
        self.assertEqual(period["hourly_activity"][5]["detections"], 1)
        self.assertEqual(period["hourly_activity"][0]["species_count"], 1)
        self.assertEqual(period["peak_hour"], {"hour": 0, "detections": 1})
        self.assertEqual(period["new_species_count"], 2)
        self.assertEqual(period["comparison"]["previous_detections"], 0)
        self.assertEqual(
            period["top_species"][0]["photo_url"],
            "https://media.birdweather.com/species/547/thrush.jpg",
        )
        self.assertEqual(
            period["top_species"][0]["photo_urls"],
            ["https://media.birdweather.com/species/547/thrush.jpg"],
        )

        serialised = self.output_path.read_text(encoding="utf-8")
        for sensitive_value in (
            "detect-secret-123",
            "station-secret-9",
            "9.9798",
            "-84.0874",
            "private.flac",
            "2026-07-31T06:30:00Z",
        ):
            self.assertNotIn(sensitive_value, serialised)
        validate_public_payload(json.loads(serialised))

    def test_privacy_validator_rejects_sensitive_shapes(self):
        unsafe_payloads = [
            {"detection_id": 123},
            {"lat": 9.9},
            {"observed": "2026-07-31T06:30:00Z"},
            {"link": "https://media.birdweather.com/soundscapes/example.flac"},
        ]
        for payload in unsafe_payloads:
            with self.subTest(payload=payload):
                with self.assertRaises(ValueError):
                    validate_public_payload(payload)

    def test_builds_period_comparison_without_exposing_rows(self):
        with self.input_path.open("r", encoding="utf-8", newline="") as handle:
            fieldnames = csv.DictReader(handle).fieldnames
        with self.input_path.open("a", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writerow(
                {
                    "id": "older-secret-detection",
                    "timestamp": "2026-07-24T12:00:00Z",
                    "confidence": "0.75",
                    "species.commonName": "Older visitor",
                    "species.scientificName": "Avis prior",
                }
            )

        period = build_dashboard(
            self.input_path,
            self.output_path,
            generated_date=date(2026, 8, 1),
        )["periods"]["7d"]

        self.assertEqual(period["total_detections"], 3)
        self.assertEqual(period["comparison"]["previous_detections"], 1)
        self.assertEqual(period["comparison"]["detection_change_percent"], 200.0)
        self.assertEqual(period["comparison"]["species_change"], 1)
        self.assertNotIn(
            "older-secret-detection",
            self.output_path.read_text(encoding="utf-8"),
        )


if __name__ == "__main__":
    unittest.main()
