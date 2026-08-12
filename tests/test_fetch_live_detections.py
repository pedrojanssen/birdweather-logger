import csv
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))

from fetch_live_detections import flatten, write_csv  # noqa: E402


class FetchLiveDetectionsTest(unittest.TestCase):
    def test_flattens_and_writes_birdweather_shape(self):
        detection = {
            "id": 123,
            "timestamp": "2026-08-11T12:15:00-06:00",
            "confidence": 0.91,
            "species": {
                "commonName": "Rufous-tailed Hummingbird",
                "scientificName": "Amazilia tzacatl",
                "imageUrl": "https://media.birdweather.com/species/example.jpg",
            },
            "soundscape": {"url": "https://example.invalid/private.flac"},
        }
        flattened = flatten(detection)
        self.assertEqual(
            flattened["species.commonName"], "Rufous-tailed Hummingbird"
        )

        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "live.csv"
            write_csv([detection], output)
            with output.open("r", encoding="utf-8", newline="") as handle:
                row = next(csv.DictReader(handle))
            self.assertEqual(row["id"], "123")
            self.assertEqual(row["species.scientificName"], "Amazilia tzacatl")
            self.assertIn("soundscape.url", row)


if __name__ == "__main__":
    unittest.main()
