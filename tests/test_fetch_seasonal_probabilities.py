import csv
import json
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))

from fetch_seasonal_probabilities import (  # noqa: E402
    infer_station_reference,
    public_probability_input,
    write_probability_input,
)


class FetchSeasonalProbabilitiesTest(unittest.TestCase):
    def test_infers_private_reference_and_omits_it_from_output(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            detections = root / "detections.csv"
            with detections.open("w", encoding="utf-8", newline="") as handle:
                writer = csv.DictWriter(handle, fieldnames=["stationId", "timestamp"])
                writer.writeheader()
                writer.writerow(
                    {
                        "stationId": "secret-station-reference",
                        "timestamp": "2026-08-14T12:00:00Z",
                    }
                )

            reference = infer_station_reference([detections])
            self.assertEqual(reference, "secret-station-reference")

            station_response = {
                "timezone": "America/Costa_Rica",
                "probabilities": [
                    {
                        "speciesId": "secret-species-reference",
                        "weeks": [0.1] * 48,
                        "species": {
                            "commonName": "Test bird",
                            "scientificName": "Avis probablis",
                            "imageUrl": "https://media.birdweather.com/species/42/test.jpg",
                            "thumbnailUrl": "https://example.invalid/not-allowed.jpg",
                        },
                    }
                ],
            }
            public = public_probability_input(station_response)
            output = root / "seasonal.json"
            write_probability_input(public, output)
            serialised = output.read_text(encoding="utf-8")

            self.assertNotIn("secret-station-reference", serialised)
            self.assertNotIn("secret-species-reference", serialised)
            self.assertEqual(public["species"][0]["common_name"], "Test bird")
            self.assertEqual(len(public["species"][0]["weeks"]), 48)
            self.assertEqual(
                public["species"][0]["photo_urls"],
                ["https://media.birdweather.com/species/42/test.jpg"],
            )
            json.loads(serialised)

    def test_refuses_to_guess_between_multiple_station_references(self):
        with tempfile.TemporaryDirectory() as directory:
            detections = Path(directory) / "detections.csv"
            with detections.open("w", encoding="utf-8", newline="") as handle:
                writer = csv.DictWriter(handle, fieldnames=["stationId"])
                writer.writeheader()
                writer.writerow({"stationId": "one"})
                writer.writerow({"stationId": "two"})
            with self.assertRaises(ValueError):
                infer_station_reference([detections])


if __name__ == "__main__":
    unittest.main()
