from __future__ import annotations

from dataclasses import dataclass

import pandas as pd

from bioTea.utils.errors import UnsupportedChip


@dataclass
class GeoPlatform:
    accession: str
    manufacturer: str
    title: str

    def __str__(self) -> str:
        return (
            f"GEO - Platform ({self.accession}) - {self.title} from {self.manufacturer}"
        )


@dataclass
class GeoSample:
    accession: str
    source: str
    extracted_molecule: str
    organism: str
    conditions: dict
    suppl_data_ftp: str
    suppl_data_local_path: str = None

    def __str__(self) -> str:
        return f"GEO - Sample ({self.accession}): {self.__dict__}"

    @classmethod
    def sample_from_xmldict(cls: GeoSample, raw_dict: dict) -> GeoSample:
        """Generate a new instance of the class from a dict parsed from an xml"""
        # Fail if the sample is more than one channel
        if (ch_count := int(raw_dict["Channel-Count"])) != 1:
            raise UnsupportedChip(f"Too many channels to parse ({ch_count})")

        if type((char := raw_dict["Channel"]["Characteristics"])) is str:
            conditions = {"type": char}
        else:
            # This is a list of small dicts. We fuse them together.
            def dictify(dict):
                return {dict["@tag"]: dict["#text"]}

            char = [dictify(x) for x in char]
            conditions = {}
            [conditions.update(x) for x in char]

        return cls(
            accession=raw_dict["@iid"],
            organism=raw_dict["Channel"]["Organism"]["#text"],
            source=raw_dict["Channel"]["Source"],
            extracted_molecule=raw_dict["Channel"]["Molecule"],
            conditions=conditions,
            suppl_data_ftp=raw_dict["Supplementary-Data"]["#text"],
        )


@dataclass
class GeoSeries:
    accession: str
    platform: GeoPlatform
    authors: list[str]
    samples: list[GeoSample]

    def __str__(self) -> str:
        authors = ", ".join(self.authors)
        samples = ", ".join([str(x) for x in self.samples])
        return f"GEO - Series ({self.accession}) from {authors}. Platform {self.platform} with {len(self.samples)} samples: {samples}"

    @property
    def design(self) -> str:
        design = {"Sample_id": [sample.accession for sample in self.samples]}
        for condition in self.samples[0].conditions:
            design[condition] = [
                sample.conditions[condition] for sample in self.samples
            ]

        return design

    @property
    def design_strings(self) -> str:
        strings = {}
        for key in self.design:
            strings[key] = ", ".join(self.design[key])
        return strings

    def generate_metadata(self) -> pd.DataFrame:
        return pd.DataFrame(data=self.design)
