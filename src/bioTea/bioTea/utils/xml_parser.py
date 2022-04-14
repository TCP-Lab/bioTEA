"""xml parsing to get useful objects"""
# To annotate the class with itself
from __future__ import annotations

import logging
import os
import shutil
from pathlib import Path
from typing import BinaryIO

import xmltodict

from bioTea.classes import GeoPlatform, GeoSample, GeoSeries
from bioTea.utils.tools import PathLike, contains_all, download_ftp, make_geo_ftp

log = logging.getLogger(__name__)


class XmlMinimlExtractor:
    def __init__(self, xml_bytes: BinaryIO) -> None:
        self.xdict = xmltodict.parse(xml_bytes)["MINiML"]

    def extract_authors(self) -> list[str]:
        raw_authors = self.xdict["Contributor"]
        authors = []
        for author in raw_authors:
            if "Person" in author.keys():
                authors.append(
                    "{} {}".format(author["Person"]["First"], author["Person"]["Last"])
                )

        return authors

    def extract_samples(self) -> list[GeoSample]:
        raw_samples = self.xdict["Sample"]
        sample_objects = []
        for sample in raw_samples:
            sample_obj = GeoSample.sample_from_xmldict(sample)
            sample_objects.append(sample_obj)

        log.debug("Sanity Check: Do all samples have the same conditions?")
        for i, sample in enumerate(sample_objects):
            if not contains_all(
                list(sample_obj.conditions.keys()), list(sample.conditions.keys())
            ):
                extra_keys = set(sample.conditions.keys()) - set(
                    sample_obj.conditions.keys()
                )
                log.warn(
                    "Failed sanity check: Not all samples have the same conditions. "
                    f"(Extra keys: {extra_keys}) "
                    "Removing the extra keys."
                )
                for key in extra_keys:
                    log.debug(f"Popping {key}..")
                    sample.conditions.pop(key)

                log.debug(f"Dict after popping: {sample.conditions.keys()}")
                sample_objects[i] = sample

        return sample_objects

    def extract_platform(self) -> GeoPlatform:
        return GeoPlatform(
            accession=self.xdict["Platform"]["@iid"],
            manufacturer=self.xdict["Platform"]["Manufacturer"],
            title=self.xdict["Platform"]["Title"],
        )

    def extract_id(self) -> str:
        return self.xdict["Series"]["@iid"]


# Have xmltodict do the heavy lifting
def miniml_xml_to_series(xml_path: PathLike) -> GeoSeries:
    # Follow the paths to make the objects
    with Path(xml_path).open("rb") as bytes:
        extractor = XmlMinimlExtractor(bytes)

    project = GeoSeries(
        accession=extractor.extract_id(),
        authors=extractor.extract_authors(),
        platform=extractor.extract_platform(),
        samples=extractor.extract_samples(),
    )

    return project


def get_minimal_from_geo(
    geo_id: str, download_dir: PathLike, cleanup: bool = False
) -> GeoSeries:
    """Generate a GeoSeries object for a certain GEO Id.

    Uses the miniml file to generate the object.

    Args:
        geo_id (str): The ID of the wanted series.
        download_dir (PathLike): (Temporary) directory to download to.
        cleanup (bool, optional): If True, downloaded files are deleted once the fuction ends. Defaults to False.

    Returns:
        GeoSeries: The GeoSeries object for the specified series.
    """
    download_dir = Path(download_dir)
    log.info(f"Retrieving GEO MINiML file for {geo_id}")

    ftp_url = make_geo_ftp(geo_id, "miniml")
    downloaded = download_ftp(ftp_url, download_dir)
    # There should be just a single file, the zipped file with the data
    output_folder = download_dir / "minimal_files"
    os.makedirs(output_folder)
    # We downloaded only one file (hopefully)
    # TODO: this seems a hack
    shutil.unpack_archive(list(downloaded.values())[0], output_folder)

    unzipped_files = os.listdir(output_folder)
    file = [Path(x) for x in unzipped_files if x.endswith("_family.xml")][0]

    series = miniml_xml_to_series(output_folder / file)

    if cleanup:
        shutil.rmtree(output_folder)
        os.remove(downloaded)

    return series
