import logging
import os
import shutil
import tempfile
from pathlib import Path
from typing import Tuple

from tqdm import tqdm

from bioTea.classes import GeoSample
from bioTea.utils.errors import SanityError
from bioTea.utils.tools import PathLike, download_ftp, make_geo_ftp
from bioTea.utils.xml_parser import get_minimal_from_geo

log = logging.getLogger(__name__)


def stage_new_analysis(staging_path: PathLike) -> Tuple[Path, Path]:
    """Stage a new analysis folder, with an output and temp folder.

    Args:
        staging_path (PathLike): The path to the output folder

    Returns:
        Tuple[Path, Path]: A tuple with first element the (resolved) output path,
        and second element the path to the temporary folder.
    """
    log.info("Staging new analysis project...")
    staging_path = Path(staging_path).resolve()
    if not staging_path.exists():
        os.makedirs(staging_path)
    log.debug(f"Staged {staging_path}.")
    log.info("Making a new temporary directory...")
    temp_dir = Path(tempfile.mkdtemp()).resolve()
    log.debug(f"Staged tempdir {temp_dir}")

    return (staging_path, temp_dir)


def retrieve_geo_data(output_folder: PathLike, geo_id: str):
    staging_path, temp_dir = stage_new_analysis(output_folder)
    geo_series = get_minimal_from_geo(geo_id, temp_dir)

    log.info("Retrieving sample raw data...")
    downloaded = download_ftp(
        make_geo_ftp(geo_id, "suppl"),
        staging_path / "raw_data",
        filter=lambda x: "_RAW" in x,
    )
    log.debug("Testing validity of downloaded archive...")
    # There should be just a single file
    if len(downloaded) > 1:
        log.warn("Downloaded more than one file. Using just the first one.")

    log.debug("Unpacking downloaded archive...")
    shutil.unpack_archive(list(downloaded.values())[0], temp_dir / "unpacked_samples")
    unpacked_files = os.listdir(temp_dir / "unpacked_samples")

    log.debug(f"Unpacked {len(unpacked_files)} files.")

    log.debug("Sanity Check: Testing congruency with MINiML file...")
    miniml_samples = [
        sample.suppl_data_ftp.split("/")[-1] for sample in geo_series.samples
    ]
    if not all(miniml_sample in unpacked_files for miniml_sample in miniml_samples):
        log.error("Mismatching download and MINiML file. Aborting.")
        raise SanityError("Mismatching downloaded and MINiML files.")

    def add_realpath(sample: GeoSample, realpath: Path) -> GeoSample:
        sample.suppl_data_local_path = realpath
        return sample

    log.debug("Adding real paths to sample objects...")
    geo_series.samples = [
        add_realpath(
            sample, temp_dir / "unpacked_samples" / sample.suppl_data_ftp.split("/")[-1]
        )
        for sample in geo_series.samples
    ]

    log.info("Done retrieving project data.")
    return geo_series
