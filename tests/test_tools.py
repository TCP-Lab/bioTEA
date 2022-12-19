from bioTea.utils import tools


def test_tempdir_changer():
    import os

    prev_wd = os.getcwd()

    with tools.TempWorkdir("/tmp/banana"):
        new_wd = os.getcwd()

    restored_wd = os.getcwd()

    assert new_wd == "/tmp/banana"
    assert prev_wd == restored_wd


def test_ping():
    # I assume google is always up
    res = tools.ping("google.com")
    assert res is True


def test_making_geo_urls():
    codes = {
        "GPL": {
            "miniml": {"GPL"},
            "soft": "ftp://ftp.ncbi.nlm.nih.gov/geo/platforms/{obsid}/{id}/soft/",
            "suppl": "ftp://ftp.ncbi.nlm.nih.gov/geo/platforms/{obsid}/{id}/suppl/",
        },
        "GDS": {
            "miniml": "ftp://ftp.ncbi.nlm.nih.gov/geo/datasets/{obsid}/{id}/miniml/",
            "soft": "ftp://ftp.ncbi.nlm.nih.gov/geo/datasets/{obsid}/{id}/soft",
            "suppl": "ftp://ftp.ncbi.nlm.nih.gov/geo/datasets/{obsid}/{id}/suppl",
        },
        "GSM": {
            "miniml": "ftp://ftp.ncbi.nlm.nih.gov/geo/samples/{obsid}/{id}/miniml/",
            "soft": "ftp://ftp.ncbi.nlm.nih.gov/geo/samples/{obsid}/{id}/soft",
            "suppl": "ftp://ftp.ncbi.nlm.nih.gov/geo/samples/{obsid}/{id}/suppl",
        },
        "GSE": {
            "miniml": "ftp://ftp.ncbi.nlm.nih.gov/geo/series/{obsid}/{id}/miniml/",
            "soft": "ftp://ftp.ncbi.nlm.nih.gov/geo/series/{obsid}/{id}/soft",
            "suppl": "ftp://ftp.ncbi.nlm.nih.gov/geo/series/{obsid}/{id}/suppl",
        },
    }
