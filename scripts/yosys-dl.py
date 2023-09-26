#!/usr/bin/env python

from json import loads as json_decode
from shutil import copyfileobj as stream_copy
from subprocess import run as proc_run, PIPE
from os.path import isfile
from urllib.request import urlopen as http_get

YOSYS_RELEASE_URL = "https://api.github.com/repos/YosysHQ/yosys/releases"
YOSYS_ABC_TAGS_URL = "https://api.github.com/repos/YosysHQ/abc/tags"

DEBUG = True
DRY_RUN = False


def dbg_print(s):
    if DEBUG:
        print(s)


def download_file(src, dest=None):
    dbg_print(f"Downloading {src}...")
    with http_get(src) as f_src:
        if dest is None:
            content_disposition = f_src.headers.get("content-disposition")
            if content_disposition is None:
                dest = src.split("/")[-1]
            else:
                start = content_disposition.find("filename=")
                dest = content_disposition[start + len("filename=") :]
        dbg_print(f"Destination {dest}")
        if not DRY_RUN:
            with open(dest, "w+b") as f_dest:
                stream_copy(f_src, f_dest)
        return dest


def untar(file):
    run = proc_run(["tar", "--version"], stdout=PIPE, encoding="utf-8")
    if run.returncode != 0:
        print("tar command error:")
        print(run.stdout)
        print(run.stderr)
        raise RuntimeError()
    tar_version = run.stdout.splitlines()[0].split()[-1]
    dbg_print(f"Using tar version {tar_version}")
    if not isfile(file):
        print(f"file not found {file}")
        raise RuntimeError()
    dbg_print(f"Extracting tar archive {file}...")
    if not DRY_RUN:
        run = proc_run(
            ["tar", "--extract", "--verbose", "--file", file],
            stdout=PIPE,
            encoding="utf-8",
        )
        if run.returncode != 0:
            print("Failed to extract archive:")
            print(run.stdout)
            print(run.stderr)
            raise RuntimeError()
        extracted_files = run.stdout.splitlines()
        dirs = set()
        for f in extracted_files:
            dir = f.split("/")[0]
            if dir not in dirs:
                dirs.add(dir)
        dbg_print(f"Extracted to {list(dirs)[0]}")


def yosys_releases():
    with http_get(YOSYS_RELEASE_URL) as db:
        return json_decode(db.read())


def yosys_abc_tags():
    with http_get(YOSYS_ABC_TAGS_URL) as db:
        return json_decode(db.read())


if __name__ == "__main__":
    db = yosys_releases()
    tarball_url = db[0]["tarball_url"]
    dest = download_file(tarball_url)
    untar(dest)
