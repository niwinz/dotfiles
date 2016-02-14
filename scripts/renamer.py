#!/usr/bin/env python3

import argparse
import exifread
import os
import os.path as path
import sys
import datetime

IMAGE_EXTS = set([".jpg", ".jpeg", ".JPG", ".JPEG"])

def is_image(_path):
    root, ext = path.splitext(_path)
    return ext.lower() in IMAGE_EXTS

def read_exif(_path):
    with open(_path, "rb") as f:
        return exifread.process_file(f)

def read_exif_date(_path):
    exif = read_exif(_path)
    field = exif["EXIF DateTimeOriginal"]
    date_format = "%Y:%m:%d %H:%M:%S"
    return datetime.datetime.strptime(field.values, date_format)

def format_date(dt):
    date_format = "%Y.%m.%d_%H.%M.%S"
    return dt.strftime(date_format)

def do_rename(_path):
    if not path.exists(_path):
        raise RuntimeException("Directory does not exists.")

    if not path.isdir(_path):
        raise RuntimeException("Is not an directory")

    files = map(path.abspath, os.listdir(_path))
    files = filter(path.isfile, files)
    images = filter(is_image, files)

    for image in images:
        for n_try in range(1000):
            try:
                file_base_name = format_date(read_exif_date(image))
                file_ext = path.splitext(image)[1]

                if n_try == 0:
                    file_name_parts = [file_base_name, file_ext.lower()]
                else:
                    file_name_parts = [file_base_name, "-{}".format(n_try), file_ext.lower()]

                file_name = "".join(file_name_parts)
                file_path = path.join(path.dirname(image), file_name)

                if path.exists(file_path):
                    continue

                print("rename:", image, "->", file_path)
                os.rename(image, file_path)
                break
            except Exception as e:
                print("Error to rename: {}".format(image))

def main():
    parser = argparse.ArgumentParser(description="Exif file renamer.")
    parser.add_argument("path")

    opts = parser.parse_args()

    try:
        do_rename(opts.path)
    except Exception as e:
        print("Error:", e, file=sys.stderr)
        return -1
    else:
        return 0


if __name__ == "__main__":
    sys.exit(main())



