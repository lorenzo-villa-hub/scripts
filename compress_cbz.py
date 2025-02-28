#!/usr/bin/env python

import os
from zipfile import ZipFile
from io import BytesIO
from PIL import Image

def compress_image(image_path, quality=30):
    with Image.open(image_path) as img:
        output = BytesIO()
        img.save(output, format='JPEG', quality=quality)
    return output.getvalue()

def compress_cbz(input_cbz, output_cbz, quality=30):
    with ZipFile(input_cbz, 'r') as zip_input:
        with ZipFile(output_cbz, 'w') as zip_output:
            for filename in zip_input.namelist():
                with zip_input.open(filename) as file_in:
                    if filename.lower().endswith(('.jpg', '.jpeg', '.png')):
                        compressed_data = compress_image(file_in, quality)
                        zip_output.writestr(filename, compressed_data)
                    else:
                        zip_output.writestr(filename, file_in.read())

if __name__ == "__main__":
    from glob import glob
    files = glob('*.cbz')
    files.sort()
    quality_percentage = 10
    
    for file in files: 
        input_cbz_file = file # Replace with your input CBZ file name/path
        volume = file.split(' ')[2]
        output_cbz_file = "db_%s.cbz" %volume  # Replace with your output CBZ file name/path
    
        compress_cbz(input_cbz_file, output_cbz_file, quality_percentage)
        print(f"Images in '{input_cbz_file}' compressed to {quality_percentage}% quality and saved to '{output_cbz_file}'.")

