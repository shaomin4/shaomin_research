'''
    File name: upload_photo_local.py
    Author: Bowen kuo
    Python Version: 3.6
'''
# Load configs
import os
import base64
import requests
import re
import time
import argparse 
import json

# Parsing arguments
def process_command():
    parser = argparse.ArgumentParser()
    parser.add_argument('--function', '-f', type=str, required=True, choices=['people_flow', 'age_gender'], help='Uploading photo to which part of function')
    return parser.parse_args()

# ======== #
#   Main   #
# ======== #
if __name__ == '__main__':
    # Get arguments
    args = process_command()
        
    # Load configs
    with open('../config.json', 'r') as reader:
        config = json.loads(reader.read())
        config = config[0]

    photo_saved_dir = config['local'][args.function]['path']['temp_saved_photos']
    remote_url = config['server']['upload_photos_url']

    requests.packages.urllib3.disable_warnings()

    while(True):
        filenames_set = []

        # Get file names under photo saved directory
        for (dirpath, dirnames, filenames) in os.walk(photo_saved_dir):
            filenames_set.extend(filenames)

        # Sleep for a while becuase some images might are being written.
        time.sleep(1)

        # Upload photo to remote url     
        for filename in filenames_set:
            # Check file name is in right format
            filename_check = re.search("[0-9]+_[0-9]+\.[0-9]*\.jpg", filenames_set[0])
            if(filename_check == None):
                continue
            else:
                photo_name = filename
                photo_path = photo_saved_dir + photo_name
                # Read photo
                with open(photo_path, "rb") as photo_file:
                    photo_data = photo_file.read()
                    encoded_content_string = base64.b64encode(photo_data)
                # Set POST data
                POST_photo_data = {'photo_name': photo_name, 'photo_content': encoded_content_string, 'purpose': "age_gender"}
                # Add data into POST request
                r = requests.post(remote_url, data = POST_photo_data, verify = False) 
                r.text
                result = int(r.text)
                if(result == 0):
                    print("Error: Unable to upload.")
                else:
                    # Uploaded successfully, delete uploaded file instantly
                    os.remove(photo_path)
                    print("Remove file: " + photo_path)
