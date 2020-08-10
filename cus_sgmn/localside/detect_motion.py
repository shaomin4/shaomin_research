# Import required library
import os, sys, time, datetime
import numpy as np
import cv2
import argparse 
import json
from pathlib import Path

# Parsing arguments
def process_command():
    parser = argparse.ArgumentParser()
    parser.add_argument('--function', '-f', type=str, required=True, choices=['people_flow', 'age_gender'], help='Performed function')
    parser.add_argument('--camera_id', '-c', type=int, required=True, help='Physical Camera ID')
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

    machine_id = str(config['local']['machine_id'])
    photo_saved_dir = config['local'][args.function]['path']['temp_saved_photos']

    # Create directory if the path doesn't exist.
    if not os.path.exists(photo_saved_dir):
        path = Path(photo_saved_dir)
        path.mkdir(parents=True)
    
    # Initialize camera
    camera = cv2.VideoCapture(args.camera_id)
    camera.set(3,1280)
    camera.set(4,720)
    time.sleep(0.5)  # Wait a little moment for camera initilization

    # Master frame
    master = None
    # Time gap to fetch new photo from camera
    time_gap = 3   # seconds

    # Start shoot while detecting any different motion.
    while 1:
        # get current timestamp, which is used for the name of saved photo.
        timestamp = datetime.datetime.now().timestamp()

        # grab a frame
        (grabbed,frame0) = camera.read()

        # end of feed
        if not grabbed:
            # if camera grabbing is failed, should send a warning message to the host.
            break

        # gray frame
        frame1 = cv2.cvtColor(frame0,cv2.COLOR_BGR2GRAY)

        # blur frame
        frame2 = cv2.GaussianBlur(frame1,(21,21),0)

        # initialize master
        if master is None:
            master = frame2
            continue

        # delta frame
        frame3 = cv2.absdiff(master,frame2)

        # threshold frame
        frame4 = cv2.threshold(frame3,15,255,cv2.THRESH_BINARY)[1]

        # dilate the thresholded image to fill in holes
        kernel = np.ones((5,5),np.uint8)
        frame5 = cv2.dilate(frame4,kernel,iterations=4)

        # find contours on thresholded image
        nada,contours = cv2.findContours(frame5.copy(),cv2.RETR_EXTERNAL,cv2.CHAIN_APPROX_SIMPLE)

        # make coutour frame
        frame6 = frame0.copy()

        # target contours
        targets = []

        # loop over the contours
        if(len(contours) > 0):
            cv2.imwrite(photo_saved_dir + machine_id + "_" + str(timestamp) + ".jpg", frame0)

        # update master
        master = frame2

        time.sleep(time_gap)

    # release camera
    camera.release()

    # close all windows
    cv2.destroyAllWindows()
