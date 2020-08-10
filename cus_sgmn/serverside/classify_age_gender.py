# Load configs
import json
with open('../config.json' , 'r') as reader:
    config = json.loads(reader.read())
    config = config[0]

import os, sys
os.chdir(config['server']['base_path'] + "AgeGender/")
sys.path.insert(0, config['server']['base_path'] + "AgeGender/")

# Import required modules
import cv2 as cv
import math
import time
import argparse
import psycopg2
import re

def getFaceBox(net, frame, conf_threshold=0.7):
    frameOpencvDnn = frame.copy()
    frameHeight = frameOpencvDnn.shape[0]
    frameWidth = frameOpencvDnn.shape[1]
    blob = cv.dnn.blobFromImage(frameOpencvDnn, 1.0, (300, 300), [104, 117, 123], True, False)

    net.setInput(blob)
    detections = net.forward()
    bboxes = []
    for i in range(detections.shape[2]):
        confidence = detections[0, 0, i, 2]
        if confidence > conf_threshold:
            x1 = int(detections[0, 0, i, 3] * frameWidth)
            y1 = int(detections[0, 0, i, 4] * frameHeight)
            x2 = int(detections[0, 0, i, 5] * frameWidth)
            y2 = int(detections[0, 0, i, 6] * frameHeight)
            bboxes.append([x1, y1, x2, y2])
            cv.rectangle(frameOpencvDnn, (x1, y1), (x2, y2), (0, 255, 0), int(round(frameHeight/150)), 8)
    return frameOpencvDnn, bboxes

faceProto = "opencv_face_detector.pbtxt"
faceModel = "opencv_face_detector_uint8.pb"

ageProto = "age_deploy.prototxt"
ageModel = "age_net.caffemodel"

genderProto = "gender_deploy.prototxt"
genderModel = "gender_net.caffemodel"

MODEL_MEAN_VALUES = (78.4263377603, 87.7689143744, 114.895847746)
ageList = ['(0-2)', '(4-6)', '(8-12)', '(15-20)', '(25-32)', '(38-43)', '(48-53)', '(60-100)']
genderList = ['Male', 'Female']

# Load network
ageNet = cv.dnn.readNet(ageModel, ageProto)
genderNet = cv.dnn.readNet(genderModel, genderProto)
faceNet = cv.dnn.readNet(faceModel, faceProto)
padding = 20

raw_photo_saved_path = config['server']['age_gender']['path']['raw_photos']
processed_photo_saved_path = config['server']['age_gender']['path']['processed_photos']
error_processed_photo_saved_path = config['server']['age_gender']['path']['error_photos']

# Connect databse (postgresql)
conn = psycopg2.connect(database=config['server']['psql_database'], 
                        user=config['server']['psql_user'], 
                        password=config['server']['psql_password'], 
                        host=config['server']['psql_host'], 
                        port=config['server']['psql_port'])
cur = conn.cursor()

# Check table exists
check_table_exists_sql = "SELECT EXISTS ( \
   SELECT 1 \
   FROM   information_schema.tables \
   WHERE  table_name = '" + config['server']['age_gender']['psql_table'] + "' \
   );"
cur.execute(check_table_exists_sql)
conn.commit()
check_result = cur.fetchall()
if(check_result[0][0] == False):
    # Create table
    cur.execute("CREATE TABLE age_gender(machine_id int, \
                                         time timestamp, \
                                         age int, \
                                         gender int, \
                                         index int, \
                                         PRIMARY KEY(machine_id, time, index));")
    conn.commit()

while True:
    filenames_set = []
    
    # Get photo's names under photo saved directory
    for (dirpath, dirnames, filenames) in os.walk(raw_photo_saved_path):
        filenames_set.extend(filenames)
    
    for filename in filenames_set:
        # Check file name is in right format
        filename_check = re.search("[0-9]+_[0-9]+\.[0-9]*\.jpg", filename)
        if(filename_check == None):
            continue
        else:
            photo_name = filename
            original_photo_path = raw_photo_saved_path + photo_name
        
            cap = cv.VideoCapture(original_photo_path)

            hasFrame, frame = cap.read()
            frameFace, bboxes = getFaceBox(faceNet, frame, conf_threshold=0.7)
            
            if len(bboxes) > 0:
                write_photo_results = []
                processed_photo_path = processed_photo_saved_path + filename
                splitted_photo_name = photo_name.split("_")
                # Extract machine id from photo_name
                machine_id = int(splitted_photo_name[0])
                # Extract timestamp from photo_name
                timestamp = float(splitted_photo_name[1].split(".jpg")[0])
                for idx, bbox in enumerate(bboxes):
                    face = frame[max(0,bbox[1]-padding):min(bbox[3]+padding,frame.shape[0]-1),max(0,bbox[0]-padding):min(bbox[2]+padding, frame.shape[1]-1)]
                    blob = cv.dnn.blobFromImage(face, 1.0, (227, 227), MODEL_MEAN_VALUES, swapRB=False)
                    
                    # Gender
                    genderNet.setInput(blob)
                    genderPreds = genderNet.forward()
                    gender_idx = genderPreds[0].argmax()
                    gender = genderList[gender_idx]
                    # Age
                    ageNet.setInput(blob)
                    agePreds = ageNet.forward()
                    age_idx = agePreds[0].argmax()
                    age = ageList[age_idx]
                    # Insert new classification record
                    sql = "INSERT INTO age_gender(machine_id, time, age, gender, index) \
                           VALUES({:d}, to_timestamp({:f}), {:d}, {:d}, {:d});".format(machine_id, timestamp, age_idx, gender_idx, idx)
                    try:
                        cur.execute(sql)
                    except Exception as e:
                        print(e)
                    conn.commit()

                    label = "{},{}".format(gender, age)
                    print(label)
                    cv.putText(frameFace, label, (bbox[0], bbox[1]-10), cv.FONT_HERSHEY_SIMPLEX, 0.8, (0, 255, 255), 2, cv.LINE_AA)
                    write_photo_result = cv.imwrite(processed_photo_path, frameFace)
                    write_photo_results.append(write_photo_result)
                
                if all(write_photo_results):
                    os.remove(original_photo_path)
                else:
                    if os.path.exists(processed_photo_path):
                        os.remove(processed_photo_path)
                    os.rename(original_photo_path, error_processed_photo_saved_path + filename)
            else:
                os.rename(original_photo_path, error_processed_photo_saved_path + filename)
