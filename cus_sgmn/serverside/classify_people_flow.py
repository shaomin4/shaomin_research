# Load configs
import json
with open('../config.json' , 'r') as reader:
    config = json.loads(reader.read())
    config = config[0]

# Change workspace
import os, sys
os.chdir(config['server']['base_path'] + "pytorch-yolo-v3/")
sys.path.insert(0, config['server']['base_path'] + "pytorch-yolo-v3/")

# Import required packages
import time
import torch 
import torch.nn as nn
from torch.autograd import Variable
import numpy as np
import cv2 
from util import *
from darknet import Darknet
from preprocess import prep_image, inp_to_image
import pandas as pd
import random 
import argparse
import pickle as pkl
import re
import psycopg2


def arg_parse():
    """
    Parse arguements to the detect module
    
    """
    parser = argparse.ArgumentParser(description='YOLO v3 Cam Demo')
    parser.add_argument("--confidence", dest = "confidence", help = "Object Confidence to filter predictions", default = 0.6)
    parser.add_argument("--nms_thresh", dest = "nms_thresh", help = "NMS Threshhold", default = 0.4)
    parser.add_argument("--reso", dest = 'reso', help = 
                        "Input resolution of the network. Increase to increase accuracy. Decrease to increase speed",
                        default = "960", type = str)
    return parser.parse_args()

def write(x, img):
    c1 = tuple(x[1:3].int())
    c2 = tuple(x[3:5].int())
    cls = int(x[-1])
    label = "{0}".format(classes[cls])
    color = random.choice(colors)
    cv2.rectangle(img, c1, c2,color, 1)
    t_size = cv2.getTextSize(label, cv2.FONT_HERSHEY_PLAIN, 1 , 1)[0]
    c2 = c1[0] + t_size[0] + 3, c1[1] + t_size[1] + 4
    cv2.rectangle(img, c1, c2,color, -1)
    cv2.putText(img, label, (c1[0], c1[1] + t_size[1] + 4), cv2.FONT_HERSHEY_PLAIN, 1, [225,255,255], 1);
    return img

def prep_image(img, inp_dim):
    """
    Prepare image for inputting to the neural network. 
    
    Returns a Variable 
    """
    orig_im = img
    dim = orig_im.shape[1], orig_im.shape[0]
    img = cv2.resize(orig_im, (inp_dim, inp_dim))
    img_ = img[:,:,::-1].transpose((2,0,1)).copy()
    img_ = torch.from_numpy(img_).float().div(255.0).unsqueeze(0)
    return img_, orig_im, dim


# Set yolov3 (darknet) config file and weights
cfgfile = "cfg/yolov3.cfg"
weightsfile = "yolov3.weights"

# Predict different 80 classes from yolo
num_classes = 80

args = arg_parse()
confidence = float(args.confidence)
nms_thesh = float(args.nms_thresh)
CUDA = torch.cuda.is_available()
torch.cuda.set_device(3)
print(torch.cuda.current_device())


# Load Darknet
model = Darknet(cfgfile)
model.load_weights(weightsfile)

# model.net_info["height"] = 640
model.net_info["height"] = args.reso
inp_dim = int(model.net_info["height"])

assert inp_dim % 32 == 0 
assert inp_dim > 32

if CUDA:
    model.cuda()

raw_photo_saved_path = config['server']['people_flow']['path']['raw_photos']
processed_photo_saved_path = config['server']['people_flow']['path']['processed_photos']
    
classes = load_classes('data/coco.names')
colors = pkl.load(open("pallete", "rb"))

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
   WHERE  table_name = '" + config['server']['people_flow']['psql_table'] + "' \
   );"
cur.execute(check_table_exists_sql)
conn.commit()
check_result = cur.fetchall()
if(check_result[0][0] == False):
    # Create table
    cur.execute("CREATE TABLE people_flow(machine_id int, \
                                          time timestamp, \
                                          people_flow_number int, \
                                          PRIMARY KEY(machine_id, time));")
    conn.commit()

    
while(True):
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
            photo_path = raw_photo_saved_path + photo_name
            
            
            splitted_photo_name = photo_name.split("_")
            # Extract machine id from photo_name
            machine_id = int(splitted_photo_name[0])
            # Extract timestamp from photo_name
            timestamp = float(splitted_photo_name[1].split(".jpg")[0])
            
            # Read photo
            frame = cv2.imread(photo_path, flags=cv2.IMREAD_COLOR)
            # Prepare image vectors
            img, orig_im, dim = prep_image(frame, inp_dim)

            im_dim = torch.FloatTensor(dim).repeat(1,2)

            if CUDA:
                im_dim = im_dim.cuda()
                img = img.cuda()

            output = model(Variable(img), CUDA)
            output = write_results(output, confidence, num_classes, nms = True, nms_conf = nms_thesh)

            output[:,1:5] = torch.clamp(output[:,1:5], 0.0, float(inp_dim))/inp_dim

            output[:,[1,3]] *= frame.shape[1]
            output[:,[2,4]] *= frame.shape[0]

            list(map(lambda x: write(x, orig_im), output))
            cv2.imwrite(processed_photo_saved_path + photo_name, orig_im)
            
            # Count the number of people in image
            people_count = 0
            for target in output:
                if(classes[int(target[7])] == 'person'):
                    people_count = people_count+1
                        
            # Insert new classification record
            sql = "INSERT INTO people_flow(machine_id, time, people_flow_number) \
                   VALUES({:d}, to_timestamp({:f}), {:d});".format(machine_id, timestamp, people_count)
            
            try:
                cur.execute(sql)
            except Exception as e:
                print(e)
            
            conn.commit()
            
            # Classify successfully, delete uploaded file instantly
            os.remove(photo_path)
            print("Remove file: " + photo_path)
                
cur.close()
conn.close()
