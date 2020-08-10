<?php

# Get POST content
$photo_data = $_POST['photo_content'];
$photo_name = $_POST['photo_name'];
$purpose = $_POST['purpose'];
    
$configs = file_get_contents("../config.json");
$configs = json_decode($configs, true);
$server_saved_directory = $configs[0]['server'][$purpose]['path']['raw_photos'];

# Decode base64
$photo_data = base64_decode($photo_data);

# Setup saved path
$photo_path = $server_saved_directory.$photo_name;
# Save file
$result = file_put_contents($photo_path, $photo_data);

# Return upload result
print($result);

?>