# cus_sgmn 消費客群影像辨識模型開發與設計(娃娃機計畫)

整個程式運行分為兩部分:

1. Local-side
2. Server-side

以下依序介紹各部分的功能執行

- Local-side: 在本機端會以 python3 在 windows 背景執行兩隻程式，分別為 motion detection 跟 photo uploader，由於本計畫預計會有多台娃娃機擺放，故在執行程式前得先在設定檔內 (`config.json`) 中設定機器的獨立ID (參數 `machine_id`)，此外執行程式前請確定照相機是否已經安裝，這些程式在 windows 中可以透過內建的工作排程器 (task scheduler) 來開機背景執行。

  - motion detection: 在娃娃機計畫中預計有兩台攝影機，一台用來節錄人流畫面，另外一台節錄使用者頭像，用以分析年齡以及性別，所以照理來說我們會執行兩隻 motion detection 的程式分別做以上兩件任務，在這個部分會需要在設定檔內 (`config.json`) 設定好 `people_flow` 跟 `age_gender` 各自的照片暫存路徑 (`temp_saved_photos`)，此二路徑不可重複或重疊。<br/>
    執行程式碼:<br/>
    ```bash
    python3 ./localside/detect_motion.py --function people_flow --camera_id 0   # 執行人流紀錄程式 (以0號攝影機拍攝)
    python3 ./localside/detect_motion.py --function age_gender --camera_id 1    # 執行使用者頭像紀錄程式  (以1號攝影機拍攝)
    ```
      
  - photo uploader: 若照片已拍攝並存放至暫存路徑，程式將會自動撈取被新增的相片路徑並持續上傳到伺服器<br/>
    執行程式碼:<br/>
    ```bash
    python3 ./localside/upload_photo_local.py --function people_flow   # 執行人流照片上傳程式
    python3 ./localside/upload_photo_local.py --function age_gender    # 執行使用者頭像照片上傳程式
    ```
      
- Server-side: 在遠端伺服器上會有以下幾隻程式背景執行，將依序 `upload_photo.php`、`classify_people_flow.py`、`classify_age_gender.py` 及 `shiny app` 介紹，由於伺服器端程式會使用到 php，故請安裝 apache 等網頁伺服器軟體，而所有分析結果將會寫入到資料庫中 (PostgreSQL)，故也必須確保資料庫 (`cus_sgmn`) 以及相關使用者都已經建立並更新設定檔 (`config.json`) 內的參數，更別忘了安裝 shiny server。

  - upload_photo.php: 當本機端程式暫存照片時，必須將照片上傳至伺服器上做模型推斷並將結果更新至資料庫，這隻程式主要是將上傳的照片寫到伺服器上的資料儲存空間，務必設定好照片存放的資料夾位置（包括raw_photos、processed_photos、error_photos，主要分別儲存未處理照片、處理完成照片及錯誤處理照片），如果 php 程式已經在 apache 等服務上架設成功，務必確認對外的網址並更新到各機台的 `config.json` 中的 `upload_photos_url`
  - classify_people_flow.py: 本程式會將人流部份的照片從伺服器儲存資料夾中撈出並作模型推論 (使用YOLO.v3)，再將分析結果細項（包括機台ID、時間點、人數）一併上傳到資料表中。<br/>
    執行程式碼:<br/>
    ```bash
    python3 ./serverside/classify_people_flow.py
    nohup python3 ./serverside/classify_people_flow.py &     # 若以背景執行
    ```
      
  - classify_age_gender.py: 本程式會將頭像部份的照片從伺服器儲存資料夾中撈出並作模型推論 (使用CNN)，再將分析結果細項（包括機台ID、時間點、性別、年齡、該時間點第n名頭像）一併上傳到資料表中。<br/>
    執行程式碼:<br/>
    ```bash
    python3 ./serverside/classify_age_gender.py
    nohup python3 ./serverside/classify_age_gender.py &     # 若以背景執行
    ```
      
  - shiny app:<br/>
    - people_flow_app: 此 app 用以顯示各個機台的人流，若點取時間點則會在下半部畫面顯善模型分析結果<br/>
    - age_gender_app: 此 app 用以顯示各個機台的頭像資訊，若點取時間點則會在下半部畫面顯善模型分析結果