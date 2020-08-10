

# UMC & NSYSU CM Unified Analytics Platform Management

Yihuang Kang

3/21/2018


我們管電大數據平台與聯電的計畫從 2018/03 做到 2018/04。 工作內容除了現有大數據平台的建置外，需要開發 web-based data management & pipeline applications，也就是常見的企業資料分析平台(unified analytics platform)建置。
下列是目前比較簡單的需求方向：

  Web-based 後台管理，分一般 User 與 Administrator, 使用 Shiny/Plumber/H2O 為主來開發:
  
  User & Administrator: 

 1. A login Page (like RStudio Server/Jupyter Notebook login page) 
 2. File browser + Data import/export (CVS from/to Greenplum)
 3. Data (Greenplum Tables) Viewer 
 4. HDFS file browser & management 
 5. System loading Viewer (embbed H2O water meter via H2O REST APIs ?)

Administrator: 

 6. System User/Group Manager 
 7. Greenplum User/Group Administration 
 8. HDFS User/Group Management 
 9. GPU Resource Monitor & Manager

至於粗略的進度要求: 

 - 3/31底要有 #1, #2, #3,  
 - 4/30 聯電計畫 meeting 前要有 #1-5  
 - 6 月初管電要有 #1-5 的 demo

