## xgboost example
#I evaluate performance of the entire boosting algorithm using the commonly benchmarked UCI Higgs dataset. 
#This is a binary classification problem with 11M rows * 29 features and is a relatively time consuming problem in the single machine setting.
#The following Python script runs the XGBoost algorithm. It outputs the decreasing test error during boosting and measures the time taken by GPU and CPU algorithms.

#> refference :[Gradient Boosting, Decision Trees and XGBoost with CUDA](https://devblogs.nvidia.com/gradient-boosting-decision-trees-xgboost-cuda/)

import csv
import numpy as np
import os.path
import pandas
import time
import xgboost as xgb
import sys
import matplotlib.pyplot as plt
if sys.version_info[0] >= 3:
    from urllib.request import urlretrieve
else:
    from urllib import urlretrieve

data_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00280/HIGGS.csv.gz"
server_file_path = "/data_lake/Higgs/"
dmatrix_train_filename = "higgs_train.dmatrix"
dmatrix_test_filename = "higgs_test.dmatrix"
csv_filename = "HIGGS.csv.gz"
train_rows = 10500000
test_rows = 500000
num_round = 20

plot = True

# return xgboost dmatrix
def load_higgs():
    if os.path.isfile(server_file_path+dmatrix_train_filename) and os.path.isfile(server_file_path+dmatrix_test_filename):
        dtrain = xgb.DMatrix(server_file_path+dmatrix_train_filename)
        dtest = xgb.DMatrix(server_file_path+dmatrix_test_filename)
        if dtrain.num_row() == train_rows and dtest.num_row() == test_rows:
            print("Loading cached dmatrix...")
            return dtrain, dtest

    if not os.path.isfile(server_file_path+csv_filename):
        print("Downloading higgs file...")
        urlretrieve(data_url, csv_filename)
        
    print("Read train csv")
    df_higgs_train = pandas.read_csv(csv_filename, dtype=np.float32, 
                                     nrows=train_rows, header=None)
    dtrain = xgb.DMatrix(df_higgs_train.ix[:, 1:29], df_higgs_train[0])
    dtrain.save_binary(dmatrix_train_filename)
    print("Read test csv")
    df_higgs_test = pandas.read_csv(csv_filename, dtype=np.float32, 
                                    skiprows=train_rows, nrows=test_rows, 
                                    header=None)
    dtest = xgb.DMatrix(df_higgs_test.ix[:, 1:29], df_higgs_test[0])
    dtest.save_binary(dmatrix_test_filename)

    return dtrain, dtest

dtrain, dtest = load_higgs()

# 設定 xgboost 使用變數，會依造tree_method的型態決定使不使用GPU
# 更多參數，請參考 https://xgboost.readthedocs.io/en/latest/parameter.html
param = {}
param['objective'] = 'binary:logitraw'
param['eval_metric'] = 'error'
param['tree_method'] = ''  # 這裡先給空值，後續會依造使用需求加入
param['silent'] = 1

# 注意!!! 跑這裡會吃掉全部的核心，請確定機器夠力以及沒有其他使用者需要使用~
print("Training with CPU ...")
param['tree_method'] = 'hist'
#param['nthread'] = 8 # 限制使用核心數!
tmp = time.time()
cpu_res = {}
xgb.train(param, dtrain, num_round, evals=[(dtest, "test")], 
          evals_result=cpu_res)
cpu_time = time.time() - tmp
print("CPU Training Time: %s seconds" % (str(cpu_time)))

print("Training with GPU ...")
param['tree_method'] = 'gpu_hist'
tmp = time.time()
gpu_res = {}
xgb.train(param, dtrain, num_round, evals=[(dtest, "test")], 
          evals_result=gpu_res)
gpu_time = time.time() - tmp
print("GPU Training Time: %s seconds" % (str(gpu_time)))



min_error = min(min(gpu_res["test"][param['eval_metric']]), min(cpu_res["test"][param['eval_metric']]))
gpu_iteration_time = [x / (num_round * 1.0) * gpu_time for x in range(0, num_round)]
cpu_iteration_time = [x / (num_round * 1.0) * cpu_time for x in range(0, num_round)]


plt.plot(gpu_iteration_time, gpu_res['test'][param['eval_metric']],label='GPU:2080 ti * 1')
plt.plot(cpu_iteration_time, cpu_res['test'][param['eval_metric']],label='CPU: Intel(R) Xeon(R) CPU E5-2650 v4 @ 2.20GHz (48 cores)')
plt.legend()
plt.xlabel('Time (s)')
plt.ylabel('Test error')
plt.axhline(y=min_error, color='r', linestyle='dashed')
plt.margins(x=0)
plt.ylim((0.23, 0.35))
plt.show()