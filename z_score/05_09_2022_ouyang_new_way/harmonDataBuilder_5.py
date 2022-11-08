from neuroCombat import neuroCombat
import pandas as pd
import numpy as np
import os
import re
import sys
import argparse

#import matplotlib.pyplot as plt 
#import seaborn as sns
#from scipy.optimize import curve_fit

# This script generates two types of files: gen_structname.csv and PostHarmon_structname.csv
# such as gen_Vermis.csv and PostHarmon_Vermis.csv
# The script takes Harmonization_meta_data.csv as input file, which has the structnames as
# this first row, such as
# "Path,ICV,Corpus Callosum,Right Ventral DC,Left Ventral DC,Vermis,..."
# and the corresponding data in the following rows, one row for one patient.
# In the sample Hamonization_meta_data.csv file, there are 6049 rows, with the first row as the 
# list of structnames. So it ontains data for 6049-1 = 6048 patients

# The generated gen_structname.csv file, such as gen_Vermis.csv, contains two duplicate 
# rows, with the 2nd row slightly modified (added 0.01 to the last column)
# each column is a for a patient. So for the above Hamonization_meta_data.csv input file, the
# generated gen_Vermis.csv file contains two rows, each has 6048 columns, with each column is 
# the Vermis data for a pitient

# The 2nd generated file, PostHarmon_Vermis.csv, contains the following columns:
# "#,Age,Vermis,Dataset"
# and 6048 subsequent rows, with one row for a patient
# The first column "#" is a sequence number starting from 1
# the Dataset information is extracted from the "Path" field from the 
# generated gen_structname.csv file. For example the "ABIDE_I" dataset is from the
# /neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I/2NIFTI_SS_SEG_RAVENS/ Path

# This script can also plot harmonization pictures. Used command line option "-p "
# to specify how many pictures you want to paint. The default number is one.



def extractDataSet(path, filename):
  genderList_ = []
  scannerList_ = []
  datasetList_ = []
  headline_ = []
  extractedDataDir_ = {}

  with open(os.path.join(path, filename), 'r') as f:
    lines = f.readlines()
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()

    print("headline_:")
    print(headline_)
    print("------------")

    for headItem in headline_:
      extractedDataDir_[headItem] = []

    for line in lines[1:]:
      linelist = line.split(',')
      for i in range(len(headline_)):
        extractedDataDir_[headline_[i]].append(linelist[i].strip()) # use strip() to remove "\n"

    # generate genderList: Male->1; Female->2; unknown->3
    for i in range(len(extractedDataDir_['Sex'])):
      if extractedDataDir_['Sex'][i] == "M":
        genderList_.append(1)
      elif extractedDataDir_['Sex'][i] == "F":
        genderList_.append(2)
      else:
        genderList_.append(3)

    # generate Scanner Strength list: 3T->1; 1.5T->2; other->3
    for i in range(len(extractedDataDir_['Magnetic field of strength'])):
      if extractedDataDir_['Magnetic field of strength'][i] == "3T":
        scannerList_.append(1)
      elif extractedDataDir_['Magnetic field of strength'][i] == "1.5T" or \
           extractedDataDir_['Magnetic field of strength'][i] == "1.494T":
        scannerList_.append(2)
      else:
        scannerList_.append(3)

    # generate dataset list:
    for i in range(len(extractedDataDir_['Dataset'])):
      if extractedDataDir_['Dataset'][i] == "ABIDE_I":
        datasetList_.append(1)
      elif extractedDataDir_['Dataset'][i] == "BCH":
        datasetList_.append(2)
      elif extractedDataDir_['Dataset'][i] == "beijingEn":
        datasetList_.append(3)
      elif extractedDataDir_['Dataset'][i] == "BGSP":
        datasetList_.append(4)
      elif extractedDataDir_['Dataset'][i] == "DLBS":
        datasetList_.append(5)
      elif extractedDataDir_['Dataset'][i] == "IXI_600":
        datasetList_.append(6)
      elif extractedDataDir_['Dataset'][i] == "MGH":
        datasetList_.append(7)
      elif extractedDataDir_['Dataset'][i] == "NIH_PD":
        datasetList_.append(8)
      elif extractedDataDir_['Dataset'][i] == "OASIS_3":
        datasetList_.append(9)
      elif extractedDataDir_['Dataset'][i] == "ABCD":
        datasetList_.append(10)
      elif extractedDataDir_['Dataset'][i] == "NKI_Rockland":
        datasetList_.append(11)
      elif extractedDataDir_['Dataset'][i] == "MCIC":
        datasetList_.append(12)
      else:
        print("Unknow dataset: %s" % extractedDataDir_['Dataset'][i])

    return genderList_, scannerList_, datasetList_, headline_, extractedDataDir_

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./harmonDataBuilder_1.py -s . -f combined_stats.csv -p 1 -o PostHarmon_all.csv')
  parser.add_argument('-s', '--sourcedir', default='.', help='directory that contains combined_stats.csv file')
  parser.add_argument('-f', '--filename', default='combined_stats.csv', help='Harmonization meta data file')
  parser.add_argument('-o', '--output', default='PostHarmon_all.csv', help='final single output file')
  args = parser.parse_args()

  genderList0, scannerList0, datasetList0, headline, extractedDataDir = \
    extractDataSet(args.sourcedir, args.filename)


  # postHarmonCombinedData is to save all the postHarmon data into a Dict
  # then use it to generate a single combined csv file
  postHarmonCombinedData = {}

  # just like postHarmonCombinedData, but it is used to save the data read from
  # padas read_csv() call. Then that data is used to plot the graph
  postHarmonData = {}

  harmoDataList = []
  harmoColList = []
  for colname, datalist in extractedDataDir.items():
    if colname == "Dataset" or colname == "subjectId" or colname == "Age" or colname == "path" \
      or colname == "Sex" or colname == "Scanner type" or colname == "Magnetic field of strength" \
      or colname == "Volume" or colname == "Type":
      continue

    harmoColList.append(colname)
    harmoDataList.append(datalist)

  inputArr = np.array(harmoDataList, dtype=float)

  covars = {'batch':scannerList0,
            'gender':genderList0}
  #          'dataset':datasetList0}

  covars = pd.DataFrame(covars)  

  # To specify names of the variables that are categorical:
  # categorical_cols = ['gender', 'dataset']
  categorical_cols = ['gender']

  # To specify the name of the variable that encodes for the scanner/batch covariate:
  batch_col = 'batch'

  #Harmonization step:
  try:
    data_combat = neuroCombat(dat=inputArr,
        covars=covars,
        batch_col=batch_col,
        categorical_cols=categorical_cols,
        mean_only=False)["data"]
  except Exception as e:
    print("data_combat failed!!!")
    print(e)
    print("batch len: %d.... gender len: %d" % (len(covars["batch"]), len(covars["gender"])))
    print("------------------")

  for i in range(len(harmoColList)):
    postHarmonCombinedData[harmoColList[i]] = data_combat[i]

  # Create single output PostHarm file
  with open(args.output, "w") as f:
    colnamelist = postHarmonCombinedData.keys()
    f.write("#,Dataset,Age,origin_eTIV,%s\n" % (','.join(colnamelist)))
    for i in range(len(genderList0)):
      # build data list
      postDatalist = []
      for key in colnamelist:
        postDatalist.append(postHarmonCombinedData[key][i])
      # note: some data elements are number (float), so use map(str, list) to change them to string
      f.write("%d,%s,%s,%s\n" % ((i+1),\
        extractedDataDir['Dataset'][i], extractedDataDir['Age'][i],','.join(map(str, postDatalist))))

if __name__ == '__main__':
  main()
