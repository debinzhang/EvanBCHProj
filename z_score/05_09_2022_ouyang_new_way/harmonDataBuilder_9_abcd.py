#!/usr/bin/env python3

from neuroCombat import neuroCombat
import pandas as pd
import numpy as np
import os
import re
import sys
import argparse

# This script harmonizes data from input pre-harmo file and generates post-harmo output file.
# the covars for neuroCombat are: { 'scanner magnetic strength' }
# and categorical_cols are {'sex', 'Scanner_type' }

# cmd line format: python harmonDataBuilder_8_abcd.py -f pre_harmo.csv -o post_harmo.csv

def extractDataSet(filename):
  genderList_ = []
  # scannerList is for Magnetic_field_of_strength
  scannerList_ = []
  scannerTypeList_ = []
  datasetList_ = []
  headline_ = []
  extractedDataDir_ = {}

  with open(filename, 'r') as f:
    lines = f.readlines()
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()

    for headItem in headline_:
      extractedDataDir_[headItem] = []

    for line in lines[1:]:
      linelist = line.split(',')
      for i in range(len(headline_)):
        extractedDataDir_[headline_[i]].append(linelist[i].strip()) # use strip() to remove "\n"

    # generate genderList: Male->1; Female->2; unknown->3
    # for ABCD dataset, sex directly shows as number: Male->1; Female->2; unknown->3
    for i in range(len(extractedDataDir_['Sex'])):
      if extractedDataDir_['Sex'][i] == "M" or extractedDataDir_['Sex'][i] == '1':
        genderList_.append(1)
      elif extractedDataDir_['Sex'][i] == "F" or extractedDataDir_['Sex'][i] == '2':
        genderList_.append(2)
      else:
        #print("for i=%d sex is:%s" % (i, extractedDataDir_['Sex'][i]))
        genderList_.append(3)

    # generate Scanner Strength list: 1.5T->1; 3T->2; other->3
    # for ABCD dataset, strength shows as number
    for i in range(len(extractedDataDir_['Magnetic_field_of_strength'])):
      if extractedDataDir_['Magnetic_field_of_strength'][i] == "1.5T" or \
           extractedDataDir_['Magnetic_field_of_strength'][i] == "1.494T" or \
           extractedDataDir_['Magnetic_field_of_strength'][i] == '1':
        scannerList_.append(1)
      elif extractedDataDir_['Magnetic_field_of_strength'][i] == "3T" or \
        extractedDataDir_['Magnetic_field_of_strength'][i] == '2':
        scannerList_.append(2)
      else:
        #print("for i=%d strength is: %s" % (i, extractedDataDir_['Magnetic_field_of_strength'][i]))
        scannerList_.append(3)

    # generate Scanner_type list: Siemens=1, GE=2, Philips=3, Blank=4
    # for ABCD dataset, Scanner_type shows as number
    for i in range(len(extractedDataDir_['Scanner_type'])):
      if extractedDataDir_['Scanner_type'][i] == 'Siemens' or \
        extractedDataDir_['Scanner_type'][i] == '1':
        scannerTypeList_.append(1)
      elif extractedDataDir_['Scanner_type'][i] == 'GE' or \
        extractedDataDir_['Scanner_type'][i] == '2':
        scannerTypeList_.append(2)
      elif extractedDataDir_['Scanner_type'][i] == 'Philips' or \
        extractedDataDir_['Scanner_type'][i] == '3':
        scannerTypeList_.append(3)
      else:
        #print("for i=%d Scanner_type:%s" % (i, extractedDataDir_['Scanner_type'][i]))
        scannerTypeList_.append(4)

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

    return genderList_, scannerList_, scannerTypeList_, datasetList_, headline_, extractedDataDir_

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./harmonDataBuilder_1.py -f combined_stats.csv -o PostHarmon_all.csv')
  parser.add_argument('-f', '--filename', default='combined_stats.csv', help='Harmonization meta data file')
  parser.add_argument('-o', '--output', default='PostHarmon_all.csv', help='final single output file')
  args = parser.parse_args()

  genderList0, scannerList0, scannerTypeList0, datasetList0, headline, extractedDataDir = \
    extractDataSet(args.filename)

  # postHarmonCombinedData is to save all the postHarmon data into a Dict
  postHarmonCombinedData = {}

  harmoDataList = []
  harmoColList = []
  for colname, datalist in extractedDataDir.items():
    if colname == "Dataset" or colname == "subjectId" or colname == "Age" or colname == "path" \
      or colname == "Sex" or colname == "Scanner_type" or colname == "Magnetic_field_of_strength" \
      or colname == "Volume" or colname == "Type" or colname == "zscore":
      continue

    harmoColList.append(colname)
    harmoDataList.append(datalist)

  inputArr = np.array(harmoDataList, dtype=float)

  covars =  {
              'batch': scannerList0,
              'gender': genderList0,
              #'scannerType': scannerTypeList0
              # 'dataset': datasetList0
            }

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
    f.write("#,Dataset,Age,Sex,%s\n" % (','.join(colnamelist)))
    for i in range(len(genderList0)):
      # build data list
      postDatalist = []
      for key in colnamelist:
        postDatalist.append(postHarmonCombinedData[key][i])
      # note: some data elements are number (float), so use map(str, list) to change them to string
      f.write("%d,%s,%s,%s,%s\n" % ((i+1),\
        extractedDataDir['Dataset'][i], extractedDataDir['Age'][i], extractedDataDir['Sex'][i], ','.join(map(str, postDatalist))))

  print("Harmonization done: total %d records have been processed." % (len(genderList0)))

if __name__ == '__main__':
  main()
