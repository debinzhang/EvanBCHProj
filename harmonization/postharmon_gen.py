#!/usr/bin/python3

from neuroCombat import neuroCombat
import pandas as pd
import numpy as np
import os
import re
import sys
import argparse

import matplotlib.pyplot as plt 
import seaborn as sns
from scipy.optimize import curve_fit

# This script generates a single PostHarmon_xxx.csv file
# The script assume the corresponding gen_xxx.csv had been created.
# So it needs to be run after running combatTest_5.py script, which generates gen_xxx.csv

def extractDataSet(path, filename):
  genderList_ = []
  scannerList_ = []
  headline_ = []
  extractedDataDir_ = {}

  with open(os.path.join(path, filename), 'r') as f:
    lines = f.readlines()
    #headline_ = (lines[0]).encode("ascii", "ignore").split(',')
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()
    try:
      genderIndex = headline_.index('Gender')
      scannerStrengthIndex = headline_.index('Scanner Strength')
      print("genderIndex: %d" % genderIndex)
      print("scannerStrengthIndex: %d" % scannerStrengthIndex)
    except ValueError:
      print("Cannot find gender or scanner Strength column")
      return

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
    for i in range(len(extractedDataDir_['Gender'])):
      if extractedDataDir_['Gender'][i] == "M":
        genderList_.append(1)
      elif extractedDataDir_['Gender'][i] == "F":
        genderList_.append(2)
      else:
        genderList_.append(3)

    # generate Scanner Strength list: 3T->1; 1.5T->2; other->3
    for i in range(len(extractedDataDir_['Scanner Strength'])):
      if extractedDataDir_['Gender'][i] == "3T":
        scannerList_.append(1)
      elif extractedDataDir_['Gender'][i] == "1.5T":
        scannerList_.append(2)
      else:
        scannerList_.append(3)

    return genderList_, scannerList_, headline_, extractedDataDir_

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  postharmon_gen.py -s . -f Harmonization_meta_data.csv -n ICV')
  parser.add_argument('-s', '--sourcedir', default='.', help='directory that contains Harmonization_meta_data.csv file')
  parser.add_argument('-f', '--filename', default='Harmonization_meta_data.csv', help='Harmonization meta data file')
  parser.add_argument('-n', '--fieldname', default='ICV', help="fieldname such as ICV")
  args = parser.parse_args()

  genderList0, scannerList0, headline, extractedDataDir = \
      extractDataSet(args.sourcedir, args.filename)
  # colname = 'Occipital WM Left'
  colname = args.fieldname

  data = np.genfromtxt('./gen_'+ colname +'.csv', delimiter=",",skip_header = 0)

  # Getting example data
  # 200 rows (features) and 10 columns (scans)
  #data = np.genfromtxt('./HarmonizationInputMelissa.csv', delimiter=",",skip_header = 0)
  # data = np.genfromtxt('./gen_ICV.csv', delimiter=",",skip_header = 0)

  covars = {'batch':scannerList0,
           'gender':genderList0}

  covars = pd.DataFrame(covars)  

  # To specify names of the variables that are categorical:
  categorical_cols = ['gender']

  # To specify the name of the variable that encodes for the scanner/batch covariate:
  batch_col = 'batch'

  #Harmonization step:
  try:
    data_combat = neuroCombat(dat=data,
        covars=covars,
        batch_col=batch_col,
        categorical_cols=categorical_cols)["data"]

    postHarmonFilename = "./PostHarmon_" + colname + ".csv"
    with open(postHarmonFilename, "w") as f:
      f.write("#,Age,%s,Dataset\n" % (colname))
      for i in range(len(data_combat[0])):
          dataSet = extractedDataDir["Path"][i].split('/')[6]
          rowStr = "%d,%s,%s,%s" % (i+1, extractedDataDir["Age"][i], data_combat[0][i], dataSet)
          f.write("%s\n" % rowStr)
  except Exception as e:
        print("data_combat failed with %s !!!" % colname)
        print(e)
        print("batch len: %d.... gender len: %d" % (len(covars["batch"]), len(covars["gender"])))
        print("------------------")

if __name__ == '__main__':
  main()
