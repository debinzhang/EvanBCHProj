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
  parser = argparse.ArgumentParser(description='Example cmd:  combatTest_5.py -s . -f Harmonization_meta_data.csv -p 1')
  parser.add_argument('-s', '--sourcedir', default='.', help='directory that contains Harmonization_meta_data.csv file')
  parser.add_argument('-f', '--filename', default='Harmonization_meta_data.csv', help='Harmonization meta data file')
  parser.add_argument('-p', '--plotnum', type=int, default=1, help='how many harmonizied pictures to plot')
  args = parser.parse_args()

  genderList0, scannerList0, headline, extractedDataDir = \
    extractDataSet(args.sourcedir, args.filename)

  for i in range(len(headline)):
    colname = headline[i]
    with open ("./gen_"+colname+'.csv', 'w') as f:
      combinedStr = ''
      for key, datalist in extractedDataDir.items():
        f.write("%s" % ','.join(datalist))

  postHarmonData = {}
  for colname, datalist in extractedDataDir.items():
    # datalistCopy is used to make a duplicate row with slight modification
    datalistCopy = list(datalist)
    try:
      datalistCopy[-1] = str(float(datalistCopy[-1])+0.01)
    except ValueError:
      datalistCopy[0] = "unknown"

    if colname == "Path" or colname == "Scanner Strength" or colname == "Vendor"\
      or colname == "Gender" or colname == "Frontal Medial GM Left":
      continue

  # generate input data file, which has two duplicated rows (with 2nd row slightly modified)
    with open ("./gen_"+colname+'.csv', 'w') as f:
      f.write("%s\n" % ','.join(datalist))
      f.write("%s\n" % ','.join(datalistCopy))

    print("Working on %s" % colname)
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
      continue

    postHarmonData[colname] = pd.read_csv(postHarmonFilename)

    # plt.figure(figsize=(10,8))
    # sns.scatterplot(x=postHarmonData[colname]["Age"],y=postHarmonData[colname][colname], hue=postHarmonData[colname]["Dataset"])
    # plt.show()

  # plot
  count = 0
  for key, value in postHarmonData.items():
    if count >= args.plotnum:
      break
    print("start plotting %s...." % key)
    plt.figure(figsize=(10,8))
    sns.scatterplot(x=value["Age"],y=value[key], hue=value["Dataset"])
    count = count + 1
    plt.show()


  # data_estimates = neuroCombat(dat=data,
  #     covars=covars,
  #     batch_col=batch_col,
  #     categorical_cols=categorical_cols)["estimates"]

  # print("data_combat.estimates: ")
  # print(data_estimates)


if __name__ == '__main__':
  main()
