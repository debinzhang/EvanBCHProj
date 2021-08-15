#!/usr/bin/python3


from neuroCombat import neuroCombat
import pandas as pd
import numpy as np
import os
import re
import sys


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

def main(argv):
  root_dir = '.'
  if len(argv) > 0:
    root_dir = argv[0]

  genderList0, scannerList0, headline, extractedDataDir = \
    extractDataSet(root_dir, 'Harmonization_meta_data.csv')

  for i in range(len(headline)):
    colname = headline[i]
    with open ("./gen_"+colname+'.csv', 'w') as f:
      combinedStr = ''
      for key, datalist in extractedDataDir.items():
        f.write("%s" % ','.join(datalist))


  for colname, datalist in extractedDataDir.items():
    # datalistCopy is used to make a duplicate row with slight modification
    print("Total there are %d patients!!! " % len(datalist))
    datalistCopy = list(datalist)
    try:
      datalistCopy[-1] = str(float(datalistCopy[-1])+0.01)
    except ValueError:
      datalistCopy[0] = "unknown"

    with open ("./gen_"+colname+'.csv', 'w') as f:
      f.write("%s\n" % ','.join(datalist))
      f.write("%s\n" % ','.join(datalistCopy))


  # Getting example data
  # 200 rows (features) and 10 columns (scans)
  #data = np.genfromtxt('./HarmonizationInputMelissa.csv', delimiter=",",skip_header = 0)
  data = np.genfromtxt('./gen_ICV.csv', delimiter=",",skip_header = 0)

  covars = {'batch':scannerList0,
            'gender':genderList0}

  covars = pd.DataFrame(covars)  

  # To specify names of the variables that are categorical:
  categorical_cols = ['gender']

  # To specify the name of the variable that encodes for the scanner/batch covariate:
  batch_col = 'batch'

  # To specify FOS of Scanner
  # FOS_col = ['FOS']

  #Harmonization step:
  data_combat = neuroCombat(dat=data,
      covars=covars,
      batch_col=batch_col,
      categorical_cols=categorical_cols)["data"]

  print(data_combat[0])
  with open("./PostHarmonizationBrainVolume.csv", "w") as f:
      for i in range(len(data_combat[0])):
          f.write("%f\n" % data_combat[0][i])

  print(" -------------------------------")

  # data_estimates = neuroCombat(dat=data,
  #     covars=covars,
  #     batch_col=batch_col,
  #     categorical_cols=categorical_cols)["estimates"]

  # print("data_combat.estimates: ")
  # print(data_estimates)


if __name__ == '__main__':
  main(sys.argv[1:])
