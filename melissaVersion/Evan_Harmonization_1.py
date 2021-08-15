#!/usr/bin/python3


from neuroCombat import neuroCombat
import pandas as pd
import numpy as np
import sys
import argparse


# This script takes a single fieldname_Input.csv file, such as GausCurve_Input.csv, 
# to generate gen_fieldname_Input.csv, such as gen_GausCurve_Input.csv

# Let's use the GausCurve_Input.csv file as an example to explain how the script works:
# Each column of GausCurve_Input.csv file is for a patient
# Each row of the GausCurve_Input.csv file is is for a struct name, such as "bankssts",
# or "caudalanteriorcingulate". Please see the lh.aparc.stats file as an example
# In the sample GausCurve_Input.csv file, it contains data for 211 patients and 35 struct
# names. So it has 35 rows and 212 columns

# what this script does is to flip this file so that each row is for a patient, and 
# each column is for a struct name. So the generated gen_GausCurve_Input.csv file
# contains 211 rows and 35 column

# This script also requires the following .txt files, which are used by the neuroCombat
# program to generate index lists:
# Evan_Scanner.txt, Evan_Gender.txt, Evan_Vendor.txt, Evan_Age.txt


def try_float(v):
   try:
       return float(v)
   except Exception:
       #return None
       return -1

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  Evan_Harmonization_1.py -s GausCurve')
  parser.add_argument('-s', '--structname', default='GausCurve', help='structname, such as GausCurve or ThickAvg')
  args = parser.parse_args()

  inputFile = args.structname + "_Input.csv"

  # Getting example data
  # 200 rows (features) and 10 columns (scans)
  data = np.genfromtxt(inputFile, delimiter=",", skip_header=1)
  print(len(data))
  strengthStr = open('Evan_Scanner.txt', 'r').read()
  strengthList = [x.strip() for x in strengthStr.split(',')]
  print(len(strengthList))
  genderStr = open('Evan_Gender.txt', 'r').read()
  genderList = [x.strip() for x in genderStr.split(',')]
  print(len(genderList))
  vendorStr = open('Evan_Vendor.txt', 'r').read()
  vendorList = [x.strip() for x in vendorStr.split(',')]
  print(len(vendorList))
  ageStr = open('Evan_Age.txt', 'r').read()
  #print(ageStr)

  ageListTmp = [x.strip() for x in ageStr.split(',')]
  #print(ageListTmp)
  #for i in range(len(ageList)): ageList[i]=float(ageList[i])
  ageList = [try_float(item) for item in ageListTmp]
  #print(ageList)

  # Specifying the batch (scanner variable) as well as a biological covariate to preserve:
  covars = {'strength':strengthList,
            'gender':genderList,
            'vendor':vendorList,
            'age':ageList}
  covars = pd.DataFrame(covars)

  # To specify names of the variables that are categorical:
  categorical_cols = ['gender','strength']

  # To specify the name of the variable that encodes for the scanner/batch covariate:
  batch_col = 'vendor'

  # To specify the name of the variable that encodes for the biological covariate:
  continuous_cols = ['age']

  #Harmonization step:
  data_combat = neuroCombat(dat=data,
      covars=covars,
      batch_col=batch_col,
      categorical_cols=categorical_cols,
      continuous_cols=continuous_cols)

  with open("gen_"+inputFile, "w") as f:
    for i in range(len(data_combat["data"][0])):
      rowlist = []
      for j in range(len(data_combat["data"])):
        rowlist.append(str(data_combat["data"][j][i]))
      f.write("%s\n" % ','.join(rowlist))


if __name__ == '__main__':
  main()
