#!/usr/bin/python3


from neuroCombat import neuroCombat
import pandas as pd
import numpy as np
import sys

def try_float(v):
   try:
       return float(v)
   except Exception:
       #return None
       return -1
def main(argv):
  inputFile = 'GausCurve_Input.csv'
  if len(argv) > 0:
    inputFile = argv[0] + "_Input.csv"
    print("inputFile: %s" % inputFile)

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
  main(sys.argv[1:])
