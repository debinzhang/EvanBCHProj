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

# The script adds age as one of the harmonization factor

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
  volumeList_ = []
  ageList_ = []
  headline_ = []
  extractedDataDir_ = {}
  #number of patients who are skipped due to missing volume info
  vol_skip_count = 0
  #number of patients who are skipped due to missing age info
  age_skip_count = 0;

  with open(os.path.join(path, filename), 'r') as f:
    lines = f.readlines()
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()

    print("headline_:")
    print(headline_)
    print("------------")

    for headItem in headline_:
      extractedDataDir_[headItem] = []

    index = 0
    for line in lines[1:]:
      linelist = line.split(',')
      #if Volume (6th column) is empty, ignore this patient
      if not linelist[6] == "":
        for i in range(len(headline_)):
          extractedDataDir_[headline_[i]].append(linelist[i].strip()) # use strip() to remove "\n"
      else:
        vol_skip_count += 1
        continue

    print("vol_skip_count: %d" % vol_skip_count)

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
      elif extractedDataDir_['Magnetic field of strength'][i] == "1.5T":
        scannerList_.append(2)
      else:
        scannerList_.append(3)

    # generate Volume list:
    for i in range(len(extractedDataDir_['Volume'])):
      if extractedDataDir_['Volume'][i] == "":
        print("Found empty volume!!!!! %d This should NOT happen!!!!" % i)
      else:
        volumeList_.append(extractedDataDir_['Volume'][i])

    # generate Age list:
    for i in range(len(extractedDataDir_['Age'])):
      if extractedDataDir_['Age'][i] == "":
        print("Found empty volume!!!!! %d This should NOT happen!!!!" % i)
        age_skip_count += 1
      else:
        ageList_.append(extractedDataDir_['Age'][i])

    print("volumeList_")
    print("volumeList_ len:%d" % len(volumeList_))
    print("genderList_ len:%d" % len(genderList_))
    print("scannerList_ len:%d" % len(scannerList_))
    print("headline_ len:%d" % len(headline_))
    print("extractedDataDir_ len: %d" % len(extractedDataDir_['Sex']))
    print("Age skip count :%d" % age_skip_count)
    print("volumeList_ done")

    return vol_skip_count, ageList_, genderList_, scannerList_, volumeList_, headline_, extractedDataDir_

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./harmonDataBuilder_1.py -s . -f combined_stats.csv -p 1 -o PostHarmon_all.csv')
  parser.add_argument('-s', '--sourcedir', default='.', help='directory that contains combined_stats.csv file')
  parser.add_argument('-f', '--filename', default='combined_stats.csv', help='Harmonization meta data file')
  parser.add_argument('-p', '--plotnum', type=int, default=1, help='how many harmonizied pictures to plot')
  parser.add_argument('-o', '--output', default='PostHarmon_all.csv', help='final single output file')
  args = parser.parse_args()

  vol_skip_count, ageList0, genderList0, scannerList0, volumeList0, headline, extractedDataDir = \
    extractDataSet(args.sourcedir, args.filename)

  if vol_skip_count > 0:
    print("%d patients are skipped due to missing volume info" % vol_skip_count)

  for i in range(len(headline)):
    colname = headline[i]
    if colname == "Dataset" or colname == "subjectId" or colname == "Age" \
      or colname == "Sex" or colname == "Scanner type" or colname == "Magnetic field of strength" \
      or colname == "Volume":
      continue
    with open ("./gen_"+colname+'.csv', 'w') as f:
      for key, datalist in extractedDataDir.items():
        f.write("%s" % ','.join(datalist))

  # postHarmonCombinedData is to save all the postHarmon data into a Dict
  # then use it to generate a single combined csv file
  postHarmonCombinedData = {}

  # just like postHarmonCombinedData, but it is used to save the data read from
  # padas read_csv() call. Then that data is used to plot the graph
  postHarmonData = {}

  for colname, datalist in extractedDataDir.items():
    colname = colname.strip()
    if colname == "Dataset" or colname == "subjectId" or colname == "Age" \
      or colname == "Sex" or colname == "Scanner type" or colname == "Magnetic field of strength" \
      or colname == "Volume":
      continue

    # datalistCopy is used to make a duplicate row with slight modification
    datalistCopy = list(datalist)
    try:
      datalistCopy[-1] = str(float(datalistCopy[-1])+0.01)
    except ValueError:
      datalistCopy[0] = "unknown"

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
              'gender':genderList0,
              'volume':volumeList0,
              'age':ageList0}

    print("scannerList_ len: %d... gen len:%d... volume len:%d ... age len:%d" % \
      (len(scannerList0), len(genderList0), len(volumeList0), len(ageList0)))

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

      postHarmonCombinedData[colname] = data_combat[0]

      postHarmonFilename = "./PostHarmon_" + colname + ".csv"

      with open(postHarmonFilename, "w") as f:
        f.write("#,Age,%s,Dataset,Volume\n" % (colname))
        for i in range(len(data_combat[0])):
            dataSet = extractedDataDir["Dataset"][i]
            volume = extractedDataDir['Volume'][i]
            rowStr = "%d,%s,%s,%s,%s" % (i+1, extractedDataDir["Age"][i], data_combat[0][i], dataSet, volume)
            f.write("%s\n" % rowStr)
    except Exception as e:
      print("data_combat failed with %s !!!" % colname)
      print(e)
      print("batch len: %d.... gender len: %d" % (len(covars["batch"]), len(covars["gender"])))
      print("------------------")
      continue

    postHarmonData[colname] = pd.read_csv(postHarmonFilename)

  # Create single output PostHarm file
  with open(args.output, "w") as f:
    colnamelist = postHarmonCombinedData.keys()
    f.write("#,Age,Dataset,Volume,%s\n" % (','.join(colnamelist)))
    for i in range(len(genderList0)):
      # build data list
      postDatalist = []
      for key in colnamelist:
        postDatalist.append(postHarmonCombinedData[key][i])
      # note: some data elements are number (float), so use map(str, list) to change them to string
      f.write("%d,%s,%s,%s,%s\n" % ((i+1),\
       extractedDataDir['Age'][i], extractedDataDir['Dataset'][i], extractedDataDir['Volume'][i], \
       ','.join(map(str, postDatalist))))

  count = 0
  for key, value in postHarmonData.items():
    if count >= args.plotnum:
      break
    print("start plotting %s...." % key)
    plt.figure(figsize=(10,8))
    sns.scatterplot(x=value["Age"],y=value[key], hue=value["Dataset"])
    count = count + 1
    plt.show()


if __name__ == '__main__':
  main()
