import json
import logging
import os
import re
import sys
import argparse

# This script combines the information from Dataset.csv file and *.stats file,
# and generates per struct, such as SurfArea and GrayVol, csv file
# From the dataset file, we get the subjectID, Age, Sex, Scanner-type, magnetic field strength
# then we use the subjectID as the key to search the corresponding patient's stats file
# the subjectID resides in the directory path of the patient's stats file, for example, the 
# directory name may be IXI313-HH-2241-T1. Here IXI313 is the patient's subjectID.
# the directory name may also be in the form of deface_subjId1_subjId2_xxx, in this case
# the final subjectId is "subjId1_subjId2"

# After located the subjectID directory, we start to search the stats file within it. The status 
# file is structed as multiple columns and rows, such as

# ColHeaders StructName NumVert SurfArea GrayVol ThickAvg ThickStd MeanCurv GausCurv FoldInd CurvInd
# bankssts                                 1407    933   2328  2.631 0.397     0.085     0.015        6     0.9
# audalanteriorcingulate                   645    449   1291  2.624 0.572     0.124     0.028        9     0.9

# where each row is for a surface area like "bankssts" or "audalanteriorcingulate ", 
# and each column is for a fieldname, such as "NumVert" or "SurfArea "

# This script generates multiple .csv files, one for each field, such as "NumVert.csv" and "SurfArea.csv"
# Each row of the generated file maps to a user info field or a surfaceArea, such as 
# "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength", "bankssts", "caudalanteriorcingulate"
# Each column is a patient


def proc_file(path, name):
    #print("apar_stats_file:%s" % (os.path.join(path, name)))

    file = open(os.path.join(path, name), 'r')
    lines = file.readlines()
    headline = []
    datalines = []
    findHeadline = False

    for line in lines:
      if "#" in line:
        if "ColHeaders" in line:
          headline = line.split()
          findHeadline = True
        else:
          continue
      else:
        if not findHeadline:
          print("Failed to find headline!!!!")
          break
        else:
          datalines.append(line.split())
    # headline looks like:
    # # ColHeaders StructName NumVert SurfArea GrayVol ThickAvg ThickStd MeanCurv GausCurv FoldInd CurvInd
    # Remove "# ColHeaders StructName" 
    return headline[3:], datalines

def getStructNamesAndDatalist(datasetHeadline, patientInfo, headline, datalines):
  structNames = list(datasetHeadline)
  datalists = [[]] * len(headline)

  for statsCol in range(len(headline)):
    datalists[statsCol] = list(patientInfo)

  for row in range(len(datalines)):
    structNames.append(datalines[row][0])
    for statsColIndex in range(len(headline)):
      datalists[statsColIndex].append(datalines[row][statsColIndex+1])

  return structNames, datalists

def mergeDatalist(datalists):
  # merge datalist, like [[1.0, 2.0,3.0], [5.0, 6.0, 7.0]]
  # into [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  mergedDatalist = []
  for datalist in datalists:
    mergedDatalist += datalist

  return mergedDatalist

def proc_dataset_file(filepath):
  subjIdlist = []
  datasetlist = []

  with open(filepath) as f:
    lines = f.readlines()

    # read the headline
    headline = lines[0].split(",")
    headline = headline[2:5] + headline[8:10]

    for line in lines[1:]:
      lineList = line.split(',')
      # usefulData are subjectID, Age, Sex, Scanner-type, magnetic field strength
      usefulData = lineList[2:5] + lineList[8:10]
      subjIdItem = line.split(',')[2]
      # in case the subjId take the format like ear0-1_Q1/4574636. In this case 4574636 is the ID
      if '/' in subjIdItem:
        subjIdItem = subjIdItem.split('/',1)[1]
      subjIdlist.append(subjIdItem)
      datasetlist.append(usefulData)

    return headline, subjIdlist, datasetlist


def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python3 ./honey_real_1.py -s . -f lh.aparc.stats -d ./dataset')
  parser.add_argument('-s', '--sourcedir', default='.', help='directory that contains *.stats file')
  parser.add_argument('-f', '--filename', default='lh.aparc.stats', help='stats file name')
  parser.add_argument('-d', '--dataset', default='./Dataset.csv', help='dataset file path and name')
  args = parser.parse_args()

  firstFile = True
  firstFileheadline = []
  firstFileStructNames = []
  finalMegeredDataDic = {}
  genFilePrefix = ''

  if args.filename == 'lh.aparc.stats':
    genFilePrefix = 'lh_';
  elif args.filename == 'rh.aparc.stats':
    genFilePrefix = 'rh_';

  datasetHeadline, subjectIdlist, datasetlist = proc_dataset_file(args.dataset)
  print("datasetHeadline: ")
  print(datasetHeadline)

  sourcedirStr = args.sourcedir.replace(" ", "") # remove all blank characters
  sourcedirList = sourcedirStr.split(',')
  for sourcedir in sourcedirList:
    print("Working on %s directory" % sourcedir)
    for path, subdirs, files in os.walk(sourcedir):
      for name in files:
        if args.filename in name and name.endswith(".stats"):
          print("stats file: %s/%s" %  (path, name))

          # find the directory name that contains subjectId, which is right before the stats dir
          # like: /IXI21009-Guys-0894-T1/stats/
          try:
            dirlist = path.split('/')
            subjIdDirName = dirlist[dirlist.index('stats')-1]
          except ValueError:
            print("Cannot find the directory name  that contains subjId before stats dir")
            continue

          # print("subjIdDirName %s" % subjIdDirName)

          # get subjId from stats file path
          try:
            #subjId = 'unknown'
            if "deface" in path: #if path start with "deface", the path format is deface_subjId1_subjId2_xxx
              pathlist = subjIdDirName.split('_')
              subjId = pathlist[1]+"_"+pathlist[2]
            # for path like ./MGH/3NIFTI_FreeSurfer/HIE_122-VISIT_01-SAGMPRAGE_P2_1MM/stats/rh.aparc.stats
            # its subjId is HIE_122
            elif "VISIT_" in path and "MGH" in path: 
              pathlist = re.split('-', subjIdDirName)
              subjId = pathlist[0]
            # for path like ./BGSP/3NIFTI_FreeSurfer/Sub1008_Ses1_Scan_01_ANAT1/stats/lh.aparc.stats 
            # its subjId is Sub1008_Ses1
            elif "Ses1_Scan" in path and "BGSP" in path:
              pathlist = re.split('_', subjIdDirName)
              subjId = pathlist[0]+'_'+pathlist[1]
            elif '-' in path: # for that rest path, it is formated either like subjid-xxx-yyy or subjid_xxx_yyy
              pathlist = re.split('-|_', subjIdDirName)
              subjId = pathlist[0]
            else:
              print("Failed to find subjId for path: %" % path)
          except ValueError:
            print("cannot find subject ID from subjIdDirName: %s" % subjIdDirName)
            continue

          print("Got subjId %s from stats file path: %s" % (subjId, subjIdDirName))

          # find the index from subjectIdlist that contains subjectId
          try:
            subjIdIndex = subjectIdlist.index(subjId)
            patientInfo = datasetlist[subjIdIndex]
          except ValueError:
            print("cannot find subject ID index for subjId: %s" % subjId)
            continue  

          print("subjIdIndex: %d" % subjIdIndex)
          print("patientInfo: ")
          print(patientInfo)

          statsHeadline, datalines = proc_file(path, name)
          structNames, datalists = getStructNamesAndDatalist(datasetHeadline, patientInfo, statsHeadline, datalines)
          if firstFile:
            # print("1.........")
            # print(structNames)
            # print("2.........")
            # print(datalists)

            firstFileheadline = list(statsHeadline)
            firstFileStructNames = list(structNames)
            finalMegeredDatalist = list(datalists)
            # res = {test_keys[i]: test_values[i] for i in range(len(test_keys))}

            finalMegeredDataDic = {firstFileheadline[i]: [list(datalists[i])] for i in range(len(statsHeadline))}
            firstFile = False
            print("firstFile: %s  Headline:" % os.path.join(path, name))
            print('. '.join(statsHeadline))
            print("")
            print("firstFile structNames:")
            print('. '.join(structNames))
            print("total row number: %d " % len(structNames))
            # print("")
            # print(*datalists, sep=", ")
            print("----------------------------------------------------")
          else:
            # print("2nd file")
            # print("statsHeadline:")
            # print(statsHeadline)
            # print("structNames")
            # print(structNames)
            # print("***********")
            # print("firstFileStructNames")
            # print(firstFileStructNames)
            # print("$$$")
            if not firstFileheadline == statsHeadline:
              print("%s headline is not equal" % os.path.join(path, name))
              continue
            if not firstFileStructNames == structNames:
              print("%s structName is not equal" % os.path.join(path, name))
              continue

            for i in range(len(firstFileheadline)):
              finalMegeredDataDic[firstFileheadline[i]].append(list(datalists[i]))

  # print("Going to output files")
  # print(finalMegeredDataDic)
  print("---------------------------------------------------")
  for i in range(len(firstFileheadline)):
    print("generating %s ..." % (genFilePrefix+firstFileheadline[i]+'.csv'))
    with open(genFilePrefix+firstFileheadline[i]+'.csv', 'w') as f:
      for j in range(len(firstFileStructNames)):
        #print("Debin: %d" % len(finalMegeredDataDic[firstFileheadline[i]]))
        rowlist = [firstFileStructNames[j]]
        for k in range(len(finalMegeredDataDic[firstFileheadline[i]])):
          rowlist.append(finalMegeredDataDic[firstFileheadline[i]][k][j])
        f.write("%s\n" %(','.join(rowlist)))

if __name__ == '__main__':
  main()