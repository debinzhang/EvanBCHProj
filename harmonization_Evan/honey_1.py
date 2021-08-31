import json
import logging
import os
import re
import sys
import argparse
import pprint

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


def proc_stats_file(path, name):
    #print("apar_stats_file:%s" % (os.path.join(path, name)))

    file = open(os.path.join(path, name), 'r')
    lines = file.readlines()
    headline = []
    findHeadline = False

    # 
    statsDict = {}

    for line in lines:
      if "#" in line:
        if "ColHeaders" in line:
          headline = line.split()
          findHeadline = True
          # headline looks like:
          # # ColHeaders StructName NumVert SurfArea GrayVol ThickAvg ThickStd MeanCurv GausCurv FoldInd CurvInd
          # Remove "# ColHeaders StructName"
          statsDict["headlist"] = headline[3:]
        else:
          continue
      else:
        if not findHeadline:
          print("Failed to find headline!!!!")
          break
        else:
          linelist = line.split()
          # OAS1 patient stats file contains a row started with "unknown", remove it
          if linelist[0] == "unknown":
            continue
          statsDict[linelist[0]] = linelist[1:]

    return statsDict

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
    # headline should be ['Dataset', 'subjectID', 'Age', 'Sex', 'Scanner type', 'Magnetic field of strength']
    headline = ['Dataset'] + headline[2:5] + headline[8:10]

    for line in lines[1:]:
      lineList = line.split(',')
      # usefulData are [dataset, subjectID, Age, Sex, Scanner-type, magnetic field strength]
      usefulData = [lineList[0]] + lineList[2:5] + lineList[8:10]
      # Age may be in months, or in days. Change them to years
      if 'm' in usefulData[2]: # age in format like: '113m'
        if usefulData[2] == 'm': # special case, 0 month is represented as 'm'
          usefulData[2] = 0
        else:
          usefulData[2] = str(float(usefulData[2].strip('m'))/12)
      elif 'd' in usefulData[2]: # age in fomat like: '3803d'
        if usefulData[2] == 'd': # special case, 0 day is represented as 'd'
          usefulData[2] = '0'
        else:
          usefulData[2] = str(float(usefulData[2].strip('d'))/365)

      subjIdItem = line.split(',')[2]
      # in case the subjId take the format like ear0-1_Q1/4574636. In this case 4574636 is the ID
      if '/' in subjIdItem:
        subjIdItem = subjIdItem.split('/',1)[1]
      subjIdlist.append(subjIdItem)
      datasetlist.append(usefulData)

    return headline, subjIdlist, datasetlist

def getOas1AgeGender(path):
  age = '-1'
  gender = "unknown"
  print("Oas1 path: %s" % path)
  # path should be in the format like: ./OAS1_0001_MR1/freesurfer_results/stats
  pathList = path.split('/')
  # remove last two levels from the list
  pathList = pathList[:len(pathList)-2]
  # get the .txt file that contains age and gender should be like: ./OAS1_0001_MR1/OAS1_0001_MR1.txt
  pathList.append(pathList[-1]+'.txt')
  txtPath = '/'.join(pathList)
  print("newPath: %s" % txtPath)

  with open(txtPath, 'r') as f:
    lines = f.readlines()
    for line in lines:
      # Age line is in format: "AGE:          74"
      if "AGE:" in line:
        age = line.strip().replace(" ","").split(':')[1]
      # gender line is in format: "M/F:          Female"
      if "M/F:" in line:
        rawGender = line.strip().replace(" ","").split(':')[1]
        gender = 'F' if rawGender=="Female" else 'M'

  return age, gender


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
  totalPatientCount = 0
  for sourcedir in sourcedirList:
    print("Working on %s directory" % sourcedir)
    patientCount  = 0
    for path, subdirs, files in os.walk(sourcedir):
      for name in files:
        forOas1Patient = False
        oas1_age = "-1"
        oas1_gender = "unknown"
        if args.filename in name and name.endswith(".stats"):
          print("stats file: %s/%s" %  (path, name))

          # for stats data that comes from OSS1_xxxx_MR1 dir, patient's Age, gender info 
          # comes from its attached .txt file
          if 'OAS1_' in path:
            oas1_age, oas1_gender = getOas1AgeGender(path)
            print("age: %s... gender:%s" % (oas1_age, oas1_gender))
            if oas1_age == '-1' or oas1_gender == 'unknown':
              continue
            forOas1Patient = True
          else:
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
          # for OAS1 patients, we do not depend on subjId to get age and gender info.
          # we should get that info from the above steps already.
          if forOas1Patient:
            # patientInfo contains [subjId, age, gender, "scanner type", "Magnetic field of strength"]
            # for Oas1 patients, they do not have subjId, scanner type, and Magnetic field of strength info
            patientInfo = ["Oas1_subjId", oas1_age, oas1_gender, '', '']
          else:
            try:
              # for path like NIH_PD/3NIFTI_FreeSurfer/deface_1268_v3_t1w/stats/lh.aparc.stats
              # its subjId is 1268_v3
              if "deface" in path: #if path start with "deface", the path format is deface_subjId1_subjId2_xxx
                pathlist = subjIdDirName.split('_')
                subjId = pathlist[1]+"_"+pathlist[2]
              elif "ABIDE_I" in path:
              # for path like ./ABIDE_I/3NIFTI_FreeSurfer/ABIDE51556_MPRAGE/stats/lh.aparc.stats
              # its subjId is 51556
                subjId = subjIdDirName.replace("ABIDE","").replace("_MPRAGE","")

              # for path like ./beijingEn/3NIFTI_FreeSurfer/BJ4488696_MPRAGE/stats/lh.aparc.stats
              # its subjId is 4488696
              elif "beijingEn" in path:
                subjId = subjIdDirName.replace("BJ","").replace("_MPRAGE","")
                print("Debin !!!! subjId %s" % subjId)

              # for path like ./DLBS/3NIFTI_FreeSurfer/0028638-session_1-anat/stats/rh.w-g.pct.stats
              # its subjId is 28638
              elif  "DLBS" in path:
                subjId = subjIdDirName.split('-')[0].lstrip('0')

              # for path like ./Huaxi/download/0028316/stats/lh.aparc.stats
              # its subjId is 28316
              elif 'Huaxi/download' in path:
                subjId = subjIdDirName.split('-')[0].lstrip('0')

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

              elif "OASIS_3" in path:
                if "sub-" in path and "ses-" in path:
                  # for path like ./OASIS_3/3NIFTI_FreeSurfer/sub-OAS31136_ses-d0156_T1w/stats/lh.aparc.stats
                  # its subjId is OAS31136_d156
                  pathlist = re.split('-|_', subjIdDirName)
                  num = pathlist[3].lstrip('d').lstrip('0')
                  subjId = pathlist[1]+"_d"+num
                elif "downloaded" in path and "_MR_" in path:
                  # for path like ./OASIS_3/downloaded/OAS30502_MR_d0395/ASSESSORS/OAS30502_Freesurfer53_d0395/out/DATA/OAS30502_MR_d0395/stats/
                  # its subjId is OAS30502_d395
                  pathlist = re.split('-|_', subjIdDirName)
                  num = pathlist[2].lstrip('d').lstrip('0')
                  subjId = pathlist[0]+"_d"+num

              elif '-' in path or '_' in path: # for that rest path, it is formated either like subjid-xxx-yyy or subjid_xxx_yyy
                # this should cover BCH case, in which dirname like 4634156-MPRAGE_rMPRAGE, where 4634156 is id
                # also covers HCPdevelopment case, in which dirname like HCD2751964_V1_MR, where HCD2751964_V1_MR is id
                pathlist = re.split('-|_', subjIdDirName)
                subjId = pathlist[0]
              else:
                print("Failed to find subjId for path: %" % path)
            except ValueError:
              print("cannot find subject ID from subjIdDirName: %s" % (subjIdDirName))
              continue

            print("Got subjId %s from stats file path: %s" % (subjId, subjIdDirName))

            # find the index from subjectIdlist that contains subjectId
            try:
              subjIdIndex = subjectIdlist.index(subjId)
              patientInfo = datasetlist[subjIdIndex]
            except ValueError:
              print("cannot find subject ID index for subjId: %s" % subjId)
              continue  

            patientCount += 1
            print("subjIdIndex: %d" % subjIdIndex)
            print("patientInfo: ")
            print(patientInfo)
            print("")

    print("For %s directory, %d patients info found" % (sourcedir, patientCount))
    totalPatientCount += patientCount
    print("--------------------------------\n")

  print("Total patient count %d\n" % totalPatientCount)

          # statsDict = proc_stats_file(path, name)
          # print("statsDict:")
          # pp = pprint.PrettyPrinter(indent=4)
          # pp.pprint(statsDict)
          # return

  #         structNames, datalists = getStructNamesAndDatalist(datasetHeadline, patientInfo, statsHeadline, datalines)
  #         if firstFile:
  #           # print("1.........")
  #           # print(structNames)
  #           # print("2.........")
  #           # print(datalists)

  #           firstFileheadline = list(statsHeadline)
  #           firstFileStructNames = list(structNames)
  #           finalMegeredDatalist = list(datalists)
  #           # res = {test_keys[i]: test_values[i] for i in range(len(test_keys))}

  #           finalMegeredDataDic = {firstFileheadline[i]: [list(datalists[i])] for i in range(len(statsHeadline))}
  #           firstFile = False
  #           print("firstFile: %s  Headline:" % os.path.join(path, name))
  #           print('. '.join(statsHeadline))
  #           print("")
  #           print("firstFile structNames:")
  #           print('. '.join(structNames))
  #           print("total row number: %d " % len(structNames))
  #           # print("")
  #           # print(*datalists, sep=", ")
  #           print("----------------------------------------------------")
  #         else:
  #           # print("2nd file")
  #           # print("statsHeadline:")
  #           # print(statsHeadline)
  #           # print("structNames")
  #           # print(structNames)
  #           # print("***********")
  #           # print("firstFileStructNames")
  #           # print(firstFileStructNames)
  #           # print("$$$")
  #           if not firstFileheadline == statsHeadline:
  #             print("%s headline is not equal" % os.path.join(path, name))
  #             continue
  #           if not firstFileStructNames == structNames:
  #             print("%s structName is not equal" % os.path.join(path, name))
  #             continue

  #           for i in range(len(firstFileheadline)):
  #             finalMegeredDataDic[firstFileheadline[i]].append(list(datalists[i]))

  # # print("Going to output files")
  # # print(finalMegeredDataDic)
  # print("---------------------------------------------------")
  # for i in range(len(firstFileheadline)):
  #   print("generating %s ..." % (genFilePrefix+firstFileheadline[i]+'.csv'))
  #   with open(genFilePrefix+firstFileheadline[i]+'.csv', 'w') as f:
  #     for j in range(len(firstFileStructNames)):
  #       #print("Debin: %d" % len(finalMegeredDataDic[firstFileheadline[i]]))
  #       rowlist = [firstFileStructNames[j]]
  #       for k in range(len(finalMegeredDataDic[firstFileheadline[i]])):
  #         rowlist.append(finalMegeredDataDic[firstFileheadline[i]][k][j])
  #       f.write("%s\n" %(','.join(rowlist)))

if __name__ == '__main__':
  main()