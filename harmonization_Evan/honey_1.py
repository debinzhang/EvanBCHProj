import json
import logging
import os
import re
import sys
import argparse
import pprint

# This script combines the information from Dataset.csv file and *.stats file, and saves the data in a single 
# combined .csv file. The script works in the following steps:
# 1. Search all the directories from sourceDir list that users input from commandline for 
#    the stats file that users specified from cmdline, such as lh.aparc.stats
# 2. Once such directory is found, get the patient's subjectId from the directory path
# 3. Use the patient's subjectId as a key to search the dataset.csv file; If an entry is found
#    get the patient's ["Dataset", "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength"
#    info from the dataset.csv
# 4. From the patient's stats file (found at step 1), get the surfaceArea list, such as bankssts and caudalanteriorcingulate,
#    and structName list, such as NumVert and SurfArea, and their corresponding data (it is a two dimention array)
# 5. Combined patient's dataset data from step 3 and stats data from step, output into the fianl combined csv file
#    in the final csv file, the first row looks like:
#    "Dataset", "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength", "bankssts.NumVert", 
#    bankssts.SurfArea", ... "insula.NumVert", "insula.SurfArea"
#    and the rest of rows are corresponding data
#    Each row is for a patient

def proc_stats_file(path, name):
    #print("apar_stats_file:%s" % (os.path.join(path, name)))

    file = open(os.path.join(path, name), 'r')
    lines = file.readlines()
    headline = []
    findHeadline = False

    # Data from the stats file is saved in statsDict, where the first column name, which is surface
    # area is the key, and the rest line, which is saved as a list, is the dict value
    statsDict = {}

    for line in lines:
      if "#" in line:
        if "ColHeaders" in line:
          # headline is in the format of: 
          #['#', 'ColHeaders', 'StructName', 'NumVert', 'SurfArea', 'GrayVol', 'ThickAvg', 'ThickStd', 'MeanCurv', 'GausCurv', 'FoldInd', 'CurvInd']
          # we only need the [NumVert', 'SurfArea', 'GrayVol', 'ThickAvg', 'ThickStd', 'MeanCurv', 'GausCurv', 'FoldInd', 'CurvInd']
          headline = line.split()[3:]
          findHeadline = True
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

    return headline, statsDict

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
  parser.add_argument('-o', '--output', default='./combined_stats.csv', help="final combined output file")
  args = parser.parse_args()

  firstFile = True
  firstFileheadline = []
  # the first row of the final output dataset. Should be in the format like:
  # ["Dataset", "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength", "bankssts.NumVert", 
  # "bankssts.SurfArea", ... "insula.NumVert", "insula.SurfArea"]
  # the first 6 column should match patientInfo list
  outputheadline = ["Dataset", "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength"]

  # this is the huge final output data list. Each list item maps to a row in the final output data file
  # and it is for one patient
  # Each row should match the above outputheadline. And the list should looks like this:
  # [ [ 'OASIS_3', 'OAS30502_d395', '52', 'F', 'SiemensTrioTim', '3T', '1367', '941', '2450', ...],
  #   ...,
  #   ['Huaxi', '28316', '27', 'F', '', '', '1672', '1145', '3203', ...] ]
  outputDatalist = []

  # firstFileStructNames = []
  firstFileSurfaceAreas = []
  # finalMegeredDataDic = {}
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
              # patientInfo is in the format: ['Dataset', 'subjectId', 'Age', 'Sex', 'Scanner type', 'Magnetic Field Strength']
              patientInfo = datasetlist[subjIdIndex]
            except ValueError:
              print("cannot find subject ID index for subjId: %s" % subjId)
              continue  

            print("subjIdIndex: %d" % subjIdIndex)
            print("patientInfo: %s\n" % patientInfo)

            # headline: [NumVert', 'SurfArea', 'GrayVol', 'ThickAvg', 'ThickStd', 'MeanCurv', 'GausCurv', 'FoldInd', 'CurvInd']
            # statsDic: dict with each surfArea as key; and data list, corresponding to headline, as value
            headline, statsDict = proc_stats_file(path, name)
            if firstFile:
              print("headline: %s" % headline)
              firstFileheadline = headline
              firstFileSurfaceAreas = statsDict.keys()
              print("Firstfile headlist length: %d... surf area number: %d\n" % (len(firstFileheadline), len(firstFileSurfaceAreas)))
              print(firstFileSurfaceAreas)
              print(outputheadline)
              for surfArea in firstFileSurfaceAreas:
                for headItem in firstFileheadline:
                  outputheadline.append(surfArea+'.'+headItem)

              firstFile = False
            else:
              # check if the stats file is in the correct format
              # if the stats file is not in the same format as the firstFile, we discard it.
              if not headline == firstFileheadline:
                print("stats file %s/%s has incompatable headline [%s]. Skip this patient" % (path, name, ', '.join(headline)))
                continue
              # else:
              #   print("headline is the same: %d" % len(headline))

              if not statsDict.keys() == firstFileSurfaceAreas:
                print("stats file %s/%s has incompatable surfArea list [%s]. Skip this patient" % (path, name, ', '.join(statsDict.keys())))
                continue
              # else:
              #   print("surf areas are the same: with number: %d" % len(statsDict.keys()))

              # passed stats file format checking. Now start to grab data from it
              # and attach data onto the outputDatalist

            # first get the ["Dataset", "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength"] info
            patientDatalist = list(patientInfo)
            for surfArea in statsDict.keys():
              for rowIndex in range(len(firstFileheadline)):
                patientDatalist.append(statsDict[surfArea][rowIndex])

            # done with this patient. Attach its data to final outputDatalist
            patientCount += 1
            outputDatalist.append(patientDatalist)


            # print("statsDict:")
            # pp = pprint.PrettyPrinter(indent=4)
            # pp.pprint(statsDict)

    print("For %s directory, %d patients info found" % (sourcedir, patientCount))
    totalPatientCount += patientCount
    print("--------------------------------\n")

  print("Total patient count %d\n" % totalPatientCount)

  # output the final combined data sheet
  with open(args.output, 'w') as f:
    # output the headline
    f.write("%s\n" % (','.join(outputheadline)))
    # output the data rows
    for row in outputDatalist:
      f.write("%s\n" % (','.join(row)))


if __name__ == '__main__':
  main()