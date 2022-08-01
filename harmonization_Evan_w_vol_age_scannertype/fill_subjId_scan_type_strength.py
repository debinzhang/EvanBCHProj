import os
import re
import sys
import argparse

def getSubjIdFromPath(dataset, path):
  if dataset == "ABIDE_I":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I/2NIFTI_SS_SEG_RAVENS/ABIDE50030/ABIDE50030_MPRAGE_ss.nii.gz"
    result = re.search('(.*)_RAVENS/ABIDE([0-9]*)/(.*)', path)
    # subjId = '50030'
    subjId = result.group(2)
  elif dataset == "BCH":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/BCH/NIFTI_NORMAL_2_MulModReg_SS/Year4-5_Q3/4438380/4438380-MPRAGE_rMPRAGE_ss.nii.gz"
    result = re.search('(.*)Year(.*)/([0-9]*)/(.*)', path)
    # subjId = "Year4-5_Q3/4438380"
    subjId = "Year" + result.group(2)+ "/" + result.group(3)
  elif dataset == "beijingEn":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/beijingEn/2NIFTI_SS_REG_RAVENS/BJ3905035/BJ3905035_MPRAGE_ss.nii.gz"
    result = re.search('(.*)/BJ([0-9]*)/BJ(.*)', path)
    # subjId = '3905035'
    subjId = result.group(2)
  elif dataset == "BGSP":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/BGSP/2NIFTI_SS_REG_RAVENS/Sub0004_Ses1_Scan_01_ANAT1/Sub0004_Ses1_Scan_01_ANAT1_ss.nii.gz"
    result = re.search('(.*)/Sub([0-9]*)_Ses1_Scan_(.*)', path)
    # subjId = 'Sub0004_Ses1'
    subjId = "Sub" + result.group(2) + "_Ses1"
  elif dataset == "DLBS":
    # path ="/neuro/labs/grantlab/research/MRI_Predict_Age/DLBS/2NIFTI_SS_REG_RAVENS/0028376/0028376-session_1-anat_ss.nii.gz"
    result = re.search('(.*)_RAVENS/(0*)([0-9]*)/(.*)', path)
    # subjId = 28376
    subjId = result.group(3)
  elif dataset == "IXI_600":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/IXI_600/2NIFTI_SS_REG_RAVENS/IXI461-Guys-0998-T1/IXI461-Guys-0998-T1_ss.nii.gz"
    result = re.search('(.*)_RAVENS/IXI([0-9]*)-(.*)', path)
    # subjId = 'IXI461'
    subjId = "IXI" + result.group(2)
  elif dataset == "MGH":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/MGH/2NIFTI_SS_REG_RAVENS/HIE_165/VISIT_01/HIE_165-VISIT_01-SAGSPGRPRE_ss.nii.gz"
    result = re.search('(.*)_RAVENS/HIE_([0-9]*)/(.*)', path)
    # subjId = 'HIE_165'
    subjId = "HIE_" + result.group(2)
  elif dataset == "NIH_PD":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/NIH_PD/2NIFTI_SS_REG_RAVENS/deface_1020_v1_t1w/deface_1020_v1_t1w_ss.nii.gz"
    result = re.search('(.*)_RAVENS/deface_([0-9]*)_v([0-9]+)_(.*)', path)
    # subjId = '1020_v1'
    subjId = result.group(2) + "_v" + result.group(3)
  elif dataset == "OASIS_3":
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/OASIS_3/2NIFTI_SS_REG_RAVENS/sub-OAS30066_ses-d0524_T1w/sub-OAS30066_ses-d0524_T1w_ss.nii.gz"
    result = re.search('(.*)_RAVENS/sub-OAS([0-9]+)_ses-d(0*)([0-9]+)_(.*)', path)
    # subjId = 'OAS30066_d524'
    subjId = "OAS" + result.group(2) + "_d" + result.group(4)
  else:
    print("unrecognized path: " + path)

  return subjId

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./fill_subjid_scan_type_strength.py -s ./source_data.csv -b ./output_w_subjId.csv \
   -i ./lh_combined_stats_07242022_1123pm_w_volume_filled.csv \
   -o ./stats_w_scanner_info.csv')
  parser.add_argument('-s', '--sourcefile', default='./source_data.csv', help='source data file that contains path and scanner info')
  parser.add_argument('-b', '--subjIdfile', default='./output_w_subjId.csv', help='output file that adds subjectId to source_data file')
  parser.add_argument('-i', '--inputfile', default='', help='file that misses scanner info')
  parser.add_argument('-o', '--outputfile', default='stats_w_scanner_info.csv', help='output stats file with scanner info filled')
  args = parser.parse_args()

  headline_ = []
  extractedDataDict_ = {}
  match_count = 0

  with open(args.sourcefile, 'r') as srcf:
    lines = srcf.readlines()
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()
    # add subjectId column in headerline
    headline_.append("subjectId")

    for headItem in headline_:
      extractedDataDict_[headItem] = []

    for line in lines[1:]:
      linelist = line.split(',')
      # copy the sourcefile line. 
      for i in range(len(headline_)-1):
        extractedDataDict_[headline_[i]].append(linelist[i].strip())

      # extract subjId from path (linelist[1]) 
      subjId = getSubjIdFromPath(linelist[0], linelist[1])
      extractedDataDict_["subjectId"].append(subjId)

  # generate subjId file 
  with open(args.subjIdfile, 'w') as subjf:
    subjf.write("%s\n" % ','.join(headline_))
    for i in range(len(extractedDataDict_['subjectId'])):
      outputLine = []
      for headItem in headline_:
        outputLine.append(extractedDataDict_[headItem][i])
      subjf.write("%s\n" % ','.join(outputLine))

  # fill outputfile with "Scanner type" amd "Magnetic field of strength"
  fill_count = 0
  with open(args.inputfile, 'r') as inf:
    with open(args.outputfile, 'w') as outf:
      lines = inf.readlines()
      outf.write(lines[0])

      index = 0
      for line in lines[1:]:
        fill_occured = False
        linelist = line.split(',')

        # first find the matching subjectIds
        subjId = linelist[1]
        for i in range(len(extractedDataDict_["subjectId"])):
          if subjId == extractedDataDict_["subjectId"][i]:
            scanner_type = extractedDataDict_["Scanner type"][i]
            magnetic_strength = extractedDataDict_["Magnetic field of strength"][i]
            break
        else:
          print("Failed to find matched subjId: " + subjId)
          index += 1
          continue

        # always use the "Scanner type" amd "Magnetic field of strength" info from sourcefile, even
        # if such info already exists in input file. This is to make data consistent
        if scanner_type != "" and scanner_type != "Unknown":
          linelist[4] = scanner_type
        elif linelist[4] == "":
          linelist[4] = "Unknown"

        if magnetic_strength != "" and magnetic_strength != "Unknown":
          linelist[5] = magnetic_strength


        outf.write(','.join(linelist))

'''
        if linelist[4] != "":
          # but we want to check if two files provide same scanner type and magnetic strength info
          if linelist[4] != scanner_type:
            print("Line %d: original scanner_type:%s... new scanner_type:%s" % \
              (index, linelist[4], scanner_type))
        elif scanner_type == "":
          linelist[4] = "Unknown"
          print("Line %d: both files have empty scanner type" % index)
        else:
          linelist[4] = scanner_type
          fill_occured = True

        if linelist[5] != "":
          if linelist[5] != magnetic_strength:
            print("Line:%d: original Magnetic field of strength:%s... new Magnetic field of strength:%s" % \
              (index, linelist[5], magnetic_strength))
        elif magnetic_strength == "":
          print("Line %d: both files have empty magnetic_strength" % index)
        else:
          linelist[5] = magnetic_strength
          fill_occured = True

        outf.write(','.join(linelist))
        index += 1
        if fill_occured:
          fill_count += 1


      print("Total %d records filled" % fill_count)    
'''


if __name__ == '__main__':
  main()