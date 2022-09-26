import os
import re
import sys
import argparse

def getSubjIdDatasetFromPath(path):
  unknownPath = False
  failToParse = False
  subjIdFound = True

  if "ABIDE_I" in path:
    dataset = "ABIDE_I"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I/2NIFTI_SS_SEG_RAVENS/ABIDE50030/ABIDE50030_MPRAGE_ss.nii.gz"
    # paath = "/neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I/3NIFTI_FreeSurfer/ABIDE50030_MPRAGE"
    result = re.search('(.*)3NIFTI_FreeSurfer/ABIDE([0-9]*)_(.*)', path)
    if result is not None:
      # subjId = '50030'
      subjId = result.group(2)
    else:
      failToParse = True

  elif "BCH" in path:
    dataset = "BCH"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/BCH/NIFTI_NORMAL_2_MulModReg_SS/Year4-5_Q3/4438380/4438380-MPRAGE_rMPRAGE_ss.nii.gz"
    # subjId = "Year4-5_Q3/4438380"
    #result = re.search('(.*)Year(.*)/([0-9]*)/(.*)', path)

    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/BCH/3NIFTI_FreeSurfer/4574338-MPRAGE_rMPRAGE"
    result = re.search('(.*)3NIFTI_FreeSurfer/([0-9]*)-(.*)', path)
    if result is not None:
      # subjId = 4574338
      subjId = result.group(2)
    else:
      failToParse = True

  elif "beijingEn" in path:
    dataset = "beijingEn"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/beijingEn/2NIFTI_SS_REG_RAVENS/BJ3905035/BJ3905035_MPRAGE_ss.nii.gz"
    # result = re.search('(.*)/BJ([0-9]*)/BJ(.*)', path)
    # subjId = '3905035'

    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/beijingEn/3NIFTI_FreeSurfer/BJ1060223_MPRAGE"
    result = re.search('(.*)/3NIFTI_FreeSurfer/BJ([0-9]*)_(.*)', path)
    if result is not None:
      # subjId = '1060223'
      subjId = result.group(2)
    else:
      failToParse = True

  elif "BGSP" in path:
    dataset = "BGSP"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/BGSP/2NIFTI_SS_REG_RAVENS/Sub0004_Ses1_Scan_01_ANAT1/Sub0004_Ses1_Scan_01_ANAT1_ss.nii.gz"
    # result = re.search('(.*)/Sub([0-9]*)_Ses1_Scan_(.*)', path)
    # subjId = 'Sub0004_Ses1'
    #subjId = "Sub" + result.group(2) + "_Ses1"

    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/BGSP/3NIFTI_FreeSurfer/Sub0001_Ses1_Scan_01_ANAT1"
    result = re.search('(.*)/Sub([0-9]*)_Ses1_Scan_(.*)', path)
    if result is not None:
      # subjId = 'Sub0001_Ses1'
      subjId = "Sub" + result.group(2) + "_Ses1"
    else:
      failToParse = True

  elif "DLBS" in path:
    dataset = "DLBS"
    # path ="/neuro/labs/grantlab/research/MRI_Predict_Age/DLBS/2NIFTI_SS_REG_RAVENS/0028376/0028376-session_1-anat_ss.nii.gz"
    # result = re.search('(.*)_RAVENS/(0*)([0-9]*)/(.*)', path)
    # subjId = 28376
    # subjId = result.group(3)

    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/DLBS/3NIFTI_FreeSurfer/0028326-session_1-anat"
    result = re.search('(.*)/3NIFTI_FreeSurfer/(0*)([0-9]*)-(.*)', path)
    if result is not None:
      # subjId = 28326
      subjId = result.group(3)
    else:
      failToParse = True

  elif "IXI_600" in path:
    dataset = "IXI_600"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/IXI_600/2NIFTI_SS_REG_RAVENS/IXI461-Guys-0998-T1/IXI461-Guys-0998-T1_ss.nii.gz"
    #result = re.search('(.*)_RAVENS/IXI([0-9]*)-(.*)', path)
    # subjId = 'IXI461'
    #subjId = "IXI" + result.group(2)

    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/IXI_600/3NIFTI_FreeSurfer/IXI002-Guys-0828-T1"
    result = re.search('(.*)/3NIFTI_FreeSurfer/IXI([0-9]*)-(.*)', path)
    if result is not None:
      # subjId = 'IXI002'
      subjId = "IXI" + result.group(2)
    else:
      failToParse = True

  elif "MGH" in path:
    dataset = "MGH"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/MGH/2NIFTI_SS_REG_RAVENS/HIE_165/VISIT_01/HIE_165-VISIT_01-SAGSPGRPRE_ss.nii.gz"
    #result = re.search('(.*)_RAVENS/HIE_([0-9]*)/(.*)', path)
    # subjId = 'HIE_165'
    #subjId = "HIE_" + result.group(2)

    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/MGH/3NIFTI_FreeSurfer/HIE_012-VISIT_01-SAGMPRAGE_P2_1MM_PRE"
    result = re.search('(.*)/3NIFTI_FreeSurfer/HIE_([0-9]*)-(.*)', path)
    if result is not None:
      # subjId = 'HIE_012'
      subjId = "HIE_" + result.group(2)
    else:
      failToParse = True


  elif "NIH_PD" in path:
    dataset = "NIH_PD"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/NIH_PD/2NIFTI_SS_REG_RAVENS/deface_1020_v1_t1w/deface_1020_v1_t1w_ss.nii.gz"
    # result = re.search('(.*)_RAVENS/deface_([0-9]*)_v([0-9]+)_(.*)', path)
    # subjId = '1020_v1'
    # subjId = result.group(2) + "_v" + result.group(3)

    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/NIH_PD/3NIFTI_FreeSurfer/deface_1002_v1_t1w"
    result = re.search('(.*)3NIFTI_FreeSurfer/deface_([0-9]*)_v([0-9]+)_(.*)', path)
    if result is not None:
      # subjId = '1002_v1'
      subjId = result.group(2) + "_v" + result.group(3)
    else:
      failToParse = True

  elif "OASIS_3" in path:
    dataset = "OASIS_3"
    # path = "/neuro/labs/grantlab/research/MRI_Predict_Age/OASIS_3/2NIFTI_SS_REG_RAVENS/sub-OAS30066_ses-d0524_T1w/sub-OAS30066_ses-d0524_T1w_ss.nii.gz"
    # result = re.search('(.*)_RAVENS/sub-OAS([0-9]+)_ses-d(0*)([0-9]+)_(.*)', path)
    # subjId = 'OAS30066_d524'
    # subjId = "OAS" + result.group(2) + "_d" + result.group(4)

    # path= "/neuro/labs/grantlab/research/MRI_Predict_Age/OASIS_3/3NIFTI_FreeSurfer/sub-OAS30001_ses-d0129_run-01_T1w"
    result = re.search('(.*)3NIFTI_FreeSurfer/sub-OAS([0-9]+)_ses-d(0*)([0-9]+)_(.*)', path)
    if result is not None:
      # subjId = 'OAS30066_d524'
      subjId = "OAS" + result.group(2) + "_d" + result.group(4)
    else:
      failToParse = True

  else:
    unknownPath = True
    subjIdFound = False
    print(" !!!  path: %s with unknown subject ID" + path)

  if failToParse:
    subjIdFound = False
    print("Failed to parse path: %s" % path)

  if not subjIdFound:
    dataset = "UK"
    subjId = "NA"
    
  return subjIdFound, dataset, subjId

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./add_subjId_dataset_and_harmo_vaars.py -s ./all_subjects_cortical_metrics_RH_thickness_09_02_2022.csv \
   -i ./source_data_w_subjId.csv -o ./preHarmo.csv')
  parser.add_argument('-s', '--sourcefile', default='./all_subjects_cortical_metrics_RH_thickness_09_02_2022.csv', help='source data file that contains only path and feature info')
  parser.add_argument('-i', '--datasetFile', default='./source_data_w_subjId.csv', help='file that patient gender, age, scanner_type, subjId info')  
  parser.add_argument('-o', '--preHarmoFile', default='./preHarmo.csv', help='output file that contains dataset, subject Id, gender, age, scanner_type etc. that is ready for harmonization')
  args = parser.parse_args()

  # Note: the datasetFile or source_data_w_subjId.csv file is a copy of harmonization_Evan_w_vol_age_scannertype/output_w_subjId.csv. This file was obtained from 
  # PreHarmonization_w_dataset.csv.bak with subjectId added, and the PreHarmonization_w_dataset.csv was originally manually generated from the 
  # melissa_PreHarmonization_super_dataset.xlsx.

  # read in datasetFile/source_data_w_subjId.csv, from which we will get harmonization vars, like sex, age, scanner_type, etc...
  ds_headline_ = []
  ds_extractedDataDict_ = {}
  with open(args.datasetFile, 'r') as dsf:
    lines = dsf.readlines()
    ds_headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    ds_headline_[-1] = ds_headline_[-1].strip()

    for ds_headItem in ds_headline_:
      ds_extractedDataDict_[ds_headItem] = []

    for line in lines[1:]:
      linelist = line.split(',')
      for i in range(len(ds_headline_)):
        ds_extractedDataDict_[ds_headline_[i]].append(linelist[i].strip())


  # read in sourcefile
  headline_ = []
  extractedDataDict_ = {}
  match_count = 0
  # miss_count is patient count for whom we cannot find match record in datasetFile
  miss_count = 0
  # miss_subjId_count is the patient count for whom we cannot generate his/her subject ID
  miss_subjId_count = 0

  with open(args.sourcefile, 'r') as srcf:
    lines = srcf.readlines()
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()
    # add the following columns in headerline
    headline_.append("subjectId")
    headline_.append("Dataset")
    headline_.append("Age")
    headline_.append("Sex")
    headline_.append("Scanner type")
    headline_.append("Magnetic field of strength")
    headline_.append("Volume")

    for headItem in headline_:
      extractedDataDict_[headItem] = []

    for line in lines[1:]:
      linelist = line.split(',')
      # extract subjId from path (linelist[1]) 
      subjIdFound, dataset, subjId = getSubjIdDatasetFromPath(linelist[0])
      if not subjIdFound:
        print("Cannot find subjId for %s, ignore this patient!" % linelist[0])
        miss_subjId_count += 1
        continue

      match_found = False
      # search the entry with matching subjectId in datasetFile . If found, get Age, Sex, Scanner_type,
      # Magnetic field of strength, and Volume info from it
      for j in range(len(ds_extractedDataDict_["subjectId"])):
        # "BCH" patients do not have complete subjectId info in the path. Need to handle separately 
        if dataset == "BCH":
          if not ds_extractedDataDict_["Dataset"][j] == "BCH":
            continue

          # print("Found BCH 2 subjId: %s ... ds_subjeid: %s" % (subjId, ds_extractedDataDict_["subjectId"][j]))
          if subjId in ds_extractedDataDict_["subjectId"][j]:
            match_found = True
        elif subjId == ds_extractedDataDict_["subjectId"][j]:
          if not dataset == ds_extractedDataDict_["Dataset"][j]:
            print("Found match subjectId: %s but mismatch dataset: %s vs %s, ignore this entry!" % \
                 (subjId, dataset, ds_extractedDataDict_["Dataset"][j]))
            miss_count += 1
            break
          else:
            match_found = True

        if match_found:
          # copy the sourcefile line. 
          for k in range(len(headline_)-7):
            extractedDataDict_[headline_[k]].append(linelist[k].strip())

          extractedDataDict_["subjectId"].append(subjId)
          extractedDataDict_["Dataset"].append(dataset)
          extractedDataDict_["Age"].append(ds_extractedDataDict_["Age"][j])
          extractedDataDict_["Sex"].append(ds_extractedDataDict_["Sex"][j])
          extractedDataDict_["Scanner type"].append(ds_extractedDataDict_["Scanner type"][j])
          extractedDataDict_["Magnetic field of strength"].append(ds_extractedDataDict_["Magnetic field of strength"][j])
          extractedDataDict_["Volume"].append(ds_extractedDataDict_["Volume"][j])
          match_count += 1
          break
      else:
        miss_count += 1        
        print("Failed to find matching subjectId:%s ... dataset:%s for %s. This patient will be ignored!" % (subjId, dataset, linelist[0]))

    # generate preHarmoFile
    with open(args.preHarmoFile, 'w') as preHarmof:
      preHarmof.write("%s\n" % ','.join(headline_))
      for i in range(len(extractedDataDict_['subjectId'])):
        outputLine = []
        for headItem in headline_:
          outputLine.append(extractedDataDict_[headItem][i])
        preHarmof.write("%s\n" % ','.join(outputLine))

    print("Matching records: %d... mismatch record:%d... records that failed to build subjectId::%d" % \
      (match_count, miss_count, miss_subjId_count))

if __name__ == '__main__':
  main()
