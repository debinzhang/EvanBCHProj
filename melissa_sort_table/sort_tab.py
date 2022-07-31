import json
import logging
import os
import re
import sys
import argparse
import pprint
import numpy as np

def sort_tab(srcfile, outputfile):

  raw_rec_list = []
  ageGrpDict = {}
  percetileDict = {}
  recIndexDict = {}  #store the rec start and end indexes for each age group, such as 110-201

  with open(srcfile) as srcf:
    lines = srcf.readlines()

    record_index = 1
    for line in lines[1:]:
      lineList = line.strip().split(',')
      rec_tuple = (lineList[0], lineList[1], record_index)
      raw_rec_list.append(rec_tuple)
      record_index += 1

  # sort rec_list by age (y[0]) (This function has bug. need to dubug)
  #raw_rec_list.sort(key= lambda y:y[0])

  # create dictionary keyed by age range tuple
  # ageGrpDict: {(0,1):[], {1,2):[], ...}
  raw_rec_index = 0
  # age range from 0 to 100 years old
  for i in range(100):
    ageRangeTuple = (i, i+1.0)
    ageGrpDict[ageRangeTuple] = []
    #for each age group, calculate percetile volume value. This will be used in the next step
    percetileDict[ageRangeTuple] = []
    groupStartIndex = raw_rec_index

    while raw_rec_index < len(raw_rec_list) and float(raw_rec_list[raw_rec_index][0]) < ageRangeTuple[1]:
      ageGrpDict[ageRangeTuple].append(raw_rec_list[raw_rec_index])
      raw_rec_index += 1

    groupEndIndex = raw_rec_index
    if groupEndIndex > groupStartIndex or groupEndIndex > len(raw_rec_list):
      groupEndIndex -= 1

    # record index is 1 based and the first line is title, so add 2
    recIndexDict[ageRangeTuple] = (groupStartIndex+2, groupEndIndex+2)
       

  #print("=============")
  #pprint.pprint(ageGrpDict)

  # for each age group, divide patients into the following percetile categories:
  # 0%(min), 10%, 25%, 50%, 75%, 90%, and 100(max)% 
  pertDict = {
    "Min": 0,
    "10th": 10,
    "25th": 25,
    "Median": 50,
    "75th": 75,
    "90th": 90,
    "Max": 100
  }

  for ageTup, rec_list in ageGrpDict.items():
    vol_list = []
    for item in rec_list:
      vol_list.append(float(item[1].strip()))
    if not vol_list:
      continue
    npAarry = np.array(vol_list)
    #print(ageTup, npAarry)
    for pertKey, pertVal in pertDict.items():
      percetileDict[ageTup].append(np.percentile(npAarry, pertVal))

  #print("*************")
  #pprint.pprint(percetileDict)    

  #print("##################")
  #pprint.pprint(recIndexDict)

  # Write to output file
  with open(outputfile, 'w') as outf:
    #write tile line
    pertTitle = []
    for title, value in pertDict.items():
      pertTitle.append(title)
    titleStr = "Age Range, {pertTitleList}, index Range\n".format(pertTitleList=','.join(pertTitle))

    outf.write(titleStr)

    for ageTup, pertList in percetileDict.items():
      ageRangestr=str(ageTup[0]) + ' - ' + str(ageTup[1])
      # print("ageRangestr:%s" % ageRangestr)
      recRangStr = str(recIndexDict[ageTup][0]) + ' - ' + str(recIndexDict[ageTup][1])
      # print("recRangestr:%s" % recRangStr)
      pertStr = ','.join(str(x) for x in pertList)
      if not pertStr:
        continue
      #print("pertstr:%s" % pertStr)

      outf.write("%s,%s,%s\n" % (ageRangestr, pertStr, recRangStr))

  print("Data sorting complete. Result is saved in %s." % outputfile)

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./sort_tab.py -s ./raw_age_vol_tab.csv -o ./sorted_tab.csv')
  parser.add_argument('-s', '--sourcefile', default='./raw_age_vol_tab.csv', help='input file contains raw age volume data')
  parser.add_argument('-o', '--outputfile', default='./sorted_tab.csv', help='sorted output file')
  args = parser.parse_args()

  sort_tab(args.sourcefile, args.outputfile)

if __name__ == '__main__':
  main()