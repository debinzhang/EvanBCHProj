import os
import re
import sys
import argparse

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./fillOasisVolume.py -s source.csv -i combined_stats.csv -o stats_w_volume.csv')
  parser.add_argument('-s', '--sourcefile', default='.', help='file that contains oasis volume data')
  parser.add_argument('-i', '--inputfile', default='', help='file that misses volume data')
  parser.add_argument('-o', '--outputfile', default='stats_w_volume.csv', help='output stats file with volume data')
  args = parser.parse_args()

  headline_ = []
  extractedDataDict_ = {}
  match_count = 0

  with open(args.sourcefile, 'r') as srcf:
    lines = srcf.readlines()
    headline_ = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
    headline_[-1] = headline_[-1].strip()

    for headItem in headline_:
      extractedDataDict_[headItem] = []

    for line in lines[1:]:
      linelist = line.split(',')
      for i in range(len(headline_)):
        extractedDataDict_[headline_[i]].append(linelist[i].strip())

  with open(args.inputfile, 'r') as inputf:
    with open(args.outputfile, 'w') as outputf:
      lines = inputf.readlines()
      outputf.write(lines[0])

      for line in lines[1:]:
        linelist = line.split(',')
        # only OASIS_3 dataset contain empty volume (6th column)
        if linelist[0] != "OASIS_3" or linelist[6] != "":
          outputf.write(line)
        else:
          # OASIS_3 subjectId is in the form of OAS####_d###, where #s are digits
          subjectId = linelist[1]
          result = re.search('OAS(.*)_d(.*)', subjectId)
          # for example: if subjectId = "OAS30080_d1318", then 
          # result.group(1)=30080, result.group(2) = 1318
          # print("%s...%s" % (result.group(1), result.group(2)))

          for i in range(len(extractedDataDict_["Path"])):
            if result.group(1) in extractedDataDict_["Path"][i] and \
              result.group(2) in extractedDataDict_["Path"][i]:
              linelist[6] = extractedDataDict_['Volume'][i]
              outputf.write("%s" % (','.join(linelist)))
              match_count += 1
              break
          else:
              # fail to find the corresponding OASIS record with volume data. 
              # just write the original line back
              outputf.write(line)

  print("Total %d missing volume records are fixed" % match_count)


if __name__ == '__main__':
  main()