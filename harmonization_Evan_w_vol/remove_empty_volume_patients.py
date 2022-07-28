import os
import re
import sys
import argparse

def remove_empty_vol_patients(path, srcfilename, outputfile):
  with open(os.path.join(path, srcfilename), 'r') as fin:
    with open(os.path.join(path, outputfile), 'w') as fout:
      lines = fin.readlines()
      headline = re.sub('[^\u0000-\u007f]', '',  lines[0]).split(',')
      #headline_[-1] = headline_[-1].strip()
      skip_count = 0

      fout.write("%s" % ','.join(headline));

      for line in lines[1:]:
        linelist = line.split(',')
        #if Volume (6th column) is empty, ignore this patient
        if linelist[6] == "":
          skip_count += 1
          continue
        else:
          fout.write("%s" % ','.join(linelist))

  return skip_count

def main():
  parser = argparse.ArgumentParser(description='Example cmd:  python ./remove_empty_volume_patients.py -s . -f combined_stats.csv -o combined_stats_w_empty_vol_removed.csv')
  parser.add_argument('-s', '--sourcedir', default='.', help='directory that contains combined_stats.csv file')
  parser.add_argument('-f', '--filename', default='raw_stats.csv', help='raw combined stats file with empty volume records')
  parser.add_argument('-o', '--output', default='raw_stats_wo_empty_vol.csv', help='combined_stats file with empty volume records removed')
  args = parser.parse_args()
  skip_count = 0

  skip_count = remove_empty_vol_patients(args.sourcedir, args.filename, args.output)

  print("total %d empty volume patients are removed" % skip_count)

if __name__ == '__main__':
  main()
