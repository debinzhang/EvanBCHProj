Many records Evan collected from BCH system miss "scanner type" and "Magnetic field of strength" information, like lh_combined_stats_07242022_1123pm_w_volume_filled.csv for example. Fortunately, Melissa's data sheet, PreHarmonization_w_dataset.csv.bak, has some of the missing "scanner type" and "Magnetic field of strength" information.


This script, ***fill_subjId_scan_type_strength.py***, extracts "scanner type" and "Magnetic field of strength" from Melissa's data sheet, fill them in Evan's data file, and use it to generate harmonization data. The key to link the records in both files is "subjectId". In Melissa's data sheet subjectId is buried in "path". The script parses the "path" info to get subjectId.

The sample command line:
```
*	python ./fill_subjid_scan_type_strength.py -s ./source_data.csv -b ./output_w_subjId.csv -i ./lh_combined_stats_07242022_1123pm_w_volume_filled.csv -o
./stats_w_scanner_info.csv*
```

The ***source_data.csv*** is obtained manually by removing all the feature columns from PreHarmonization_w_dataset.csv.bak. 

The ***output_w_subjId.csv*** file is just like source_data.csv but with added subjectId column.

The ***lh_combined_stats_07242022_1123pm_w_volume_filled.csv*** file is what Evan collected from BCH system, but has missing "scanner type" and "Magnetic field of strength" info.

The ***stats_w_scanner_info.csv ***is the output file, which just like  lh_combined_stats_07242022_1123pm_w_volume_filled.csv but had missing "scanner type" and "Magnetic field of strength" info filled.

---

The harmonization script in this directory, harmonDataBuilder_1_vol_no_empty_vol_w_age_sannertype.py is modified from harmonization_Evan_w_vol/harmonDataBuilder_1_vol_no_empty_vol_w_age.py, to add scanner type covars as a new harmonization parameter.

To run the harmonization command:

```
	mkdir output; cd output;
	python ../harmonDataBuilder_1_vol_no_empty_vol_w_age_sannertype.py -s ../ -f stats_w_scanner_info.csv -p 1 -o PostHarmon_all.csv
```