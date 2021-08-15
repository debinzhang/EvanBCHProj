# EvanBCHProj

This repo contains the scripts and data for Evan's Boston Child Hospital image process projects. There are three directories:

1. RealData

	The script, honey_real_1.py, searches all the stats files under the root_dir to generate  a list of csv files; one for a fieldname. The following is the command to run the script:
	
		python3 ./honey_real_1.py -s . -f lh.aparc.stats -d ./Dataset.csv
	
	
2. harmonization

	This script generates two types of files: gen_structname.csv and PostHarmon_structname.csv
such as gen_Vermis.csv and PostHarmon_Vermis.csv
The script takes Harmonization_meta_data.csv as input file, which has the structnames as
this first row, such as
"Path,ICV,Corpus Callosum,Right Ventral DC,Left Ventral DC,Vermis,..."
and the corresponding data in the following rows, one row for one patient.
In the sample Hamonization_meta_data.csv file, there are 6049 rows, with the first row as the list of structnames. So it ontains data for 6049-1 = 6048 patients

	The generated gen_structname.csv file, such as gen_Vermis.csv, contains two duplicate 
rows, with the 2nd row slightly modified (added 0.01 to the last column)
each column is a for a patient. So for the above Hamonization_meta_data.csv input file, the generated gen_Vermis.csv file contains two rows, each has 6048 columns, with each column is the Vermis data for a patient.

	The 2nd generated file, PostHarmon_Vermis.csv, contains the following columns: 
"#,Age,Vermis,Dataset"
and 6048 subsequent rows, with one row for a patient
The first column "#" is a sequence number starting from 1
the Dataset information is extracted from the "Path" field from the 
generated gen_structname.csv file. For example the "ABIDE_I" dataset is from the
/neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I/2NIFTI_SS_SEG_RAVENS/ Path

	This script can also plot harmonization pictures. Used command line option "-p "
to specify how many pictures you want to paint. The default number is one.

	The following is the command to run the script:

		python3 ./combatTest_5.py -s . -f Harmonization_meta_data.csv -p 1
		
		
	Another script,  postharmon_gen.py, generates a single PostHarmon_xxx.csv file
The script assume the corresponding gen_xxx.csv had been created.
So it needs to be run after running combatTest_5.py script, which generates gen_xxx.csv

	The following is the command to run the script:

		postharmon_gen.py -s . -f Harmonization_meta_data.csv -n ICV	

3. melissaVersion

	This script takes a single fieldname_Input.csv file, such as GausCurve_Input.csv, 
to generate gen_fieldname_Input.csv, such as gen_GausCurve_Input.csv

	Let's use the GausCurve_Input.csv file as an example to explain how the script works:
Each column of GausCurve_Input.csv file is for a patient
Each row of the GausCurve_Input.csv file is is for a struct name, such as "bankssts",
or "caudalanteriorcingulate". Please see the lh.aparc.stats file as an example
In the sample GausCurve_Input.csv file, it contains data for 211 patients and 35 struct
names. So it has 35 rows and 212 columns

	what this script does is to flip this file so that each row is for a patient, and 
each column is for a struct name. So the generated gen_GausCurve_Input.csv file
contains 211 rows and 35 column

	This script also requires the following .txt files, which are used by the neuroCombat
program to generate index lists:
Evan_Scanner.txt, Evan_Gender.txt, Evan_Vendor.txt, Evan_Age.txt

	The following is the command to run the script:
	
			python3 Evan_Harmonization_1.py -s GausCurve