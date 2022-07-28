# EvanBCHProj

This repo contains the scripts and data for Evan's Boston Child Hospital image process projects. Please reference login_and_cmds.txt and harmonization_Evan/Rstudio_cmds.txt for more info.

**There are three directories:**

1. RealData

  The script, honey_real_1.py, searches all the stats files under the root_dir to generate  a list of csv files; one for a fieldname. The following is the command to run the script:
  
    python ./honey_real_1.py -s "." -f lh.aparc.stats -d ./Dataset.csv

The source directory, can be a directory list, such as 
"/neuro/labs/grantlab/research/MRI_Predict_Age/MGH/, /neuro/labs/grantlab/research/MRI_Predict_Age/BGSP"

This script combines the information from Dataset.csv file and *.stats file,
and generates per struct, such as SurfArea and GrayVol, csv files. From the dataset file, we get the subjectID, Age, Sex, Scanner-type, magnetic field strength,
then we use the subjectID as the key to search the corresponding patient's stats file
the subjectID resides in the directory path of the patient's stats file. For example, the 
directory name may be IXI313-HH-2241-T1. Here IXI313 is the patient's subjectID.
the directory name may also be in the form of deface_subjId1_subjId2_xxx, in this case
the final subjectId is "subjId1_subjId2".

After located the subjectID directory, we start to search the stats file within it. The status 
file is structed as multiple columns and rows, such as

ColHeaders StructName NumVert SurfArea GrayVol ThickAvg ThickStd MeanCurv GausCurv FoldInd CurvInd
bankssts                                 1407    933   2328  2.631 0.397     0.085     0.015        6     0.9
audalanteriorcingulate                   645    449   1291  2.624 0.572     0.124     0.028        9     0.9

where each row is for a surface area like "bankssts" or "audalanteriorcingulate ", 
and each column is for a fieldname, such as "NumVert" or "SurfArea "

This script generates multiple .csv files, one for each field, such as "NumVert.csv" and "SurfArea.csv". Each row of the generated file maps to a user info field or a surfaceArea, such as "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength", "bankssts", "caudalanteriorcingulate". Each column is for a patient.
  
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
and 6048 subsequent rows, with one row for a patient.
The first column "#" is a sequence number starting from 1.
The Dataset information is extracted from the "Path" field from the generated gen_structname.csv file. For example the "ABIDE_I" dataset is from the
/neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I/2NIFTI_SS_SEG_RAVENS/ Path

  This script can also plot harmonization pictures. Used command line option "-p "
to specify how many pictures you want to paint. The default number is one.

  The following is the command to run the script:

    python ./combatTest_5.py -s . -f Harmonization_meta_data.csv -p 1
    
    
  Another script,  postharmon_gen.py, generates a single PostHarmon_xxx.csv file.
The script assumes the corresponding gen_xxx.csv had been created.
So it needs to be run after running combatTest_5.py script, which generates gen_xxx.csv

  The following is the command to run the script:

    python ./postharmon_gen.py -s . -f Harmonization_meta_data.csv -n ICV 

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
      
 4. harmonization_Evan
 
  This is a newly added directory. The purpose for the scripts in this directory is to handle the whole process from stats collection to generating harmonization data. There are two scripts under harmonization_Evan directory:
  
  **a. honey_1.py**
    
  This script combines the information from Dataset.csv file and *.stats files, and saves the data in a single combined .csv file. The script works in the following steps:
	  
	      1. Search all the directories from sourceDir list that users input from commandline for the stats file that users specified from cmdline, such as lh.aparc.stats
	      2. Once such a directory is found, get the patient's subjectId from the directory path
	      3. Use the patient's subjectId as a key to search the dataset.csv file; If an entry is found get the patient's ["Dataset", "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength" info from the dataset.csv
	      4. From the patient's stats file (found in step 1), get the surfaceArea list, such as bankssts and caudalanteriorcingulate, and structName list, such as NumVert and SurfArea, and their corresponding data (it is a two dimensional array)
	      5. Combined patient's dataset data from step 3 and stats data from step4, output into the final combined csv file in the final csv file, the first row looks like:
        	
        	"Dataset", "subjectId", "Age", "Sex", "Scanner type", "Magnetic field of strength", "bankssts.NumVert", bankssts.SurfArea", ... "insula.NumVert", "insula.SurfArea"
        
        	and the rest of the rows are corresponding data. Each row is for a patient.
      
  The following is the command to run the script:
    
*`		python ./honey_1.py -s . -f lh.aparc.stats -d ./dataset.csv -o lh_combined_stats.csv`*
        
   Note: -s specifies the directory list that may contain the stats files. Currently, the following directories contain real data:
    
  These are the directories that contain 3NIFTI. It is what Evan needs so far:
  
    "/neuro/labs/grantlab/research/MRI_Predict_Age/MGH/3NIFTI_RAVENS, /neuro/labs/grantlab/research/MRI_Predict_Age/MGH/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/BGSP/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/Abbott/3NIFTI_RAVENS, /neuro/labs/grantlab/research/MRI_Predict_Age/BCH/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/DLBS/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/NIH_PD/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/beijingEn/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/IXI_600/3NIFTI_FreeSurfer, /neuro/labs/grantlab/research/MRI_Predict_Age/OASIS_3/3NIFTI_FreeSurfer"

  These are the directories that contain lh.aparc.stats. We used them previously:
    "/neuro/labs/grantlab/research/MRI_Predict_Age/ABIDE_I, /neuro/labs/grantlab/research/MRI_Predict_Age/BGSP, /neuro/labs/grantlab/research/MRI_Predict_Age/DLBS, /neuro/labs/grantlab/research/MRI_Predict_Age/HCPdevelopment, /neuro/labs/grantlab/research/MRI_Predict_Age/IXI_600, /neuro/labs/grantlab/research/MRI_Predict_Age/MGH, /neuro/labs/grantlab/research/MRI_Predict_Age/NIH_PD, /neuro/labs/grantlab/research/MRI_Predict_Age/beijingEn, /neuro/labs/grantlab/research/MRI_Predict_Age/BCH, /neuro/labs/grantlab/research/MRI_Predict_Age/Huaxi, /neuro/labs/grantlab/research/MRI_Predict_Age/OASIS_3"
  
 Note: For Rstudio usage, the output file,  lh_combined_stats.csv, is enough. No need for running the folllowing script. See the other cheat sheet, Rstudio_cmds.txt, in the same directory for more instructions.
    
 **b.  harmonDataBuilder_1.py**
    
 This script generates three types of files: gen_structname.csv, PostHarmon_structname.csv such as gen_Vermis.csv and PostHarmon_Vermis.csv, and combined PostHarmon_all.csv. 
    
 The script takes the lh_combined_stats.csv, which is built by the ./honey_1.py script in the last step, as input file; each row in the file is for a patient.

The generated gen_structname.csv file, such as gen_Vermis.csv, contains two duplicate rows, with the 2nd row slightly modified (added 0.01 to the last column). Each column is a for a patient.

The 2nd generated file, PostHarmon_Vermis.csv, contains the following columns:
    "#,Age,Vermis,Dataset" and 6048 subsequent rows, with one row for a patient. The first column "#" is a sequence number starting from 1. 

The 3rd generated file, PostHarmon_all.csv, combines all the PostHarmon_xxx.csv into a single file. That makes uploading to R Studio easier.

This script can also plot harmonization pictures. Used command line option "-p " to specify how many pictures you want to paint. The default number is one.
    
The following is the command to run the script:
     
  python ./harmonDataBuilder_1.py -s . -f combined_stats.csv -p 1 -o PostHarmon_all.csv 
  
 5. harmonization_Evan_w_vol

 The scripts:
	
	1. harmonDataBuilder_1_vol.py
	2. harmonDataBuilder_1_vol_no_empty_vol.py
	3. remove_empty_volume_patients.py
	4.	harmonDataBuilder_1_vol_no_empty_vol_w_age.py
	
 is similar to harmonization_Evan/harmonDataBuilder_1.py with the following differences:
 	1. harmonDataBuilder_1_vol.py adds volume as one of the harmonization covars, like:
 	
 	```json
` 		    covars = {
 		    	'batch':scannerList0,
              'gender':genderList0,
              'volume':volumeList0
           }
    ```
      To run the script:
    ```
        python ./harmonDataBuilder_1_vol.py -s . -f combined_stats.csv -p 1 -o PostHarmon_all.csv
    ```
        
 	2. harmonDataBuilder_1_vol_no_empty_vol.py

 	The combined_stats.csv may contain patients that have empty "volume". This script eliminates/ignores those patients whose records have empty volume.
 	
 	To run the script:
 	
 	```
 		python ./harmonDataBuilder_1_vol_no_empty_vol.py -s . -f combined_stats.csv -p 1 -o PostHarmon_all.csv
 	```
 	
 	3. remove_empty_volume_patients.py
 
 	This simple utility script simply removes all the records that have empty volumes. To run the script:
 	
 ```	
 python ./remove_empty_volume_patients.py -s . -f source_combined_stats.csv -o combined_stats_w_empty_vol_removed.csv
 ```
 	
 	4. harmonDataBuilder_1_vol_no_empty_vol_w_age.py
 	Similar to harmonDataBuilder_1.py, but adds both "volume" and "age" as harmonization covars, like:
 	
``` 	    covars = {'batch':scannerList0,
              'gender':genderList0,
              'volume':volumeList0,
              'age':ageList0}
```
 
 6. fillOasisVolumeData
 	
 	Evan's combined_stats.csv, which is obtains by running the honey_2.py script against BCH database, contains records with empty volumes. Somehow, Melissa's file (either PreHarmonization_w_dataset.csv or ../melissa_PreHarmonization_super_dataset.xlsx) has all patients' volume data. This script is to extract the volume info from Melissa file and fill them into Evan's combined_stats.csv.
 	
 	Note: so far all the records with missing volume come from Oasis.
 	
 	To run the script:
 	
 	```
 	python ./fillOasisVolume.py -s ./source.csv -i lh_combined_stats_07242022_1123pm.csv -o lh_combined_stats_07242022_1123pm_w_volume_filled.csv
 	```
 	
 	Here the ./source.csv is a manually generated file from PreHarmonization_w_dataset.csv. The source.csv file only has 'Dataset', 'Path', 'Volume', and 'Sex' columns. The PreHarmonization_w_dataset.csv is manually generated from the original melissa_PreHarmonization_super_dataset.xlsx. PreHarmonization_w_dataset.csv adds 'Dataset' column. The the Dataset information comes from "path".
 	
 	The script links the records in Melissa's and Evan's files with subject Id (like "OAS30080_d1318"). In Melissa's file the subject id stays in the "Path".
 	
 	
 	
 	
 	
 7. To_be_added