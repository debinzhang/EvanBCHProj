# 05_09_2022_ouyang_new_way

	This folder keeps the scripts and data files that are used to calculate z-score with the new approach that Ouyang told Evan on May 09 2022.
	
	Today Ouyang gave Evan a set of new csv files. Each of the files contains a column that lists the mean of a feature, such as "thickness". In the all_subjects_cortical_metrics_RH_thickness_09_02_2022.csv file, the mean column is "rh_MeanThickness_thickness". 
	
	For each feature, we first divide the feature value by the "mean", like:
		`adjByMean= {feature}/rh_MeanThickness_thickness`
		
   Then the z_score is calculated as:
   
     `zscore=(adjByMean - mean(adjByMean))/sd(adjByMean)`
     
   Then we remove the z_scores that go beyond the valid range (outliers). Finally we use boxplot to graph the z_scores before and after harmonization.
   
   
## Steps to calculate z_scores:

### 1. Build data for harmonization:
	The data (.csv) file does not contain the co-vars data, such as "Sex", "Age", "Scanner_type", that is necessary for harmonization. But it has a "path" column, from which we can calculate each row's (patient) subjectId. Then we use the subjectId as a key to retrieve the co-vars information from our "super-dataset" file.
	
	The super datasetFile or source_data_w_subjId.csv file is a copy of harmonization_Evan_w_vol_age_scannertype/output_w_subjId.csv. This file was obtained from PreHarmonization_w_dataset.csv.bak with subjectId added, and the PreHarmonization_w_dataset.csv was originally manually generated from the melissa_PreHarmonization_super_dataset.xlsx.
	
	The script that performs the above operation is:
	
	`add_subjid_dataset_and_harmo_vars.py -s ./all_subjects_cortical_metrics_RH_thickness_09_02_2022.csv -i ./source_data_w_subjId.csv -o ./preHarmo.csv'`
   
   ./preHarmo.csv is the output file that contains both feature data and harmonization co-var data, such as "Sex", "Age", etc.
	
### 2. Build harmonization data

	Here we modified the harmonDataBuilder_1.py script from harmonization_Evan directory to generate post-harmonization data. We added dataset as part of neuroCombat co-vars, and we set "mean_only=True" while calling the neuroCombat function. The new script is called harmonDataBuilder_2.py. The following is an example of how to run the above commands (here we assume to original data is saved in ../data_w_mean/all_subjects_cortical_metrics_RH_gauscurv_09_02_2022.csv):	
 	 mkdir output_RH_thickness; 
	 cd output_RH_thickness;
	 python ../add_subjid_dataset_and_harmo_vars.py -s ../data_w_mean/all_subjects_cortical_metrics_RH_gauscurv_09_02_2022.csv -i ../source_data_w_subjId.csv -o ./preHarmo.csv
	 python ../harmonDataBuilder_2.py -s . -f preHarmo.csv -p 1 -o PostHarmon_all.csv
	 
### 3. Plot z_scroe with R Studio
    The commands to run R studio are saved in:
    	new_zscore_calculation.R
    
    We have several sample output directories, such as 05_09_2022_ouyang_new_way/output_RH_thickness. Inside each such directory, there is a customized R script that is tailored for that special feature. For example, under 005_09_2022_ouyang_new_way/output_RH_thickness, such script is called new_zscore_calculation_RH_thickness.R. We can directly open this script file inside R-Studio and generate boxplots. 
    
    There are also several sample boxplot output directories, such as 05_09_2022_ouyang_new_way/plotBoxes_RH_thickness and 05_09_2022_ouyang_new_way/plotBoxes_RH_gauscurv, which shows the before/after harmonization boxplots.
    	
 
	 

   
   

   
		