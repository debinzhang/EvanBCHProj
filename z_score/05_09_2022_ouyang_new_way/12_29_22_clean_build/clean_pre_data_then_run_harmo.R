library(dplyr)
library(reticulate)  # this package enable python in R

# this script does two things:
# 1. remove the records from the raw preharmonized file that does not contain valid age 
# and sex information, the output is saved in a file ended with '_preHarmo_clean.csv'
# 2. Harmonize the above _preHarmo_clean.csv file to build harmonized 
#    _postHarmon_clean.csv file. Note: this file is generated before removing outliers

use_python("/usr/local/bin/python3")

wd <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/12_29_22_clean_build"
setwd(wd)
source_python("harmonDataBuilder_9_R_age_key.py")

file_list <- list("all_subjects_cortical_metrics_LH_curvind_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_LH_thicknessstd_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_RH_thickness_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_LH_foldind_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_RH_curvind_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_RH_thicknessstd_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_LH_gauscurv_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_RH_foldind_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_LH_meancurv_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_RH_gauscurv_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_LH_thickness_09_15_2022_preHarmo.csv",
                  "all_subjects_cortical_metrics_RH_meancurv_09_15_2022_preHarmo.csv")

for (file_path in file_list) {
    print(paste("working on :", file_path, sep = ''))
    file_name <- strsplit(file_path, "\\.")[[1]][1]
    file_extension <- strsplit(file_path, "\\.")[[1]][2]
    data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
    row_num_before <- nrow(data_raw)

    data_raw_1 <- data_raw[ which(data_raw$Age!="" & data_raw$Age!="Siemens"  &
                   (data_raw$Sex==1 | data_raw$Sex==2 |
                   data_raw$Sex=="M" | data_raw$Sex=="m" | data_raw$Sex=="F" |
                   data_raw$Sex=="f")
                   ),  ]
    row_num_after_sex <- nrow(data_raw_1)

    outfile <- paste(file_name, '_clean', '.csv', sep="")
    print(paste("creating cleanfile: ", outfile, sep = ''))
    write.csv(data_raw_1, outfile, row.names = FALSE, quote = FALSE)
    
    # harmonize the outfile
    harmo_file <- gsub("preHarmo", "postHarmo", outfile)
    print(paste("creating harmo file: ", harmo_file, sep = ''))
    print("------------")
    harmonize(outfile, harmo_file, scanertype=FALSE, Eb=FALSE, Mean_only=TRUE, dataset=TRUE, Age=TRUE)
}


