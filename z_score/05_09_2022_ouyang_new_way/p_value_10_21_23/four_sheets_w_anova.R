library(ggplot2)
library(dplyr)

wd <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/p_value_10_21_23"
setwd(wd)
data_raw <- read.csv("preHarmo_LH_thickness.csv", stringsAsFactors = TRUE)
data_harmo <- read.csv("postHarmo_LH_thickness.csv", stringsAsFactors = TRUE)

gen_4_sheet <- function(data_raw, data_harmo, feature) {
  #  raw data
  print("Start to process raw data ...", quote=FALSE)
  raw_tbl_list <- remove_outlier(data_raw, feature, FALSE)
  gen_csv(raw_tbl_list, feature, FALSE)

  print(paste("AVO summary for raw ", feature, " before removing outlier:", sep=''), quote=FALSE)
  aov_rst <- aov(get(feature) ~ Dataset, data= data.frame(raw_tbl_list[1]))
  print(summary(aov_rst))
  
  print("", quote=FALSE)
  print(paste("AVO summary for raw ", feature, " after removing outlier:", sep=''), quote=FALSE)
  aov_rst2 <- aov(get(feature) ~ Dataset, data = data.frame(raw_tbl_list[2]))  
  print(summary(aov_rst2))

  # harmonization data
  print("", quote=FALSE)
  print("", quote=FALSE)
  print("Start to process harmonization data ...", quote=FALSE)
  harmo_tbl_list <- remove_outlier(data_harmo, feature, TRUE)
  gen_csv(harmo_tbl_list, feature, TRUE)
  
  print(paste("AVO summary for harmonization ", feature, " before removing outlier:", sep=''), quote=FALSE)
  aov_rst <- aov(get(feature) ~ Dataset, data= data.frame(harmo_tbl_list[1]))
  print(summary(aov_rst))
  
  print("", quote=FALSE)
  print(paste("AVO summary for harmonization ", feature, " after removing outlier:", sep=''), quote=FALSE)
  aov_rst2 <- aov(get(feature) ~ Dataset, data = data.frame(harmo_tbl_list[2]))  
  print(summary(aov_rst2))
}

gen_csv <- function(data_list, feature, harmoData=FALSE) {
  if (harmoData) {
    prefix <- 'harmo_'
  } else {
    prefix <- 'raw_'
  }
  # output pre_outlier removal csv file that contains only dataset and feature column 
  # file name is like: harmo_lh_bankssts_thickness.csv
  outfile <- paste(wd, '/', prefix, feature, ".csv", sep="")
  write.csv(data_list[1], outfile, row.names = FALSE)
  
  # output post_outlier removal csv file that contains only dataset and feature column
  # file name is like: harmo_no_outlier_lh_bankssts_thickness.csv
  outfileAfrm <- paste(wd, '/', prefix, 'no_outlier_', feature, '.csv', sep="")
  write.csv(data_list[2], outfileAfrm, row.names = FALSE)
}

remove_outlier <- function(data, feature, harmoData=FALSE) {
  featureAndDataset <- data %>% select(Dataset, {{feature}})
  
  aov_rst <- aov(lh_bankssts_thickness ~ Dataset, data = data)
  summary(aov_rst)

  nrowB4rm <- nrow(featureAndDataset)
  quartiles <- quantile(featureAndDataset[[feature]], probs=c(.25, .75), na.rm = FALSE)
  IQR <-  IQR(featureAndDataset[[feature]])
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR
  
  data_no_outlier <- subset(featureAndDataset, featureAndDataset[[feature]]>Lower & 
                              featureAndDataset[[feature]] < Upper)
  nrowAfrm <- nrow(data_no_outlier)
  print(paste("Outlier Removal Result: ", "row# before removal:", nrowB4rm, "; Row# after removal:", 
             nrowAfrm, "; Outlier row removed:", (nrowB4rm-nrowAfrm), sep=""), quote=FALSE)
  
  # put the two output data table into a list, and return the list
  list(featureAndDataset, data_no_outlier)
}

gen_4_sheet(data_raw, data_harmo, 'lh_bankssts_thickness')

