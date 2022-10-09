library(ggplot2)
#library(tidyverse)
#install.packages("dplyr")
library(dplyr)

#install.packages("devtools")
#devtools::install_github("hauselin/hausekeep")
library(hausekeep)

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/10_09_22_robust_zscore_boxplot")

preharmo_data <- read.csv("preHarmo_LH_gauscurv.csv", stringsAsFactors = TRUE)
postharmo_data <- read.csv("postHarmo_LH_gauscurv.csv", stringsAsFactors = TRUE)

adjustByMeanData <- function(data, feature) {
  data1 <-data[data$eTIV != 0, ]
  adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$eTIV))) %>% select(Dataset, {{feature}}, adjByMean, eTIV)
  adjByMeanData
}

getRobustZScoreByDataset <- function(data) {
  grouped_data <- data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
  merged_data <- merge(data, grouped_data, by="Dataset")
  
  zscore_data<-merged_data %>% mutate(zscore=(adjByMean - group_median)/group_mad)
  print(head(zscore_data, 3))
  zscore_data
}

rmOutlier <- function(data, low, high) {
  data[data$zscore<high & data$zscore>low, ]
}

zscore_plot_grp_dataset <- function(data, feature, low=-3.5, high=3.5) {
  t1<-adjustByMeanData(data, {{feature}})
  t2<-getRobustZScoreByDataset(t1)
  t3<-rmOutlier(t2, low, high)
  u<-ggplot(t3, aes(x=Dataset, y=zscore, color=Dataset))
  u + geom_boxplot() + ylim(low, high)
}


zscore_plot_grp_dataset(preharmo_data, lh_bankssts_thickness)
zscore_plot_grp_dataset(postharmo_data, lh_bankssts_thickness)

zscore_plot_grp_dataset(preharmo_data, lh_cuneus_thickness)
zscore_plot_grp_dataset(postharmo_data, lh_cuneus_thickness)

zscore_plot_grp_dataset(preharmo_data, lh_inferiortemporal_thickness)
zscore_plot_grp_dataset(postharmo_data, lh_inferiortemporal_thickness)


zscore_plot_grp_dataset(preharmo_data, lh_bankssts_curvind)
zscore_plot_grp_dataset(postharmo_data, lh_bankssts_curvind)

zscore_plot_grp_dataset(preharmo_data, lh_cuneus_curvind)
zscore_plot_grp_dataset(postharmo_data, lh_cuneus_curvind)

zscore_plot_grp_dataset(preharmo_data, lh_inferiortemporal_curvind)
zscore_plot_grp_dataset(postharmo_data, lh_inferiortemporal_curvind)



zscore_plot_grp_dataset(preharmo_data, lh_bankssts_gauscurv)
zscore_plot_grp_dataset(postharmo_data, lh_bankssts_gauscurv)

zscore_plot_grp_dataset(preharmo_data, lh_cuneus_gauscurv)
zscore_plot_grp_dataset(postharmo_data, lh_cuneus_gauscurv)

zscore_plot_grp_dataset(preharmo_data, lh_inferiortemporal_gauscurv)
zscore_plot_grp_dataset(postharmo_data, lh_inferiortemporal_gauscurv)



survey <- data.frame("col1" = c(-3, 1, 3, 3, 6, 8, 10, 10, 1000),
                     "col2" = c(-5, -1,1,1,4,6,8,8,998))

data2 <-outliersMAD(survey$col1)
survey$col1 <-data2
survey
na.omit(survey)


getRobustZScoreByDataset_0 <- function(data) {
  print('col #0: ')
  print(ncol(data))
  print('row #0: ')
  print(nrow(data))
  print(data[1:3, ])
  #head(data, 3)
  
  grouped_data <- data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(groupMed = median, group_mad = mad))
  print('col # 1: ')
  print(ncol(grouped_data))
  print('row #1: ')
  print(nrow(grouped_data))
  print(head(grouped_data, 3))
  
  merged_data <- merge(data, grouped_data, by="Dataset")
  print('col # 2: ')
  print(ncol(merged_data))
  print('row #2: ')
  print(nrow(merged_data))
  print(head(merged_data, 3))
  
  # replace all the outliers with NA
  print("lalala....")
  print(head(merged_data$adjByMean, 3))
  #filteredAdjByMean<- merged_data$adjByMean
  filteredAdjByMean <-outliersMAD(merged_data$adjByMean)
  #filteredAdjByMean <-outliersMAD(merged_data$adjByMean, MADCutOff = 3.0, replaceOutliersWith = -9999)
  #filteredAdjByMean <-outliersMAD(meanRow, MADCutOff = 3.0, replaceOutliersWith = -9999)
  #filteredAdjByMean <- merged_data$adjByMean %>% outliersMAD(MADCutOff = 3.0, replaceOutliersWith = -9999)
  print("filtered adjMean:")
  print(nrow(filteredAdjByMean))
  print(ncol(filteredAdjByMean))
  print(head(filteredAdjByMean))
  
  merged_data$adjByMean <- filteredAdjByMean
  # remove all the rows that contain NA
  print('col # 3: ')
  print(ncol(merged_data))
  print('row #3 ')
  print(nrow(merged_data))
  print(head(merged_data, 20))
  
  na.omit(merged_data)
  #merged_data[merged_data$adjByMean!=NA, ]
  print('col # 4: ')
  print(ncol(merged_data))
  print('row #4 ')
  print(nrow(merged_data))
  print(head(merged_data, 20))
  
  zscore_data<-merged_data %>% mutate(zscore=(adjByMean - groupMed)/group_mad)
  zscore_data
}

