library(ggplot2)
#library(tidyverse)
#install.packages("dplyr")
library(dplyr)

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/09_26_zscore_boxplot")

preharmo_data <- read.csv("preHarmo_LH_thickness.csv", stringsAsFactors = TRUE)
postharmo_data <- read.csv("postHarmo_LH_thickness.csv", stringsAsFactors = TRUE)

adjustByMeanData <- function(data, feature) {
  data1 <-data[data$eTIV != 0, ]
  adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$eTIV))) %>% select(Dataset, {{feature}}, adjByMean, eTIV)
  adjByMeanData
}

getZScoreByDataset <- function(data) {
  grouped_data <- data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(groupMean = mean, group_sd = sd))
  merged_data <- merge(data, grouped_data, by="Dataset")
  zscore_data<-merged_data %>% mutate(zscore=(adjByMean - groupMean)/group_sd)
  zscore_data
}

getZScore <- function(data) {
  zscore_data<-data %>% mutate(zscore=(adjByMean - mean(adjByMean))/sd(adjByMean))
  zscore_data
}

rmOutlier <- function(data, low, high) {
  data[data$zscore<high & data$zscore>low, ]
}

zscore_plot <- function(data, feature, low=-3.0, high=3.0) {
  t1<-adjustByMeanData(data, {{feature}})
  t2<-getZScore(t1)
  t3<-rmOutlier(t2, low, high)
  u<-ggplot(t3, aes(x=Dataset, y=zscore, color=Dataset))
  u + geom_boxplot() + ylim(low, high)
}

zscore_plot_grp_dataset <- function(data, feature, low=-3.0, high=3.0) {
  t1<-adjustByMeanData(data, {{feature}})
  t2<-getZScoreByDataset(t1)
  t3<-rmOutlier(t2, low, high)
  u<-ggplot(t3, aes(x=Dataset, y=zscore, color=Dataset))
  u + geom_boxplot() + ylim(low, high)
}


zscore_plot_grp_dataset(preharmo_data, lh_bankssts_thickness)
zscore_plot(postharmo_data, lh_bankssts_thickness)
zscore_plot_grp_dataset(postharmo_data, lh_bankssts_thickness)

zscore_plot_grp_dataset(preharmo_data, lh_cuneus_thickness)
zscore_plot(postharmo_data, lh_cuneus_thickness)
zscore_plot_grp_dataset(postharmo_data, lh_cuneus_thickness)

zscore_plot_grp_dataset(preharmo_data, lh_inferiortemporal_thickness)
zscore_plot(postharmo_data, lh_inferiortemporal_thickness)
zscore_plot_grp_dataset(postharmo_data, lh_inferiortemporal_thickness)




zscore_plot_grp_dataset(preharmo_data, lh_bankssts_curvind)
zscore_plot(postharmo_data, lh_bankssts_curvind)
zscore_plot_grp_dataset(postharmo_data, lh_bankssts_curvind)

zscore_plot_grp_dataset(preharmo_data, lh_cuneus_curvind)
zscore_plot(postharmo_data, lh_cuneus_curvind)
zscore_plot_grp_dataset(postharmo_data, lh_cuneus_curvind)

zscore_plot_grp_dataset(preharmo_data, lh_inferiortemporal_curvind)
zscore_plot(postharmo_data, lh_inferiortemporal_curvind)
zscore_plot_grp_dataset(postharmo_data, lh_inferiortemporal_curvind)



zscore_plot_grp_dataset(preharmo_data, lh_bankssts_gauscurv)
zscore_plot(postharmo_data, lh_bankssts_gauscurv)
zscore_plot_grp_dataset(postharmo_data, lh_bankssts_gauscurv)

zscore_plot_grp_dataset(preharmo_data, lh_cuneus_gauscurv)
zscore_plot(postharmo_data, lh_cuneus_gauscurv)
zscore_plot_grp_dataset(postharmo_data, lh_cuneus_gauscurv)

zscore_plot_grp_dataset(preharmo_data, lh_inferiortemporal_gauscurv)
zscore_plot(postharmo_data, lh_inferiortemporal_gauscurv)
zscore_plot_grp_dataset(postharmo_data, lh_inferiortemporal_gauscurv)


