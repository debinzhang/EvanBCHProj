library(ggplot2)
#library(tidyverse)
#install.packages("dplyr")
library(dplyr)

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/output_RH_thickness")

preharmo_data <- read.csv("preHarmo.csv", stringsAsFactors = TRUE)
postharmo_data <- read.csv("PostHarmon_all.csv", stringsAsFactors = TRUE)

adjustByMeanData <- function(data, feature) {
  data1 <-data[data$Mean != 0, ]
  adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$Mean))) %>% select(Dataset, {{feature}}, adjByMean, Mean)
  adjByMeanData
}

getZScore <- function(data) {
  grouped_data <- data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(groupMean = mean, group_sd = sd))
  merged_data <- merge(data, grouped_data, by="Dataset")
  zscore_data<-merged_data %>% mutate(zscore=(adjByMean - groupMean)/group_sd)
  zscore_data
}

rmOutlier <- function(data, low, high) {
  data[data$zscore<high & data$zscore>low, ]
}

zscore_plot <- function(data, feature, low=-3.0, high=3.0) {
  t1<-adjustByMeanData(data, {{feature}})
  t2<-getZScore(t1)
  t3<-rmOutlier(t2, low, high)
  u<-ggplot(t3, aes(x=Dataset, y=zscore))
  u + geom_boxplot()
}

# boxplot with fixed scale (zscore always displayed within (-3.0, 3.0) range)
zscore_plot_fs <- function(data, feature, low=-3.0, high=3.0) {
  t1<-adjustByMeanData(data, {{feature}})
  t2<-getZScore(t1)
  t3<-rmOutlier(t2, low, high)
  u<-ggplot(t3, aes(x=Dataset, y=zscore))
  u + geom_boxplot() + ylim(low, high)
}

zscore_plot(preharmo_data, rh_bankssts_thickness)
zscore_plot(postharmo_data, rh_bankssts_thickness)
zscore_plot_fs(postharmo_data, rh_bankssts_thickness)

zscore_plot(preharmo_data, rh_caudalanteriorcingulate_thickness)
zscore_plot(postharmo_data, rh_caudalanteriorcingulate_thickness)
zscore_plot_fs(postharmo_data, rh_caudalanteriorcingulate_thickness)

zscore_plot(preharmo_data, rh_caudalmiddlefrontal_thickness)
zscore_plot(postharmo_data, rh_caudalmiddlefrontal_thickness)
zscore_plot_fs(postharmo_data, rh_caudalmiddlefrontal_thickness)

zscore_plot(preharmo_data, rh_cuneus_thickness)
zscore_plot(postharmo_data, rh_cuneus_thickness)
zscore_plot_fs(postharmo_data, rh_cuneus_thickness)

zscore_plot(preharmo_data, rh_entorhinal_thickness)
zscore_plot(postharmo_data, rh_entorhinal_thickness)
zscore_plot_fs(postharmo_data, rh_entorhinal_thickness)

zscore_plot(preharmo_data, rh_fusiform_thickness)
zscore_plot(postharmo_data, rh_fusiform_thickness)
zscore_plot_fs(postharmo_data, rh_fusiform_thickness)

zscore_plot(preharmo_data, rh_inferiorparietal_thickness)
zscore_plot(postharmo_data, rh_inferiorparietal_thickness)
zscore_plot_fs(postharmo_data, rh_inferiorparietal_thickness)

zscore_plot(preharmo_data, rh_inferiortemporal_thickness)
zscore_plot(postharmo_data, rh_inferiortemporal_thickness)
zscore_plot_fs(postharmo_data, rh_inferiortemporal_thickness)

zscore_plot(preharmo_data, rh_isthmuscingulate_thickness)
zscore_plot(postharmo_data, rh_isthmuscingulate_thickness)
zscore_plot_fs(postharmo_data, rh_isthmuscingulate_thickness)

zscore_plot(preharmo_data, rh_lateraloccipital_thickness)
zscore_plot(postharmo_data, rh_lateraloccipital_thickness)
zscore_plot_fs(postharmo_data, rh_lateraloccipital_thickness)

zscore_plot(preharmo_data, rh_lateralorbitofrontal_thickness)
zscore_plot(postharmo_data, rh_lateralorbitofrontal_thickness)
zscore_plot_fs(postharmo_data, rh_lateralorbitofrontal_thickness)

zscore_plot(preharmo_data, rh_lingual_thickness)
zscore_plot(postharmo_data, rh_lingual_thickness)
zscore_plot_fs(postharmo_data, rh_lingual_thickness)

zscore_plot(preharmo_data, rh_medialorbitofrontal_thickness)
zscore_plot(postharmo_data, rh_medialorbitofrontal_thickness)
zscore_plot_fs(postharmo_data, rh_medialorbitofrontal_thickness)

zscore_plot(preharmo_data, rh_middletemporal_thickness)
zscore_plot(postharmo_data, rh_middletemporal_thickness)
zscore_plot_fs(postharmo_data, rh_middletemporal_thickness)

zscore_plot(preharmo_data, rh_parahippocampal_thickness)
zscore_plot(postharmo_data, rh_parahippocampal_thickness)
zscore_plot_fs(postharmo_data, rh_parahippocampal_thickness)

zscore_plot(preharmo_data, rh_paracentral_thickness)
zscore_plot(postharmo_data, rh_paracentral_thickness)
zscore_plot_fs(postharmo_data, rh_paracentral_thickness)

zscore_plot(preharmo_data, rh_parsopercularis_thickness)
zscore_plot(postharmo_data, rh_parsopercularis_thickness)
zscore_plot_fs(postharmo_data, rh_parsopercularis_thickness)

zscore_plot(preharmo_data, rh_parsorbitalis_thickness)
zscore_plot(postharmo_data, rh_parsorbitalis_thickness)
zscore_plot_fs(postharmo_data, rh_parsorbitalis_thickness)

zscore_plot(preharmo_data, rh_parstriangularis_thickness)
zscore_plot(postharmo_data, rh_parstriangularis_thickness)
zscore_plot_fs(postharmo_data, rh_parstriangularis_thickness)

zscore_plot(preharmo_data, rh_pericalcarine_thickness)
zscore_plot(postharmo_data, rh_pericalcarine_thickness)
zscore_plot_fs(postharmo_data, rh_pericalcarine_thickness)

zscore_plot(preharmo_data, rh_postcentral_thickness)
zscore_plot(postharmo_data, rh_postcentral_thickness)
zscore_plot_fs(postharmo_data, rh_postcentral_thickness)

zscore_plot(preharmo_data, rh_posteriorcingulate_thickness)
zscore_plot(postharmo_data, rh_posteriorcingulate_thickness)
zscore_plot_fs(postharmo_data, rh_posteriorcingulate_thickness)

zscore_plot(preharmo_data, rh_precentral_thickness)
zscore_plot(postharmo_data, rh_precentral_thickness)
zscore_plot_fs(postharmo_data, rh_precentral_thickness)

zscore_plot(preharmo_data, rh_precuneus_thickness)
zscore_plot(postharmo_data, rh_precuneus_thickness)
zscore_plot_fs(postharmo_data, rh_precuneus_thickness)

zscore_plot(preharmo_data, rh_rostralanteriorcingulate_thickness)
zscore_plot(postharmo_data, rh_rostralanteriorcingulate_thickness)
zscore_plot_fs(postharmo_data, rh_rostralanteriorcingulate_thickness)

zscore_plot(preharmo_data, rh_rostralmiddlefrontal_thickness)
zscore_plot(postharmo_data, rh_rostralmiddlefrontal_thickness)
zscore_plot_fs(postharmo_data, rh_rostralmiddlefrontal_thickness)

zscore_plot(preharmo_data, rh_superiorfrontal_thickness)
zscore_plot(postharmo_data, rh_superiorfrontal_thickness)
zscore_plot_fs(postharmo_data, rh_superiorfrontal_thickness)

zscore_plot(preharmo_data, rh_superiorparietal_thickness)
zscore_plot(postharmo_data, rh_superiorparietal_thickness)
zscore_plot_fs(postharmo_data, rh_superiorparietal_thickness)

zscore_plot(preharmo_data, rh_superiortemporal_thickness)
zscore_plot(postharmo_data, rh_superiortemporal_thickness)
zscore_plot_fs(postharmo_data, rh_superiortemporal_thickness)

zscore_plot(preharmo_data, rh_supramarginal_thickness)
zscore_plot(postharmo_data, rh_supramarginal_thickness)
zscore_plot_fs(postharmo_data, rh_supramarginal_thickness)

zscore_plot(preharmo_data, rh_frontalpole_thickness)
zscore_plot(postharmo_data, rh_frontalpole_thickness)
zscore_plot_fs(postharmo_data, rh_frontalpole_thickness)

zscore_plot(preharmo_data, rh_temporalpole_thickness)
zscore_plot(postharmo_data, rh_temporalpole_thickness)
zscore_plot_fs(postharmo_data, rh_temporalpole_thickness)

zscore_plot(preharmo_data, rh_transversetemporal_thickness)
zscore_plot(postharmo_data, rh_transversetemporal_thickness)
zscore_plot_fs(postharmo_data, rh_transversetemporal_thickness)

zscore_plot(preharmo_data, rh_insula_thickness)
zscore_plot(postharmo_data, rh_insula_thickness)
zscore_plot_fs(postharmo_data, rh_insula_thickness)


