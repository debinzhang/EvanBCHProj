library(ggplot2)
#library(tidyverse)
#install.packages("dplyr")
library(dplyr)

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/output_RH_thicknessstd")

preharmo_data <- read.csv("preHarmo.csv", stringsAsFactors = TRUE)
postharmo_data <- read.csv("PostHarmon_all.csv", stringsAsFactors = TRUE)

adjustByMeanData <- function(data, feature) {
  data1 <-data[data$Mean != 0, ]
  adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$Mean))) %>% select(Dataset, {{feature}}, adjByMean, Mean)
  adjByMeanData
}

getZScore0 <- function(data) {
  zscore_data<-data %>% mutate(zscore=(adjByMean - mean(adjByMean))/sd(adjByMean))
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

zscore_plot(preharmo_data, rh_bankssts_thicknessstd)
zscore_plot(postharmo_data, rh_bankssts_thicknessstd)
zscore_plot_fs(postharmo_data, rh_bankssts_thicknessstd)

zscore_plot(preharmo_data, rh_caudalanteriorcingulate_thicknessstd)
zscore_plot(postharmo_data, rh_caudalanteriorcingulate_thicknessstd)
zscore_plot_fs(postharmo_data, rh_caudalanteriorcingulate_thicknessstd)

zscore_plot(preharmo_data, rh_caudalmiddlefrontal_thicknessstd)
zscore_plot(postharmo_data, rh_caudalmiddlefrontal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_caudalmiddlefrontal_thicknessstd)

zscore_plot(preharmo_data, rh_cuneus_thicknessstd)
zscore_plot(postharmo_data, rh_cuneus_thicknessstd)
zscore_plot_fs(postharmo_data, rh_cuneus_thicknessstd)

zscore_plot(preharmo_data, rh_entorhinal_thicknessstd)
zscore_plot(postharmo_data, rh_entorhinal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_entorhinal_thicknessstd)

zscore_plot(preharmo_data, rh_fusiform_thicknessstd)
zscore_plot(postharmo_data, rh_fusiform_thicknessstd)
zscore_plot_fs(postharmo_data, rh_fusiform_thicknessstd)

zscore_plot(preharmo_data, rh_inferiorparietal_thicknessstd)
zscore_plot(postharmo_data, rh_inferiorparietal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_inferiorparietal_thicknessstd)

zscore_plot(preharmo_data, rh_inferiortemporal_thicknessstd)
zscore_plot(postharmo_data, rh_inferiortemporal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_inferiortemporal_thicknessstd)

zscore_plot(preharmo_data, rh_isthmuscingulate_thicknessstd)
zscore_plot(postharmo_data, rh_isthmuscingulate_thicknessstd)
zscore_plot_fs(postharmo_data, rh_isthmuscingulate_thicknessstd)

zscore_plot(preharmo_data, rh_lateraloccipital_thicknessstd)
zscore_plot(postharmo_data, rh_lateraloccipital_thicknessstd)
zscore_plot_fs(postharmo_data, rh_lateraloccipital_thicknessstd)

zscore_plot(preharmo_data, rh_lateralorbitofrontal_thicknessstd)
zscore_plot(postharmo_data, rh_lateralorbitofrontal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_lateralorbitofrontal_thicknessstd)

zscore_plot(preharmo_data, rh_lingual_thicknessstd)
zscore_plot(postharmo_data, rh_lingual_thicknessstd)
zscore_plot_fs(postharmo_data, rh_lingual_thicknessstd)

zscore_plot(preharmo_data, rh_medialorbitofrontal_thicknessstd)
zscore_plot(postharmo_data, rh_medialorbitofrontal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_medialorbitofrontal_thicknessstd)

zscore_plot(preharmo_data, rh_middletemporal_thicknessstd)
zscore_plot(postharmo_data, rh_middletemporal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_middletemporal_thicknessstd)

zscore_plot(preharmo_data, rh_parahippocampal_thicknessstd)
zscore_plot(postharmo_data, rh_parahippocampal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_parahippocampal_thicknessstd)

zscore_plot(preharmo_data, rh_paracentral_thicknessstd)
zscore_plot(postharmo_data, rh_paracentral_thicknessstd)
zscore_plot_fs(postharmo_data, rh_paracentral_thicknessstd)

zscore_plot(preharmo_data, rh_parsopercularis_thicknessstd)
zscore_plot(postharmo_data, rh_parsopercularis_thicknessstd)
zscore_plot_fs(postharmo_data, rh_parsopercularis_thicknessstd)

zscore_plot(preharmo_data, rh_parsorbitalis_thicknessstd)
zscore_plot(postharmo_data, rh_parsorbitalis_thicknessstd)
zscore_plot_fs(postharmo_data, rh_parsorbitalis_thicknessstd)

zscore_plot(preharmo_data, rh_parstriangularis_thicknessstd)
zscore_plot(postharmo_data, rh_parstriangularis_thicknessstd)
zscore_plot_fs(postharmo_data, rh_parstriangularis_thicknessstd)

zscore_plot(preharmo_data, rh_pericalcarine_thicknessstd)
zscore_plot(postharmo_data, rh_pericalcarine_thicknessstd)
zscore_plot_fs(postharmo_data, rh_pericalcarine_thicknessstd)

zscore_plot(preharmo_data, rh_postcentral_thicknessstd)
zscore_plot(postharmo_data, rh_postcentral_thicknessstd)
zscore_plot_fs(postharmo_data, rh_postcentral_thicknessstd)

zscore_plot(preharmo_data, rh_posteriorcingulate_thicknessstd)
zscore_plot(postharmo_data, rh_posteriorcingulate_thicknessstd)
zscore_plot_fs(postharmo_data, rh_posteriorcingulate_thicknessstd)

zscore_plot(preharmo_data, rh_precentral_thicknessstd)
zscore_plot(postharmo_data, rh_precentral_thicknessstd)
zscore_plot_fs(postharmo_data, rh_precentral_thicknessstd)

zscore_plot(preharmo_data, rh_precuneus_thicknessstd)
zscore_plot(postharmo_data, rh_precuneus_thicknessstd)
zscore_plot_fs(postharmo_data, rh_precuneus_thicknessstd)

zscore_plot(preharmo_data, rh_rostralanteriorcingulate_thicknessstd)
zscore_plot(postharmo_data, rh_rostralanteriorcingulate_thicknessstd)
zscore_plot_fs(postharmo_data, rh_rostralanteriorcingulate_thicknessstd)

zscore_plot(preharmo_data, rh_rostralmiddlefrontal_thicknessstd)
zscore_plot(postharmo_data, rh_rostralmiddlefrontal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_rostralmiddlefrontal_thicknessstd)

zscore_plot(preharmo_data, rh_superiorfrontal_thicknessstd)
zscore_plot(postharmo_data, rh_superiorfrontal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_superiorfrontal_thicknessstd)

zscore_plot(preharmo_data, rh_superiorparietal_thicknessstd)
zscore_plot(postharmo_data, rh_superiorparietal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_superiorparietal_thicknessstd)

zscore_plot(preharmo_data, rh_superiortemporal_thicknessstd)
zscore_plot(postharmo_data, rh_superiortemporal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_superiortemporal_thicknessstd)

zscore_plot(preharmo_data, rh_supramarginal_thicknessstd)
zscore_plot(postharmo_data, rh_supramarginal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_supramarginal_thicknessstd)

zscore_plot(preharmo_data, rh_frontalpole_thicknessstd)
zscore_plot(postharmo_data, rh_frontalpole_thicknessstd)
zscore_plot_fs(postharmo_data, rh_frontalpole_thicknessstd)

zscore_plot(preharmo_data, rh_temporalpole_thicknessstd)
zscore_plot(postharmo_data, rh_temporalpole_thicknessstd)
zscore_plot_fs(postharmo_data, rh_temporalpole_thicknessstd)

zscore_plot(preharmo_data, rh_transversetemporal_thicknessstd)
zscore_plot(postharmo_data, rh_transversetemporal_thicknessstd)
zscore_plot_fs(postharmo_data, rh_transversetemporal_thicknessstd)

zscore_plot(preharmo_data, rh_insula_thicknessstd)
zscore_plot(postharmo_data, rh_insula_thicknessstd)
zscore_plot_fs(postharmo_data, rh_insula_thicknessstd)


