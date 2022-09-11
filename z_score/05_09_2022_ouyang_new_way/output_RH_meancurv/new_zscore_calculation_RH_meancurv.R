library(ggplot2)
#library(tidyverse)
#install.packages("dplyr")
library(dplyr)

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/output_RH_meancurv")

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

zscore_plot(preharmo_data, rh_bankssts_meancurv)
zscore_plot(postharmo_data, rh_bankssts_meancurv)
zscore_plot_fs(postharmo_data, rh_bankssts_meancurv)

zscore_plot(preharmo_data, rh_caudalanteriorcingulate_meancurv)
zscore_plot(postharmo_data, rh_caudalanteriorcingulate_meancurv)
zscore_plot_fs(postharmo_data, rh_caudalanteriorcingulate_meancurv)

zscore_plot(preharmo_data, rh_caudalmiddlefrontal_meancurv)
zscore_plot(postharmo_data, rh_caudalmiddlefrontal_meancurv)
zscore_plot_fs(postharmo_data, rh_caudalmiddlefrontal_meancurv)

zscore_plot(preharmo_data, rh_cuneus_meancurv)
zscore_plot(postharmo_data, rh_cuneus_meancurv)
zscore_plot_fs(postharmo_data, rh_cuneus_meancurv)

zscore_plot(preharmo_data, rh_entorhinal_meancurv)
zscore_plot(postharmo_data, rh_entorhinal_meancurv)
zscore_plot_fs(postharmo_data, rh_entorhinal_meancurv)

zscore_plot(preharmo_data, rh_fusiform_meancurv)
zscore_plot(postharmo_data, rh_fusiform_meancurv)
zscore_plot_fs(postharmo_data, rh_fusiform_meancurv)

zscore_plot(preharmo_data, rh_inferiorparietal_meancurv)
zscore_plot(postharmo_data, rh_inferiorparietal_meancurv)
zscore_plot_fs(postharmo_data, rh_inferiorparietal_meancurv)

zscore_plot(preharmo_data, rh_inferiortemporal_meancurv)
zscore_plot(postharmo_data, rh_inferiortemporal_meancurv)
zscore_plot_fs(postharmo_data, rh_inferiortemporal_meancurv)

zscore_plot(preharmo_data, rh_isthmuscingulate_meancurv)
zscore_plot(postharmo_data, rh_isthmuscingulate_meancurv)
zscore_plot_fs(postharmo_data, rh_isthmuscingulate_meancurv)

zscore_plot(preharmo_data, rh_lateraloccipital_meancurv)
zscore_plot(postharmo_data, rh_lateraloccipital_meancurv)
zscore_plot_fs(postharmo_data, rh_lateraloccipital_meancurv)

zscore_plot(preharmo_data, rh_lateralorbitofrontal_meancurv)
zscore_plot(postharmo_data, rh_lateralorbitofrontal_meancurv)
zscore_plot_fs(postharmo_data, rh_lateralorbitofrontal_meancurv)

zscore_plot(preharmo_data, rh_lingual_meancurv)
zscore_plot(postharmo_data, rh_lingual_meancurv)
zscore_plot_fs(postharmo_data, rh_lingual_meancurv)

zscore_plot(preharmo_data, rh_medialorbitofrontal_meancurv)
zscore_plot(postharmo_data, rh_medialorbitofrontal_meancurv)
zscore_plot_fs(postharmo_data, rh_medialorbitofrontal_meancurv)

zscore_plot(preharmo_data, rh_middletemporal_meancurv)
zscore_plot(postharmo_data, rh_middletemporal_meancurv)
zscore_plot_fs(postharmo_data, rh_middletemporal_meancurv)

zscore_plot(preharmo_data, rh_parahippocampal_meancurv)
zscore_plot(postharmo_data, rh_parahippocampal_meancurv)
zscore_plot_fs(postharmo_data, rh_parahippocampal_meancurv)

zscore_plot(preharmo_data, rh_paracentral_meancurv)
zscore_plot(postharmo_data, rh_paracentral_meancurv)
zscore_plot_fs(postharmo_data, rh_paracentral_meancurv)

zscore_plot(preharmo_data, rh_parsopercularis_meancurv)
zscore_plot(postharmo_data, rh_parsopercularis_meancurv)
zscore_plot_fs(postharmo_data, rh_parsopercularis_meancurv)

zscore_plot(preharmo_data, rh_parsorbitalis_meancurv)
zscore_plot(postharmo_data, rh_parsorbitalis_meancurv)
zscore_plot_fs(postharmo_data, rh_parsorbitalis_meancurv)

zscore_plot(preharmo_data, rh_parstriangularis_meancurv)
zscore_plot(postharmo_data, rh_parstriangularis_meancurv)
zscore_plot_fs(postharmo_data, rh_parstriangularis_meancurv)

zscore_plot(preharmo_data, rh_pericalcarine_meancurv)
zscore_plot(postharmo_data, rh_pericalcarine_meancurv)
zscore_plot_fs(postharmo_data, rh_pericalcarine_meancurv)

zscore_plot(preharmo_data, rh_postcentral_meancurv)
zscore_plot(postharmo_data, rh_postcentral_meancurv)
zscore_plot_fs(postharmo_data, rh_postcentral_meancurv)

zscore_plot(preharmo_data, rh_posteriorcingulate_meancurv)
zscore_plot(postharmo_data, rh_posteriorcingulate_meancurv)
zscore_plot_fs(postharmo_data, rh_posteriorcingulate_meancurv)

zscore_plot(preharmo_data, rh_precentral_meancurv)
zscore_plot(postharmo_data, rh_precentral_meancurv)
zscore_plot_fs(postharmo_data, rh_precentral_meancurv)

zscore_plot(preharmo_data, rh_precuneus_meancurv)
zscore_plot(postharmo_data, rh_precuneus_meancurv)
zscore_plot_fs(postharmo_data, rh_precuneus_meancurv)

zscore_plot(preharmo_data, rh_rostralanteriorcingulate_meancurv)
zscore_plot(postharmo_data, rh_rostralanteriorcingulate_meancurv)
zscore_plot_fs(postharmo_data, rh_rostralanteriorcingulate_meancurv)

zscore_plot(preharmo_data, rh_rostralmiddlefrontal_meancurv)
zscore_plot(postharmo_data, rh_rostralmiddlefrontal_meancurv)
zscore_plot_fs(postharmo_data, rh_rostralmiddlefrontal_meancurv)

zscore_plot(preharmo_data, rh_superiorfrontal_meancurv)
zscore_plot(postharmo_data, rh_superiorfrontal_meancurv)
zscore_plot_fs(postharmo_data, rh_superiorfrontal_meancurv)

zscore_plot(preharmo_data, rh_superiorparietal_meancurv)
zscore_plot(postharmo_data, rh_superiorparietal_meancurv)
zscore_plot_fs(postharmo_data, rh_superiorparietal_meancurv)

zscore_plot(preharmo_data, rh_superiortemporal_meancurv)
zscore_plot(postharmo_data, rh_superiortemporal_meancurv)
zscore_plot_fs(postharmo_data, rh_superiortemporal_meancurv)

zscore_plot(preharmo_data, rh_supramarginal_meancurv)
zscore_plot(postharmo_data, rh_supramarginal_meancurv)
zscore_plot_fs(postharmo_data, rh_supramarginal_meancurv)

zscore_plot(preharmo_data, rh_frontalpole_meancurv)
zscore_plot(postharmo_data, rh_frontalpole_meancurv)
zscore_plot_fs(postharmo_data, rh_frontalpole_meancurv)

zscore_plot(preharmo_data, rh_temporalpole_meancurv)
zscore_plot(postharmo_data, rh_temporalpole_meancurv)
zscore_plot_fs(postharmo_data, rh_temporalpole_meancurv)

zscore_plot(preharmo_data, rh_transversetemporal_meancurv)
zscore_plot(postharmo_data, rh_transversetemporal_meancurv)
zscore_plot_fs(postharmo_data, rh_transversetemporal_meancurv)

zscore_plot(preharmo_data, rh_insula_meancurv)
zscore_plot(postharmo_data, rh_insula_meancurv)
zscore_plot_fs(postharmo_data, rh_insula_meancurv)


