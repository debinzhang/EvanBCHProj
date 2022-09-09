library(ggplot2)
#library(tidyverse)
#install.packages("dplyr")
library(dplyr)

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way")
#test_data <- read.csv("test_data.csv", stringsAsFactors = TRUE)
#test_data

#real_data <- read.csv("all_subjects_cortical_metrics_RH_thickness_09_02_2022_w_dataset.csv", stringsAsFactors = TRUE)
preharmo_data <- read.csv("preHarmo.csv", stringsAsFactors = TRUE)
postharmo_data <- read.csv("PostHarmon_all_test.csv", stringsAsFactors = TRUE)

adjustByMeanData <- function(data, feature) {
  data1 <-data[data$rh_MeanThickness_thickness != 0, ]
  adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$rh_MeanThickness_thickness))) %>% select(Dataset, {{feature}}, adjByMean, rh_MeanThickness_thickness)
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

# this section is for testing only
t1<-adjustByMeanData(test_data, rh_bankssts_thickness)
t2<-getZScore(t1)
t3<-rmOutlier(t2,-1.0,1.0)
u<-ggplot(t3, aes(x=Dataset, y=zscore))
u + geom_boxplot()

t10<-adjustByMeanData(real_data, rh_bankssts_thickness)
t11<-getZScore(t10)
t12<-rmOutlier(t11,-3.0,3.0)
u<-ggplot(t12, aes(x=Dataset, y=zscore))
u + geom_boxplot()
#end of testing function

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

zscore_plot(preharmo_data, rh_entorhinal_thickness)
zscore_plot(postharmo_data, rh_entorhinal_thickness)

zscore_plot(preharmo_data, rh_fusiform_thickness)
zscore_plot(postharmo_data, rh_fusiform_thickness)

zscore_plot(preharmo_data, rh_inferiorparietal_thickness)
zscore_plot(postharmo_data, rh_inferiorparietal_thickness)

zscore_plot(preharmo_data, rh_inferiortemporal_thickness)
zscore_plot(postharmo_data, rh_inferiortemporal_thickness)

zscore_plot(preharmo_data, rh_isthmuscingulate_thickness)
zscore_plot(postharmo_data, rh_isthmuscingulate_thickness)

zscore_plot(preharmo_data, rh_lateraloccipital_thickness)
zscore_plot(postharmo_data, rh_lateraloccipital_thickness)

zscore_plot(preharmo_data, rh_lateralorbitofrontal_thickness)
zscore_plot(postharmo_data, rh_lateralorbitofrontal_thickness)

zscore_plot(preharmo_data, rh_lingual_thickness)
zscore_plot(postharmo_data, rh_lingual_thickness)

zscore_plot(preharmo_data, rh_medialorbitofrontal_thickness)
zscore_plot(postharmo_data, rh_medialorbitofrontal_thickness)

zscore_plot(preharmo_data, rh_middletemporal_thickness)
zscore_plot(postharmo_data, rh_middletemporal_thickness)

zscore_plot(preharmo_data, rh_parahippocampal_thickness)
zscore_plot(postharmo_data, rh_parahippocampal_thickness)

zscore_plot(preharmo_data, rh_paracentral_thickness)
zscore_plot(postharmo_data, rh_paracentral_thickness)

zscore_plot(preharmo_data, rh_parsopercularis_thickness)
zscore_plot(postharmo_data, rh_parsopercularis_thickness)

zscore_plot(preharmo_data, rh_parsorbitalis_thickness)
zscore_plot(postharmo_data, rh_parsorbitalis_thickness)

zscore_plot(preharmo_data, rh_parstriangularis_thickness)
zscore_plot(postharmo_data, rh_parstriangularis_thickness)

zscore_plot(preharmo_data, rh_pericalcarine_thickness)
zscore_plot(postharmo_data, rh_pericalcarine_thickness)

zscore_plot(preharmo_data, rh_postcentral_thickness)
zscore_plot(postharmo_data, rh_postcentral_thickness)

zscore_plot(preharmo_data, rh_posteriorcingulate_thickness)
zscore_plot(postharmo_data, rh_posteriorcingulate_thickness)

zscore_plot(preharmo_data, rh_precentral_thickness)
zscore_plot(postharmo_data, rh_precentral_thickness)

zscore_plot(preharmo_data, rh_precuneus_thickness)
zscore_plot(postharmo_data, rh_precuneus_thickness)

zscore_plot(preharmo_data, rh_rostralanteriorcingulate_thickness)
zscore_plot(postharmo_data, rh_rostralanteriorcingulate_thickness)

zscore_plot(preharmo_data, rh_rostralmiddlefrontal_thickness)
zscore_plot(postharmo_data, rh_rostralmiddlefrontal_thickness)

zscore_plot(preharmo_data, rh_superiorfrontal_thickness)
zscore_plot(postharmo_data, rh_superiorfrontal_thickness)

zscore_plot(preharmo_data, rh_superiorparietal_thickness)
zscore_plot(postharmo_data, rh_superiorparietal_thickness)

zscore_plot(preharmo_data, rh_superiortemporal_thickness)
zscore_plot(postharmo_data, rh_superiortemporal_thickness)

zscore_plot(preharmo_data, rh_supramarginal_thickness)
zscore_plot(postharmo_data, rh_supramarginal_thickness)

zscore_plot(preharmo_data, rh_frontalpole_thickness)
zscore_plot(postharmo_data, rh_frontalpole_thickness)

zscore_plot(preharmo_data, rh_temporalpole_thickness)
zscore_plot(postharmo_data, rh_temporalpole_thickness)

zscore_plot(preharmo_data, rh_transversetemporal_thickness)
zscore_plot(postharmo_data, rh_transversetemporal_thickness)

zscore_plot(preharmo_data, rh_insula_thickness)
zscore_plot(postharmo_data, rh_insula_thickness)



