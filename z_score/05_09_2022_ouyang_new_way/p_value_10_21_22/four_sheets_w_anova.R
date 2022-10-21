library(ggplot2)
library(dplyr)

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/p_value_10_21_22")
data_raw <- read.csv("preHarmo_LH_thickness.csv", stringsAsFactors = TRUE)
data_post <- read.csv("postHarmo_LH_thickness.csv", stringsAsFactors = TRUE)

featureAndDataset <- data_raw %>% select(Dataset, lh_inferiortemporal_thickness)
write.csv(featureAndDataset, "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/p_value_10_21_22/raw_lh_inferiortemporal_thickness.csv", row.names = FALSE)

quartiles <- quantile(featureAndDataset$lh_inferiortemporal_thickness, probs=c(.25, .75), na.rm = FALSE)
IQR <-  IQR(featureAndDataset$lh_inferiortemporal_thickness)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
data_no_outlier <- subset(featureAndDataset, featureAndDataset$lh_inferiortemporal_thickness>Lower & 
                            featureAndDataset$lh_inferiortemporal_thickness < Upper)
write.csv(data_no_outlier, "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/p_value_10_21_22/raw_no_outlier_lh_inferiortemporal_thickness.csv", row.names = FALSE)


featureAndDataset1 <- data_post %>% select(Dataset, lh_inferiortemporal_thickness)
write.csv(featureAndDataset, "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/p_value_10_21_22/post_lh_inferiortemporal_thickness.csv", row.names = FALSE)

quartiles1 <- quantile(featureAndDataset1$lh_inferiortemporal_thickness, probs=c(.25, .75), na.rm = FALSE)
IQR1 <-  IQR(featureAndDataset1$lh_inferiortemporal_thickness)
Lower1 <- quartiles1[1] - 1.5*IQR1
Upper1 <- quartiles1[2] + 1.5*IQR1
data_no_outlier1 <- subset(featureAndDataset, featureAndDataset1$lh_inferiortemporal_thickness>Lower1 & 
                            featureAndDataset$lh_inferiortemporal_thickness < Upper1)
write.csv(data_no_outlier1, "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/p_value_10_21_22/post_no_outlier_lh_inferiortemporal_thickness.csv", row.names = FALSE)


raw.lh_inferiortemporal_thickness.aov <- aov(lh_inferiortemporal_thickness ~ Dataset, data = data_raw)
raw.no_outlier.lh_inferiortemporal_thickness.aov <- aov(lh_inferiortemporal_thickness ~ Dataset, data = data_no_outlier)
post.lh_inferiortemporal_thickness.aov <- aov(lh_inferiortemporal_thickness ~ Dataset, data = data_post)
post.no_outlier.lh_inferiortemporal_thickness.aov <- aov(lh_inferiortemporal_thickness ~ Dataset, data = data_no_outlier1)

write.csv(data_raw, "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/p_value_10_21_22/raw_lh_inferiortemporal_thickness.csv", row.names = FALSE)


summary(raw.lh_inferiortemporal_thickness.aov)
summary(raw.no_outlier.lh_inferiortemporal_thickness.aov)
summary(post.lh_inferiortemporal_thickness.aov)
summary(post.no_outlier.lh_inferiortemporal_thickness.aov)
