library(ggplot2)
library(dplyr)

wd <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/10_27_simple_plot"
setwd(wd)
# data_raw <- read.csv("preHarmo_LH_thickness.csv", stringsAsFactors = TRUE)
# data_harmo <- read.csv("postHarmo_LH_thickness.csv", stringsAsFactors = TRUE)

#data_raw <- read.csv("preHarmo_LH_gauscurv.csv", stringsAsFactors = TRUE)
#data_harmo <- read.csv("postHarmo_LH_gauscurv.csv", stringsAsFactors = TRUE)

data_raw <- read.csv("preHarmo_LH_curvind.csv", stringsAsFactors = TRUE)
data_harmo <- read.csv("postHarmo_LH_curvind.csv", stringsAsFactors = TRUE)

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
    suffix <- '_harmo'
  } else {
    suffix <- '_raw'
  }
  # output pre_outlier removal csv file that contains only dataset and feature column 
  # file name is like: harmo_lh_bankssts_thickness.csv
  outfile <- paste(wd, '/', feature, suffix, ".csv", sep="")
  write.csv(data_list[1], outfile, row.names = FALSE)
  
  # output post_outlier removal csv file that contains only dataset and feature column
  # file name is like: harmo_no_outlier_lh_bankssts_thickness.csv
  outfileAfrm <- paste(wd, '/',  feature, suffix, '_no_outlier', '.csv', sep="")
  write.csv(data_list[2], outfileAfrm, row.names = FALSE)
}

remove_outlier <- function(data, feature, harmoData=FALSE) {
  featureAndDataset <- data %>% select(Dataset, Age, {{feature}})
  
  aov_rst <- aov(get(feature) ~ Dataset, data = data)
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

############# 1 ##################3
gen_4_sheet(data_raw, data_harmo, 'lh_bankssts_thickness')

gg_data <- read.csv("lh_bankssts_thickness_raw.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_thickness, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("raw data") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
# has to run 'u' on console to draw scaterplot. This is a workaround for Rstudio bug; otherwise, R crash
# u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_thickness, color=Dataset)) 
v + geom_boxplot() + ggtitle("raw data") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_thickness_raw_no_outlier.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_thickness, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("raw data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_thickness, color=Dataset)) 
v + geom_boxplot() + ggtitle("raw data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_thickness_harmo.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_thickness, color=Dataset))
u <- u + geom_point(size=0.7)  + geom_smooth()+ ggtitle("Data with harmonization") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_thickness, color=Dataset)) 
v + geom_boxplot() + ggtitle("Data with harmonization") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_thickness_harmo_no_outlier.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_thickness, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("Harmonized data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_thickness, color=Dataset)) 
v + geom_boxplot() + ggtitle("Harmonized data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))


########## 2 ############

gen_4_sheet(data_raw, data_harmo, 'lh_bankssts_gauscurv')

gg_data <- read.csv("lh_bankssts_gauscurv_raw.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_gauscurv, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("raw data") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
# has to run 'u' on console to draw scaterplot. This is a workaround for Rstudio bug; otherwise, R crash
# u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_gauscurv, color=Dataset)) 
v + geom_boxplot() + ggtitle("raw data") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_gauscurv_raw_no_outlier.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_gauscurv, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("raw data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_gauscurv, color=Dataset)) 
v + geom_boxplot() + ggtitle("raw data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_gauscurv_harmo.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_gauscurv, color=Dataset))
u <- u + geom_point(size=0.7)  + geom_smooth()+ ggtitle("Data with harmonization") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_gauscurv, color=Dataset)) 
v + geom_boxplot() + ggtitle("Data with harmonization") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_gauscurv_harmo_no_outlier.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_gauscurv, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("Harmonized data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_gauscurv, color=Dataset)) 
v + geom_boxplot() + ggtitle("Harmonized data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

################  3 ####################

gen_4_sheet(data_raw, data_harmo, 'lh_bankssts_curvind')

gg_data <- read.csv("lh_bankssts_curvind_raw.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_curvind, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("raw data") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
# has to run 'u' on console to draw scaterplot. This is a workaround for Rstudio bug; otherwise, R crash
# u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_curvind, color=Dataset)) 
v + geom_boxplot() + ggtitle("raw data") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_curvind_raw_no_outlier.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_curvind, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("raw data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_curvind, color=Dataset)) 
v + geom_boxplot() + ggtitle("raw data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_curvind_harmo.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_curvind, color=Dataset))
u <- u + geom_point(size=0.7)  + geom_smooth()+ ggtitle("Data with harmonization") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_curvind, color=Dataset)) 
v + geom_boxplot() + ggtitle("Data with harmonization") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))

gg_data <- read.csv("lh_bankssts_curvind_harmo_no_outlier.csv", stringsAsFactors = TRUE)
u <- ggplot(data=gg_data, aes(x=Age, y=lh_bankssts_curvind, color=Dataset))
u <- u + geom_point(size=0.7) + geom_smooth() + ggtitle("Harmonized data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
#u
v <- ggplot(data=gg_data, aes(x=Dataset, y=lh_bankssts_curvind, color=Dataset)) 
v + geom_boxplot() + ggtitle("Harmonized data with outliers removed") + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))


#######################################



gen_4_sheet(data_raw, data_harmo, 'lh_caudalanteriorcingulate_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_caudalmiddlefrontal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_cuneus_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_entorhinal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_fusiform_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_inferiorparietal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_inferiortemporal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_isthmuscingulate_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_lateraloccipital_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_lateralorbitofrontal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_lingual_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_medialorbitofrontal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_middletemporal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_parahippocampal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_paracentral_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_parsopercularis_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_parsorbitalis_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_parstriangularis_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_pericalcarine_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_postcentral_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_posteriorcingulate_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_precentral_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_precuneus_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_rostralanteriorcingulate_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_rostralmiddlefrontal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_superiorfrontal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_superiorparietal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_superiortemporal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_supramarginal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_frontalpole_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_temporalpole_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_transversetemporal_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_insula_thickness')
gen_4_sheet(data_raw, data_harmo, 'lh_MeanThickness_thickness')
gen_4_sheet(data_raw, data_harmo, 'BrainSegVolNotVent')
gen_4_sheet(data_raw, data_harmo, 'eTIV')
gen_4_sheet(data_raw, data_harmo, 'Volume')


