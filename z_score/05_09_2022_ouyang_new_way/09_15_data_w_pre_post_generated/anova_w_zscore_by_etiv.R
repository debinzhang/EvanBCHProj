
# install.packages("XLConnect")
library(ggplot2)
library(dplyr)
library(XLConnect)

wd <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/09_15_data_w_pre_post_generated"
setwd(wd)

# data_raw <- read.csv("all_subjects_cortical_metrics_LH_curvind_09_15_2022_preHarmo.csv", stringsAsFactors = TRUE)
# data_harmo <- read.csv("all_subjects_cortical_metrics_LH_curvind_09_15_2022_postHarmo_wo_scannertype.csv", stringsAsFactors = TRUE)

data_raw <- read.csv("all_subjects_cortical_metrics_RH_curvind_09_15_2022_preHarmo.csv", stringsAsFactors = TRUE)
data_harmo <- read.csv("all_subjects_cortical_metrics_RH_curvind_09_15_2022_postHarmo_wo_scannertype.csv", stringsAsFactors = TRUE)

adjustByMeanData <- function(data, feature) {
  data1 <-data[data$eTIV != 0, ]
  adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$eTIV)))
  return(adjByMeanData)
}

getRobustZScoreByDataset <- function(data, feature, byETIV=TRUE) {
  if (byETIV) {
    eTIV_data<-adjustByMeanData(data, get(feature))
    grouped_data <- eTIV_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
    merged_data <- merge(eTIV_data, grouped_data, by="Dataset")
  } else {
    grouped_data <- data %>% group_by(Dataset) %>% summarise_at(vars({{feature}}), list(group_median = median, group_mad = mad))
    merged_data <- merge(data, grouped_data, by="Dataset")
  }

  if (byETIV) {
    zscore_data<-merged_data %>% mutate(zscore=(adjByMean - group_median)/group_mad)
  } else {
    zscore_data<-merged_data %>% mutate(zscore=(get(feature) - group_median)/group_mad)
  }
  return(zscore_data)
}

rmOutlier <- function(data, low, high) {
  #data[data$zscore<high & data$zscore>low, ]
  data_no_outlier <- subset(data, data$zscore>low & data$zscore < high)
  data_no_outlier
}

AovStats <- function(data_in, feature) {
  aov_rst <- aov(data_in$zscore ~ Dataset, data = data_in)
  print(summary(aov_rst))
  print(TukeyHSD(aov_rst))
}

gender_suffix <- function(sex=3) {
  if (sex == 1) {
    sex_suffix = "_male"
  } else if (sex==2) {
    sex_suffix = "_female"
  } else {
    sex_suffix = "_all"
  }
  return( sex_suffix)
}

sort_by_sex <- function(data, sex=3) {
  if (sex == 1) {
    sexSubset <- data %>% filter(Sex=="M" | Sex=="1")
  } else if (sex==2) {
    sexSubset <- data %>% filter(Sex=="F" | Sex=="2")
  } else {
    sexSubset <- data
  }
  return(sexSubset)
}

# sex: 1-> Male; 2->Female; 3->all
data_analyse <- function(data_raw, data_harmo, feature, gen_csv=TRUE, gen_TukeyHSD=FALSE, 
                         low=-3.5, high=3.5, sex=3, byETIV=TRUE, wb=NULL, index=0) {
  data_raw_z <- getRobustZScoreByDataset(data_raw, feature, byETIV)
  rawSubset <- data_raw_z %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(rawSubset, sex)
  raw_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  raw_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))

  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }

  data_raw_z1 <- getRobustZScoreByDataset(data_raw, feature, byETIV)
  rawNoOutlier <- rmOutlier(data_raw_z1, low, high)
  rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(rawNoOutlierSubset, sex)
  rawNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  rawNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))

  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw_no_outlier', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }
  
  data_harmo_z <- getRobustZScoreByDataset(data_harmo, feature, byETIV)
  harmoSubset <- data_harmo_z %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harm_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harm_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))

  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }
  
  data_harmo_z1 <- getRobustZScoreByDataset(data_harmo, feature, byETIV)
  harmoNoOutlier <- rmOutlier(data_harmo_z1, low, high)
  harmoNoOutlierSubset <- harmoNoOutlier %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoNoOutlierSubset, sex)
  harmoNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harmoNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))

  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo_no_outlier', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }

  if (!is.null(wb)) {
    writeWorksheet(wb, raw_Anova, sheet="workSheet", startRow=index*3+2, startCol = 3)
    writeWorksheet(wb, rawNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 9)
    writeWorksheet(wb, harm_Anova, sheet="workSheet", startRow=index*3+2, startCol = 15)
    writeWorksheet(wb, harmoNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 21)
  }
  
  print(paste("==============", feature, "=============="))

  print(summary(raw_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(raw_aov_rst))
  }
  print(summary(rawNoOutlier_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(rawNoOutlier_aov_rst))
  }
  print(summary(harm_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(harm_aov_rst))
  }
  print(summary(harmoNoOutlier_aov_rst))
  if (gen_TukeyHSD) {
    print(TukeyHSD(harmoNoOutlier_aov_rst))
  }
}

gender_title <- function(sex=3) {
  if (sex == 1) {
    sex_suffix = "(male)"
  } else if (sex==2) {
    sex_suffix = "(female)"
  } else {
    sex_suffix = "(all gender)"
  }
  return( sex_suffix)
}

# option: 1->raw; 2->raw_wo_outlier; 3->harmo; 4->harmo_wo_outlier
# sex: 1->Male; 2->Female; 3->All gender
draw_plot <- function(feature, option=1, sex=3) {
  if (option==1) {
    suffix <- "raw"
    title <- paste("Raw", feature, gender_title(sex), sep=" ")
  } else if (option==2) {
    suffix <- "raw_no_outlier"
    title <- paste(feature, "with outlier removed", gender_title(sex), sep=" ")
  } else if (option==3) {
    suffix <- "harmo"
    title <- paste("Harmonized", feature, gender_title(sex), sep=" ")
  } else if (option==4) {
    suffix <- "harmo_no_outlier"
    title <- paste("Harmonized", feature, "with outlier removed", gender_title(sex), sep=" ")
  } else {
    print("invalid option")
    return()
  }
  file_path <- paste(wd, '/', feature, '_', suffix, gender_suffix(sex), '.csv', sep = "")
  print(file_path)
  gg_data0 <- read.csv(file_path, stringsAsFactors = FALSE)
  gg_data <- gg_data0 %>% filter(Age !="")

  u <- ggplot(data=gg_data, aes(x=as.numeric(Age), y=get(feature), color=Dataset))
  u <- u + geom_point(size=0.5) + 
      #geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5)) +
      geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=5)) + 
      ggtitle(title) + ylab(feature)+ xlab('Age') 
  u <- u + theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=5))
  u <- u + scale_x_continuous(breaks = seq(0,100, by =2))
  #u <- u + scale_x_discrete(breaks = seq(0,100, by =2))
  u
}

set_anova_skeleton <- function(wb, region_list) {
  # Each region take three rows. The 1st row is region name, followed by 2 empty rows
  region.df <- data.frame(Region=c(''))
  writeWorksheet(wb, region.df, sheet="workSheet", startRow=1, startCol = 1)
  index <- 0
  for (region_name in region_list) {
    region.df <- data.frame(place_holder=c(''))
    colnames(region.df) <- c(region_name)
    writeWorksheet(wb, region.df, sheet="workSheet", startRow=index*3+2, startCol = 1)
    index <- index + 1
  }

  Raw.df <- data.frame(Raw=c(''))
  Raw_no_outlier.df <- data.frame(Raw_wo_outliers=c(''))
  harmo.df <- data.frame(Harmonized=c(''))
  harmo_no_outlier.df <- data.frame(Harmonized_wo_outliers=c(''))
  writeWorksheet(wb, Raw.df, sheet="workSheet", startRow=1, startCol = 3)
  writeWorksheet(wb, Raw_no_outlier.df, sheet="workSheet", startRow=1, startCol = 9)
  writeWorksheet(wb, harmo.df, sheet="workSheet", startRow=1, startCol = 15)
  writeWorksheet(wb, harmo_no_outlier.df, sheet="workSheet", startRow=1, startCol = 21)
}

gen_4_sheet_all_regions <- function(data_raw=NULL, data_harm=NULL, sex=3, byETIV=TRUE,
                                    region_list=lh_thickness_region_list,
                                    outfile="lh_thickness_anova.xlsx") {
  # generate skeleton anova output file
  wb <- loadWorkbook(outfile, create = TRUE)
  createSheet(wb, name="workSheet")
  set_anova_skeleton(wb, region_list)
  
  index <- 0
  for (region_name in region_list) {
    data_analyse(data_raw, data_harmo, region_name, TRUE, FALSE, -3.5, 3.5,
                 sex, byETIV, wb, index)
    index <- index + 1
  }
  
  saveWorkbook(wb)

  print("All Done!!!")
}

gen_plot_all_region <- function(region_list, sex) {
  option_list <- list("_raw", "_raw_no_outlier", "_harmo", "_harmo_no_outlier")
  for (region_name in region_list) {
    for (option in 1:4) {
      draw_plot(region_name, option, sex)
      file_name = paste(region_name, option_list[option], '.png', sep='')
      print(paste("generating:", file_name))
      ggsave(file_name)
    }
  }
}


lh_curvind_region_list <- list("lh_bankssts_curvind",
                               "lh_caudalanteriorcingulate_curvind",
                               "lh_caudalmiddlefrontal_curvind",
                               "lh_cuneus_curvind",
                               "lh_entorhinal_curvind",
                               "lh_fusiform_curvind",
                               "lh_inferiorparietal_curvind",
                               "lh_inferiortemporal_curvind",
                               "lh_isthmuscingulate_curvind",
                               "lh_lateraloccipital_curvind",
                               "lh_lateralorbitofrontal_curvind",
                               "lh_lingual_curvind",
                               "lh_medialorbitofrontal_curvind",
                               "lh_middletemporal_curvind",
                               "lh_parahippocampal_curvind",
                               "lh_paracentral_curvind",
                               "lh_parsopercularis_curvind",
                               "lh_parsorbitalis_curvind",
                               "lh_parstriangularis_curvind",
                               "lh_pericalcarine_curvind",
                               "lh_postcentral_curvind",
                               "lh_posteriorcingulate_curvind",
                               "lh_precentral_curvind",
                               "lh_precuneus_curvind",
                               "lh_rostralanteriorcingulate_curvind",
                               "lh_rostralmiddlefrontal_curvind",
                               "lh_superiorfrontal_curvind",
                               "lh_superiorparietal_curvind",
                               "lh_superiortemporal_curvind",
                               "lh_supramarginal_curvind",
                               "lh_frontalpole_curvind",
                               "lh_temporalpole_curvind",
                               "lh_transversetemporal_curvind",
                               "lh_insula_curvind")

rh_curvind_region_list <- list("rh_bankssts_curvind",
                               "rh_caudalanteriorcingulate_curvind",
                               "rh_caudalmiddlefrontal_curvind",
                               "rh_cuneus_curvind",
                               "rh_entorhinal_curvind",
                               "rh_fusiform_curvind",
                               "rh_inferiorparietal_curvind",
                               "rh_inferiortemporal_curvind",
                               "rh_isthmuscingulate_curvind",
                               "rh_lateraloccipital_curvind",
                               "rh_lateralorbitofrontal_curvind",
                               "rh_lingual_curvind",
                               "rh_medialorbitofrontal_curvind",
                               "rh_middletemporal_curvind",
                               "rh_parahippocampal_curvind",
                               "rh_paracentral_curvind",
                               "rh_parsopercularis_curvind",
                               "rh_parsorbitalis_curvind",
                               "rh_parstriangularis_curvind",
                               "rh_pericalcarine_curvind",
                               "rh_postcentral_curvind",
                               "rh_posteriorcingulate_curvind",
                               "rh_precentral_curvind",
                               "rh_precuneus_curvind",
                               "rh_rostralanteriorcingulate_curvind",
                               "rh_rostralmiddlefrontal_curvind",
                               "rh_superiorfrontal_curvind",
                               "rh_superiorparietal_curvind",
                               "rh_superiortemporal_curvind",
                               "rh_supramarginal_curvind",
                               "rh_frontalpole_curvind",
                               "rh_temporalpole_curvind",
                               "rh_transversetemporal_curvind",
                               "rh_insula_curvind")

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_curvind_region_list, outfile="lh_curvind_anova.xlsx")

gen_plot_all_region(lh_curvind_region_list, 3)



gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_curvind_region_list, outfile="rh_curvind_anova.xlsx")

gen_plot_all_region(rh_curvind_region_list, 3)











data_analyse(data_raw, data_harmo, 'lh_inferiorparietal_curvind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=1, byETIV=TRUE)
data_analyse(data_raw, data_harmo, 'lh_inferiorparietal_curvind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=2, byETIV=TRUE)
data_analyse(data_raw, data_harmo, 'lh_inferiorparietal_curvind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3, byETIV=TRUE)
data_analyse(data_raw, data_harmo, 'lh_cuneus_curvind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3, byETIV=TRUE)
data_analyse(data_raw, data_harmo, 'lh_entorhinal_curvind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3, byETIV=TRUE)

data_analyse(data_raw, data_harmo, 'lh_cuneus_foldind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3, byETIV=TRUE)

data_analyse(data_raw, data_harmo, 'lh_entorhinal_foldind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3, byETIV=TRUE)
data_analyse(data_raw, data_harmo, 'lh_inferiorparietal_foldind', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3, byETIV=TRUE)

x<-draw_plot('lh_inferiorparietal_curvind', option=1, sex=1)
x<-draw_plot('lh_inferiorparietal_curvind', option=1, sex=2)
x<-draw_plot('lh_inferiorparietal_curvind', option=1, sex=3)
x<-draw_plot('lh_inferiorparietal_curvind', option=2, sex=1)
x<-draw_plot('lh_inferiorparietal_curvind', option=2, sex=2)
x<-draw_plot('lh_inferiorparietal_curvind', option=2, sex=3)
x<-draw_plot('lh_inferiorparietal_curvind', option=3, sex=1)
x<-draw_plot('lh_inferiorparietal_curvind', option=3, sex=2)
x<-draw_plot('lh_inferiorparietal_curvind', option=3, sex=3)
x<-draw_plot('lh_inferiorparietal_curvind', option=4, sex=1)
x<-draw_plot('lh_inferiorparietal_curvind', option=4, sex=2)
x<-draw_plot('lh_inferiorparietal_curvind', option=4, sex=3)

data_analyse(data_raw, data_harmo, 'rh_inferiorparietal_thickness', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=1, byETIV=TRUE)
data_analyse(data_raw, data_harmo, 'rh_inferiorparietal_thickness', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=2, byETIV=TRUE)
data_analyse(data_raw, data_harmo, 'rh_inferiorparietal_thickness', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3, byETIV=TRUE)


data_analyse(data_raw, data_harmo, 'rh_inferiorparietal_thickness', gen_csv = TRUE)
draw_plot('rh_inferiorparietal_thickness')

data_analyse(data_raw, data_harmo, 'rh_superiorfrontal_thickness')

data_analyse(data_raw, data_harmo, 'lh_caudalanteriorcingulate_thickness')
data_analyse(data_raw, data_harmo, 'lh_caudalmiddlefrontal_thickness')
data_analyse(data_raw, data_harmo, 'lh_cuneus_thickness')
data_analyse(data_raw, data_harmo, 'lh_entorhinal_thickness')
data_analyse(data_raw, data_harmo, 'lh_fusiform_thickness')
data_analyse(data_raw, data_harmo, 'lh_inferiorparietal_thickness')
data_analyse(data_raw, data_harmo, 'lh_inferiortemporal_thickness')
data_analyse(data_raw, data_harmo, 'lh_isthmuscingulate_thickness')
data_analyse(data_raw, data_harmo, 'lh_lateraloccipital_thickness')
data_analyse(data_raw, data_harmo, 'lh_lateralorbitofrontal_thickness')
data_analyse(data_raw, data_harmo, 'lh_lingual_thickness')
data_analyse(data_raw, data_harmo, 'lh_medialorbitofrontal_thickness')
data_analyse(data_raw, data_harmo, 'lh_middletemporal_thickness')
data_analyse(data_raw, data_harmo, 'lh_parahippocampal_thickness')
data_analyse(data_raw, data_harmo, 'lh_paracentral_thickness')
data_analyse(data_raw, data_harmo, 'lh_parsopercularis_thickness')
data_analyse(data_raw, data_harmo, 'lh_parsorbitalis_thickness')
data_analyse(data_raw, data_harmo, 'lh_parstriangularis_thickness')
data_analyse(data_raw, data_harmo, 'lh_pericalcarine_thickness')
data_analyse(data_raw, data_harmo, 'lh_postcentral_thickness')
data_analyse(data_raw, data_harmo, 'lh_posteriorcingulate_thickness')
data_analyse(data_raw, data_harmo, 'lh_precentral_thickness')
data_analyse(data_raw, data_harmo, 'lh_precuneus_thickness')
data_analyse(data_raw, data_harmo, 'lh_rostralanteriorcingulate_thickness')
data_analyse(data_raw, data_harmo, 'lh_rostralmiddlefrontal_thickness')
data_analyse(data_raw, data_harmo, 'lh_superiorfrontal_thickness')
data_analyse(data_raw, data_harmo, 'lh_superiorparietal_thickness')
data_analyse(data_raw, data_harmo, 'lh_superiortemporal_thickness')
data_analyse(data_raw, data_harmo, 'lh_supramarginal_thickness')
data_analyse(data_raw, data_harmo, 'lh_frontalpole_thickness')
data_analyse(data_raw, data_harmo, 'lh_temporalpole_thickness')
data_analyse(data_raw, data_harmo, 'lh_transversetemporal_thickness')
data_analyse(data_raw, data_harmo, 'lh_insula_thickness')
data_analyse(data_raw, data_harmo, 'lh_MeanThickness_thickness')
data_analyse(data_raw, data_harmo, 'BrainSegVolNotVent')
data_analyse(data_raw, data_harmo, 'eTIV')
data_analyse(data_raw, data_harmo, 'Volume')


