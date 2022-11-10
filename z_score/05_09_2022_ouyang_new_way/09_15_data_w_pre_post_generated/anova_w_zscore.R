library(ggplot2)
library(dplyr)

wd <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/09_15_data_w_pre_post_generated"
setwd(wd)
data_raw <- read.csv("all_subjects_cortical_metrics_RH_thickness_09_15_2022_preHarmo.csv", stringsAsFactors = TRUE)
data_harmo <- read.csv("all_subjects_cortical_metrics_RH_thickness_09_15_2022_postHarmo_1.csv", stringsAsFactors = TRUE)

getRobustZScoreByDataset <- function(data, feature) {
  grouped_data <- data %>% group_by(Dataset) %>% summarise_at(vars({{feature}}), list(group_median = median, group_mad = mad))
  merged_data <- merge(data, grouped_data, by="Dataset")
  zscore_data<-merged_data %>% mutate(zscore=(get(feature) - group_median)/group_mad)
  zscore_data
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
data_analyse <- function(data_raw, data_harmo, feature, gen_csv=TRUE, gen_TukeyHSD=FALSE, low=-3.5, high=3.5, sex=3) {
  data_raw_z <- getRobustZScoreByDataset(data_raw, feature)
  rawSubset <- data_raw_z %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(rawSubset, sex)
  raw_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }

  data_raw_z1 <- getRobustZScoreByDataset(data_raw, feature)
  rawNoOutlier <- rmOutlier(data_raw_z1, low, high)
  rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(rawNoOutlierSubset, sex)
  rawNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw_no_outlier', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }
  
  data_harmo_z <- getRobustZScoreByDataset(data_harmo, feature)
  harmoSubset <- data_harmo_z %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harm_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }
  
  data_harmo_z1 <- getRobustZScoreByDataset(data_harmo, feature)
  harmoNoOutlier <- rmOutlier(data_harmo_z1, low, high)
  harmoNoOutlierSubset <- harmoNoOutlier %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoNoOutlierSubset, sex)
  harmoNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo_no_outlier', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE)
  }

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
      geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5)) + 
      #geom_smooth(method = "loess") +
      ggtitle(title) + ylab(feature)+ theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5))
  u <- u + scale_x_continuous(breaks = seq(0,100, by =2))
  #u <- u + scale_x_discrete(breaks = seq(0,100, by =2))
  u
}


data_analyse(data_raw, data_harmo, 'rh_bankssts_thickness', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=1)
data_analyse(data_raw, data_harmo, 'rh_bankssts_thickness', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=2)
data_analyse(data_raw, data_harmo, 'rh_bankssts_thickness', gen_TukeyHSD=FALSE, gen_csv=TRUE, sex=3)

x<-draw_plot('rh_bankssts_thickness', option=1, sex=1)
x<-draw_plot('rh_bankssts_thickness', option=1, sex=2)
x<-draw_plot('rh_bankssts_thickness', option=1, sex=3)
x<-draw_plot('rh_bankssts_thickness', option=2, sex=1)
x<-draw_plot('rh_bankssts_thickness', option=2, sex=2)
x<-draw_plot('rh_bankssts_thickness', option=2, sex=3)
x<-draw_plot('rh_bankssts_thickness', option=3, sex=1)
x<-draw_plot('rh_bankssts_thickness', option=3, sex=2)
x<-draw_plot('rh_bankssts_thickness', option=3, sex=3)
x<-draw_plot('rh_bankssts_thickness', option=4, sex=1)
x<-draw_plot('rh_bankssts_thickness', option=4, sex=2)
x<-draw_plot('rh_bankssts_thickness', option=4, sex=3)

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


