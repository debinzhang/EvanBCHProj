# install.packages("XLConnect")
#install.packages("reticulate")
library(ggplot2)
library(dplyr)
library(XLConnect)

library(VGAM)
library(reshape2)
library(reticulate)  # this package enable python in R

use_python("/usr/local/bin/python3")
source_python("harmonDataBuilder_9_abcd_R.py")

wd <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/12_01_reorder_harmo/"
setwd(wd)
# data_raw <- read.csv("all_subjects_cortical_metrics_LH_thickness_09_15_2022_preHarmo.csv", stringsAsFactors = TRUE)
# data_harmo <- read.csv("all_subjects_cortical_metrics_LH_thickness_09_15_2022_postHarmo_wo_scannertype.csv", stringsAsFactors = TRUE)

data_raw <- read.csv("all_subjects_cortical_metrics_RH_thickness_09_15_2022_preHarmo.csv", stringsAsFactors = TRUE)
data_harmo <- read.csv("all_subjects_cortical_metrics_RH_thickness_09_15_2022_postHarmo_wo_scannertype.csv", stringsAsFactors = TRUE)


adjustByMeanData <- function(data, feature, IsLH=TRUE) {
  if (IsLH) {
    data1 <-data[data$lh_MeanThickness_thickness != 0, ]
    adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$lh_MeanThickness_thickness)))
  } else {
    data1 <-data[data$rh_MeanThickness_thickness != 0, ]
    adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$rh_MeanThickness_thickness)))
  }
    return(adjByMeanData)
}

getRobustZScoreByDataset <- function(data, feature, byMeanThick=TRUE, IsLH=TRUE) {
  if (byMeanThick) {
    if (IsLH) {
      lh_MeanThickness_thickness_data<-adjustByMeanData(data, get(feature), IsLH)
      grouped_data <- lh_MeanThickness_thickness_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
      merged_data <- merge(lh_MeanThickness_thickness_data, grouped_data, by="Dataset")
    } else {
      rh_MeanThickness_thickness_data<-adjustByMeanData(data, get(feature), IsLH)
      grouped_data <- rh_MeanThickness_thickness_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
      merged_data <- merge(rh_MeanThickness_thickness_data, grouped_data, by="Dataset")
    }
  } else {
    grouped_data <- data %>% group_by(Dataset) %>% summarise_at(vars({{feature}}), list(group_median = median, group_mad = mad))
    merged_data <- merge(data, grouped_data, by="Dataset")
  }
 
  if (byMeanThick) {
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
                         low=-3.5, high=3.5, sex=3, byMeanThick=TRUE, wb=NULL, index=0, IsLH=TRUE) {
  print("Debin 1")
  # step1: generation raw file
  data_raw_z <- getRobustZScoreByDataset(data_raw, feature, byMeanThick, IsLH)
  rawSubset <- data_raw_z %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, zscore, {{feature}})
  sexSubset <- sort_by_sex(rawSubset, sex)
  print(paste("raw: # of row:", nrow(sexSubset)))
  raw_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  raw_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  print("Debin 2")
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }

  print("Debin 3.0")
  # step2: generation raw_no_outlier file
  data_raw_z1 <- getRobustZScoreByDataset(data_raw, feature, byMeanThick, IsLH)
  rawNoOutlier <- rmOutlier(data_raw_z1, low, high)
  if (IsLH) {
    rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, lh_MeanThickness_thickness, zscore, {{feature}})
  } else {
    rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, rh_MeanThickness_thickness, zscore, {{feature}})
  }
  sexSubset <- sort_by_sex(rawNoOutlierSubset, sex)
  print(paste("raw_no: # of row:", nrow(sexSubset)))
  rawNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  rawNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  print("Debin 4")
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
    # Note: the output raw_no_outlier file contains zscore column. But that zscore coloumn will be ignored during later on harmonization step
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }
  print("Debin 4.1")
  # # step3: generating harmon file. Note: this harmon file is based on raw (before outlier removal) data 
  data_harmo_z <- getRobustZScoreByDataset(data_harmo, feature, byMeanThick, IsLH)
  harmoSubset <- data_harmo_z %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoSubset, sex)
  print(paste("harmo: # of row:", nrow(sexSubset)))
  harm_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harm_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  print("Debin 4.2")
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }
  
  print("Debin 4.3")
  # step4: generation harmo_no_outlier file. This is based on the step 2 output.
  src_no_file <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
  outfile <- paste(wd, '/', feature, '_harmo_no', gender_suffix(sex), '.csv', sep="")
  rslt <- harmonize(src_no_file, outfile)
  print("Debin 4.4")
  if (!rslt) {
    print(paste("Harmonization failed for", feature))
    return(FALSE)
  }

  print("Debin 4.5")
  data_harmo_no <- read.csv(outfile, stringsAsFactors = TRUE)
  print(head(data_harmo_no, n=3))
  data_harmo_z1 <- getRobustZScoreByDataset(data_harmo_no, feature, byMeanThick, IsLH)
  # harmoNoOutlier <- rmOutlier(data_harmo_z1, low, high)
  harmoSubset <-  data_harmo_z1 %>% select(Dataset, Age, Sex, zscore, {{feature}})
  #sexSubset <- sort_by_sex(harmoNoOutlierSubset, sex)
  sexSubset <- sort_by_sex(harmoSubset, sex)
  print(paste("harmo_no: # of row:", nrow(sexSubset)))
  harmoNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harmoNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  print("Debin 4.6")
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


# option: 1->raw; 2->raw_wo_outlier; 3->harmo; 4->harmo_wo_outlier
# sex: 1->Male; 2->Female; 3->All gender
draw_plot <- function(feature, option=1, sex=3, is_boxplot=FALSE) {
  print(paste("ploting", feature))
  if (option==1) {
    suffix <- "raw"
    title <- paste("Raw", feature, gender_title(sex), sep=" ")
  } else if (option==2) {
    suffix <- "raw_no"
    title <- paste(feature, "with outlier removed", gender_title(sex), sep=" ")
  } else if (option==3) {
    suffix <- "harmo"
    title <- paste("Harmonized", feature, gender_title(sex), sep=" ")
  } else if (option==4) {
    suffix <- "harmo_no"
    title <- paste("Harmonized", feature, "with outlier removed", gender_title(sex), sep=" ")
  } else {
    print("invalid option")
    return()
  }
  file_path <- paste(wd, '/', feature, '_', suffix, gender_suffix(sex), '.csv', sep = "")
  print(file_path)
  gg_data0 <- read.csv(file_path, stringsAsFactors = FALSE)
  gg_data <- gg_data0 %>% filter(Age !="") %>% filter(get(feature)!=0 & get(feature)>0)
  
  if (is_boxplot) {
    v <- ggplot(data=gg_data, aes(x=Dataset, y=zscore, color=Dataset)) +
      geom_boxplot() + ggtitle(title) + ylab(feature) + xlab('Dataset') +
      theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5))
    return(v)
  }
  
  dmelt <- NULL
  fitted.values <- NULL
  fit4 <- NULL
  draw_vgam <- tryCatch( 
    {
      #fit4 <- vgam(get(feature) ~ s(Age, df = c(4, 2)), lms.bcn(zero = 1), data = gg_data, trace = TRUE)
      fit4 <- vgam(get(feature) ~ s(Age, df = 2), lms.bcn(zero = 1), data = gg_data, trace = TRUE, 
                   na.action=na.omit,  eps = 1e-12)
      if (is.null(fit4)) {
        print("fit4 is null")
      }
      fitted.values <- data.frame(qtplot.lmscreg(fit4, percentiles = c(2,25,50,75,98))$fitted.values)
      fitted.values[, 'Dataset'] <- gg_data[, 'Dataset']
      fitted.values[, 'Age'] <- gg_data[, 'Age']
      fitted.values[, 'Sex'] <- gg_data[, 'Sex']
      print(head(fitted.values))
      print("Debin 1.4")
      fitted.values[, feature] <- gg_data[, feature]
      print(head(fitted.values))
      print("Debin 2")
      
      dmelt <- melt(fitted.values, id.vars=c('Dataset', 'Age', 'Sex', feature))
      print(head(dmelt))
      print("....")
      print(tail(dmelt))
      TRUE
    }, 
    # warning  = function(warn) {
    #   print("dd warning....")
    #   fit4 <- vgam(get(feature) ~ s(Age, df = 2), lms.bcn(zero = 1), data = gg_data, trace = TRUE, 
    #                na.action=na.omit,  eps = 1e-12)
    #   if (!is.null(fit4)) {
    #     print("set fit in warning")
    #   }
    #   print(paste("MY_WARNING: ", warn))
    # }, 
    
    error = function(err) {
      print("dd err....")
      print(paste("Skipping vgam: ", file_path))
      print(paste("MY_ERROR: ", err))
      FALSE
    }
  )
  
  print("Debin 2.0.1")
  if (draw_vgam) {
    print("Debin 2.1")
    u <- ggplot(data=dmelt, aes(x=as.numeric(Age), y=get(feature), color=Dataset))
  } else {
    print("Debin 2.2")
    u <- ggplot(data=gg_data, aes(x=as.numeric(Age), y=get(feature), color=Dataset))
  }
  u <- u + geom_line(aes(Age, value, group=variable), color='black')
  u <- u + geom_point(size=0.5) + 
    #geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=5)) + 
    ggtitle(title) + ylab(feature)+ xlab('Age')
  print("Debin 3")
  
  u<- u + theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=5))
  print("Debin 4")
  u <- u + scale_x_continuous(breaks = seq(0,100, by =2))
  if (draw_vgam) {
    print("Debin 5")
    u <- u + annotate(geom='text',
                      x = 96,
                      y = unlist(fitted.values[which.max(fitted.values[, 'Age']), c(1:5)]),
                      label=c('2%', '25%', '50%', '75%', '98%'))
  }
  #u <- u + scale_x_discrete(breaks = seq(0,100, by =2))
  if (draw_vgam) {
    print("Debin 6")
    u
  }
  print("Debin 7")
  return(draw_vgam)
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

gen_4_sheet_all_regions <- function(data_raw=NULL, data_harm=NULL, sex=3, byMeanThick=TRUE,
                                    region_list=lh_thickness_region_list,
                                    outfile="lh_thickness_anova.xlsx",
                                    IsLH=TRUE) {
  # generate skeleton anova output file
  wb <- loadWorkbook(outfile, create = TRUE)
  createSheet(wb, name="workSheet")
  set_anova_skeleton(wb, region_list)
  
  index <- 0
  for (region_name in region_list) {
    data_analyse(data_raw, data_harmo, region_name, TRUE, FALSE, -3.5, 3.5,
                 sex, byMeanThick, wb, index, IsLH)
    index <- index + 1
  }
  
  saveWorkbook(wb)
  
  print( paste("All Done!!!  Finished total", index, " regions."))
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

gen_file_path <- function(feature, option, sex) {
  if (option==1) {
    suffix <- "raw"
  } else if (option==2) {
    suffix <- "raw_no_outlier"
  } else if (option==3) {
    suffix <- "harmo"
  } else if (option==4) {
    suffix <- "harmo_no_outlier"
  } else {
    print("invalid option")
    return()
  }
  file_path <- paste(wd, '/', feature, '_', suffix, gender_suffix(sex), '.csv', sep = "")
  return (file_path)
}

gen_plot_all_region <- function(region_list, sex) {
  skip_count <- 0
  created_file_count <- 0
  skip_file_list <- list()
  option_list <- list("_raw", "_raw_no", "_harmo", "_harmo_no")
  sex_list <- list("_male", "_female", "_all")
  for (region_name in region_list) {
    #    for (option in 1:4) {
    for (option in 2:4) {
      draw_vgam <- draw_plot(region_name, option, sex)
      print("Debin 10")
      if (draw_vgam) {
        created_file_count <- created_file_count + 1
        print("Debin 11")
        file_name <-paste(region_name, option_list[option], sex_list[sex], '.png', sep='')
        print(paste("generating:", file_name))
        ggsave(file_name)
      } else {
        skip_file <- gen_file_path(region_name, option, sex)
        skip_file_list <- append(skip_file_list, skip_file)
        skip_count <- skip_count + 1
      }
    }
  }
  
  if (created_file_count > 0 ) {
    print(paste("There are ", skip_count, " plot files generated", sep = ''))
  }
  
  if (skip_count > 0) {
    print(paste("Failed to generate images for the following ", skip_count, " file due to vgram issue:", sep = ''))
    for (i in 1:skip_count) {
      print(paste("  ", skip_file_list[i]))
    }
  }
}

gen_boxplot_all_region <- function(region_list, sex) {
  option_list <- list("_raw", "_raw_no_outlier", "_harmo", "_harmo_no_outlier")
  sex_list <- list("_male", "_female", "_all")
  for (region_name in region_list) {
    for (option in 1:4) {
      draw_plot(region_name, option, sex, is_boxplot=TRUE)
      file_name = paste(region_name, option_list[option], sex_list[sex], '_boxplot', '.png', sep='')
      print(paste("generating:", file_name))
      ggsave(file_name)
    }
  }
}

lh_thickness_region_list <- list("lh_bankssts_thickness",
                                 "lh_caudalanteriorcingulate_thickness",
                                 "lh_caudalmiddlefrontal_thickness",
                                 "lh_cuneus_thickness",
                                 "lh_entorhinal_thickness",
                                 "lh_fusiform_thickness",
                                 "lh_inferiorparietal_thickness",
                                 "lh_inferiortemporal_thickness",
                                 "lh_isthmuscingulate_thickness",
                                 "lh_lateraloccipital_thickness",
                                 "lh_lateralorbitofrontal_thickness",
                                 "lh_lingual_thickness",
                                 "lh_medialorbitofrontal_thickness",
                                 "lh_middletemporal_thickness",
                                 "lh_parahippocampal_thickness",
                                 "lh_paracentral_thickness",
                                 "lh_parsopercularis_thickness",
                                 "lh_parsorbitalis_thickness",
                                 "lh_parstriangularis_thickness",
                                 "lh_pericalcarine_thickness",
                                 "lh_postcentral_thickness",
                                 "lh_posteriorcingulate_thickness",
                                 "lh_precentral_thickness",
                                 "lh_precuneus_thickness",
                                 "lh_rostralanteriorcingulate_thickness",
                                 "lh_rostralmiddlefrontal_thickness",
                                 "lh_superiorfrontal_thickness",
                                 "lh_superiorparietal_thickness",
                                 "lh_superiortemporal_thickness",
                                 "lh_supramarginal_thickness",
                                 "lh_frontalpole_thickness",
                                 "lh_temporalpole_thickness",
                                 "lh_transversetemporal_thickness",
                                 "lh_insula_thickness")

gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byMeanThick=TRUE, 
                        region_list=lh_thickness_region_list, outfile="lh_thickness_anova_male.xlsx", IsLH=TRUE)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byMeanThick=TRUE, 
                        region_list=lh_thickness_region_list, outfile="lh_thickness_anova_female.xlsx", IsLH=TRUE)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byMeanThick=TRUE, 
                        region_list=lh_thickness_region_list, outfile="lh_thickness_anova.xlsx", IsLH=TRUE)


rh_thickness_region_list <- list("rh_bankssts_thickness",
                                 "rh_caudalanteriorcingulate_thickness",
                                 "rh_caudalmiddlefrontal_thickness",
                                 "rh_cuneus_thickness",
                                 "rh_entorhinal_thickness",
                                 "rh_fusiform_thickness",
                                 "rh_inferiorparietal_thickness",
                                 "rh_inferiortemporal_thickness",
                                 "rh_isthmuscingulate_thickness",
                                 "rh_lateraloccipital_thickness",
                                 "rh_lateralorbitofrontal_thickness",
                                 "rh_lingual_thickness",
                                 "rh_medialorbitofrontal_thickness",
                                 "rh_middletemporal_thickness",
                                 "rh_parahippocampal_thickness",
                                 "rh_paracentral_thickness",
                                 "rh_parsopercularis_thickness",
                                 "rh_parsorbitalis_thickness",
                                 "rh_parstriangularis_thickness",
                                 "rh_pericalcarine_thickness",
                                 "rh_postcentral_thickness",
                                 "rh_posteriorcingulate_thickness",
                                 "rh_precentral_thickness",
                                 "rh_precuneus_thickness",
                                 "rh_rostralanteriorcingulate_thickness",
                                 "rh_rostralmiddlefrontal_thickness",
                                 "rh_superiorfrontal_thickness",
                                 "rh_superiorparietal_thickness",
                                 "rh_superiortemporal_thickness",
                                 "rh_supramarginal_thickness",
                                 "rh_frontalpole_thickness",
                                 "rh_temporalpole_thickness",
                                 "rh_transversetemporal_thickness",
                                 "rh_insula_thickness")

gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byMeanThick=TRUE, 
                        region_list=rh_thickness_region_list, outfile="rh_thickness_anova_male.xlsx", IsLH=FALSE)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byMeanThick=TRUE, 
                        region_list=rh_thickness_region_list, outfile="rh_thickness_anova_female.xlsx", IsLH=FALSE)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byMeanThick=TRUE, 
                        region_list=rh_thickness_region_list, outfile="rh_thickness_anova.xlsx", IsLH=FALSE)


draw_plot("lh_bankssts_thickness", option=1, sex=3, is_boxplot=FALSE)


lh_thickness_region_list_1 <- list("lh_bankssts_thickness")
gen_plot_all_region(lh_thickness_region_list_1, 1)
