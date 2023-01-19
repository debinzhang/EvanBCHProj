# install.packages("XLConnect")
#install.packages("reticulate")
library(ggplot2)
library(dplyr)
library(XLConnect)

library(VGAM)
library(reshape2)
library(reticulate)  # this package enable python in R

use_python("/usr/local/bin/python3")

wd <- getwd()
setwd(wd)
source_python("draw_shaded_plot.py")
source_python("harmonDataBuilder_9_R_age_key.py")
source_python("categorize_data.py")

gen_raw_harmo_data <- function() {
  file_list <- list("raw_raw_new.csv")
  
  for (file_path in file_list) {
    print(paste("working on :", file_path, sep = ''))
    file_name <- strsplit(file_path, "\\.")[[1]][1]
    file_extension <- strsplit(file_path, "\\.")[[1]][2]
    data_raw <- read.csv(file_path, stringsAsFactors = FALSE, check.names=FALSE)
    row_num_before <- nrow(data_raw)
    
    data_raw_1 <- data_raw[ which(data_raw$Age!="" & data_raw$Age!="Siemens"  &
                                    (data_raw$Sex==1 | data_raw$Sex==2 | data_raw$Sex=='1' | data_raw$Sex=='2' | 
                                       data_raw$Sex=="M" | data_raw$Sex=="m" | data_raw$Sex=="F" |
                                       data_raw$Sex=="f")
    ),  ]
    row_num_after_sex <- nrow(data_raw_1)
    
    outfile <- "raw_clean.csv"
    print(paste("creating cleanfile: ", outfile, sep = ''))
    write.csv(data_raw_1, outfile, row.names = FALSE, quote = FALSE)
    
    # harmonize the outfile
    harmo_file <- "raw_clean_harmo.csv"
    print(paste("creating harmo file: ", harmo_file, sep = ''))
    print("------------")
    harmonize(outfile, harmo_file, scanertype=FALSE, Eb=FALSE, Mean_only=TRUE, dataset=TRUE, Age=TRUE)
  }
}


adjustByMeanData <- function(data, feature, byETIV=TRUE, IsLH=TRUE) {
  if (byETIV) {
    data1 <-data[data$eTIV != 0, ]
    adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$eTIV)))
    return(adjByMeanData)
  } else {
    # this is by chickness case
    if (IsLH) {
      data1 <-data[data$lh_MeanThickness_thickness != 0, ]
      adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$lh_MeanThickness_thickness)))
    } else {
      data1 <-data[data$rh_MeanThickness_thickness != 0, ]
      adjByMeanData<-data1 %>% mutate(adjByMean=({{feature}}/(data1$rh_MeanThickness_thickness)))
    }
    return(adjByMeanData)
  }
}

getRobustZScoreByDataset <- function(data, feature, byETIV=TRUE, IsLH=TRUE) {
  if (byETIV) {
    eTIV_data<-adjustByMeanData(data, get(feature), byETIV)
    grouped_data <- eTIV_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
    merged_data <- merge(eTIV_data, grouped_data, by="Dataset")
  } else {
    if (IsLH) {
      lh_MeanThickness_thickness_data<-adjustByMeanData(data, get(feature), byETIV, IsLH)
      grouped_data <- lh_MeanThickness_thickness_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
      merged_data <- merge(lh_MeanThickness_thickness_data, grouped_data, by="Dataset")
    } else {
      rh_MeanThickness_thickness_data<-adjustByMeanData(data, get(feature), byETIV, IsLH)
      grouped_data <- rh_MeanThickness_thickness_data %>% group_by(Dataset) %>% summarise_at(vars(adjByMean), list(group_median = median, group_mad = mad))
      merged_data <- merge(rh_MeanThickness_thickness_data, grouped_data, by="Dataset")
    }
  }

  zscore_data<-merged_data %>% mutate(zscore=(adjByMean - group_median)/group_mad)
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
                low=-3.5, high=3.5, sex=3, byETIV=TRUE, wb=NULL, index=0, IsLH=TRUE, gen_shaded_plot=FALSE) {
  print(paste("working on ", feature, sep = ''))
  # step1: generation raw file
  print("evan 1")
  data_raw_z <- getRobustZScoreByDataset(data_raw, feature, byETIV, IsLH)
  rawSubset <- data_raw_z %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, zscore, {{feature}})
  sexSubset <- sort_by_sex(rawSubset, sex)
  raw_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  raw_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }

  print("evan 2")
  # step2: generation raw_no_outlier file
  data_raw_z1 <- getRobustZScoreByDataset(data_raw, feature, byETIV, IsLH)
  rawNoOutlier <- rmOutlier(data_raw_z1, low, high)
  if (byETIV) {
    rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, eTIV, zscore, {{feature}})
  } else {  # by chickness cases
    if (IsLH) {
      rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, lh_MeanThickness_thickness, zscore, {{feature}})
    } else {
      rawNoOutlierSubset <- rawNoOutlier %>% select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, rh_MeanThickness_thickness, zscore, {{feature}})
    }
  }
  sexSubset <- sort_by_sex(rawNoOutlierSubset, sex)
  rawNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  rawNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
    # Note: the output raw_no_outlier file contains zscore column. But that zscore coloumn will be ignored during later on harmonization step
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }

    print("evan 3")
  # # step3: generating harmon file. Note: this harmon file is based on raw (before outlier removal) data 
  data_harmo_z <- getRobustZScoreByDataset(data_harmo, feature, byETIV, IsLH)
  harmoSubset <- data_harmo_z %>% select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harm_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harm_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }
  
    print("evan 4")
  # step4: generation harmo_no_outlier file. This is based on the step 2 output.
  src_no_file <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
  outfile <- paste(wd, '/', feature, '_harmo_no', gender_suffix(sex), '.csv', sep="")
  rslt <- harmonize(src_no_file, outfile, scanertype=FALSE, Eb=FALSE, Mean_only=TRUE, dataset=TRUE, Age=TRUE, gen_plot=TRUE)
   if (!rslt) {
    print(paste("Harmonization failed for", feature))
    return(FALSE)
  }

  # draw shaded plot
  if (gen_shaded_plot) {
    rslt <- draw_shaded_plot(feature)
    if (!rslt) {
      print(paste("failed to draw shaded plot for", feature, "!!!"))
    }
  }

  data_harmo_no <- read.csv(outfile, stringsAsFactors = TRUE, check.names=FALSE)
  data_harmo_z1 <- getRobustZScoreByDataset(data_harmo_no, feature, byETIV, IsLH)
  # harmoNoOutlier <- rmOutlier(data_harmo_z1, low, high)
  harmoSubset <-  data_harmo_z1 %>% select(Dataset, Age, Sex, zscore, {{feature}})
  #sexSubset <- sort_by_sex(harmoNoOutlierSubset, sex)
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harmoNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harmoNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  if (!is.null(wb)) {
    writeWorksheet(wb, raw_Anova, sheet="workSheet", startRow=index*3+2, startCol = 3)
    writeWorksheet(wb, rawNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 9)
    writeWorksheet(wb, harm_Anova, sheet="workSheet", startRow=index*3+2, startCol = 15)
    writeWorksheet(wb, harmoNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 21)
  }
  
  sink("anova_analysis.txt", append=TRUE)
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
  sink(NULL)
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
  gg_data0 <- read.csv(file_path, stringsAsFactors = FALSE, check.names=FALSE)
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
                    na.action=na.omit,  eps = 1e-16)
      if (is.null(fit4)) {
        print("fit4 is null")
      }
      fitted.values <- data.frame(qtplot.lmscreg(fit4, percentiles = c(1,25,50,75,99))$fitted.values)
      fitted.values[, 'Dataset'] <- gg_data[, 'Dataset']
      fitted.values[, 'Age'] <- gg_data[, 'Age']
      fitted.values[, 'Sex'] <- gg_data[, 'Sex']
      fitted.values[, feature] <- gg_data[, feature]
      
      dmelt <- melt(fitted.values, id.vars=c('Dataset', 'Age', 'Sex', feature))
      TRUE
    },
    
    error = function(err) {
      print(paste("Skipping vgam: ", file_path))
      print(paste("MY_ERROR: ", err))
      FALSE
    }
  )
  
  if (draw_vgam) {
    u <- ggplot(data=dmelt, aes(x=as.numeric(Age), y=get(feature), color=Dataset))
    u <- u + geom_point(size=0.5) + ggtitle(title) + ylab(feature)+ xlab('Age')
    u <- u + geom_line(aes(Age, value, group=variable), color='black')
  } else {
    u <- ggplot(data=gg_data, aes(x=Age, y=get(feature), color=Dataset))
    u <- u + geom_point(size=0.5) + geom_point(size=0.5) +
          ggtitle(title) + ylab(feature)+ xlab('Age')
    u <- u +  geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=5))
  }
  
  u<- u + theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=5))
  u <- u + scale_x_continuous(breaks = seq(0,100, by =2))
  if (draw_vgam) {
    u <- u + annotate(geom='text',
                      x = 98,
                      y = unlist(fitted.values[which.max(fitted.values[, 'Age']), c(1:5)]),
                      label=c('1%', '25%', '50%', '75%', '99%'), size=3)
  }
  u
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

gen_4_sheet_all_regions <- function(data_raw=NULL, data_harmo=NULL, sex=3, byETIV=TRUE,
                                    region_list=lh_thickness_region_list,
                                    outfile="unnamed_anova.xlsx", genTukeyHSD=FALSE,
                                    genShadedPlot=FALSE, isLH=FALSE) {
  # generate skeleton anova output file
  wb <- loadWorkbook(outfile, create = TRUE)
  createSheet(wb, name="workSheet")
  set_anova_skeleton(wb, region_list)
  
  index <- 0
  for (region_name in region_list) {
    data_analyse(data_raw, data_harmo, region_name, TRUE, genTukeyHSD, -3.5, 3.5,
                 sex, byETIV, wb, index, IsLH=isLH, gen_shaded_plot=genShadedPlot)
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
    # for (option in 1:4) {
    # for now we only care about after harmonization and after outliers removed data 
    for (option in 4:4) {
      draw_vgam <- draw_plot(region_name, option, sex)
      if (draw_vgam) {
        created_file_count <- created_file_count + 1
        file_name <-paste(region_name, option_list[option], sex_list[sex], '.png', sep='')
      } else {
        skip_file <- gen_file_path(region_name, option, sex)
        skip_file_list <- append(skip_file_list, skip_file)
        skip_count <- skip_count + 1
        file_name <-paste(region_name, option_list[option], sex_list[sex], '_s', '.png', sep='')
      }
      ggsave(file_name)
    }
  }
  
  if (created_file_count > 0 ) {
    print(paste("There are ", created_file_count, " plot files generated", sep = ''))
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

# this function draw plots that shows shaded left and right hemisphere comparision
gen_hemisphere_plots<- function(feature_list) {
  for (rh_feature in feature_list) {
    # get feature name without hemsphere prefix. like 
    # rh_entorhinal_thickness -> entorhinal_thickness
    feature = substr(rh_feature, 4, nchar(rh_feature))
    lh_feature = paste("lh_", feature, sep='')
    data_lh <- read.csv(paste(lh_feature, "_harmo_no_all_plot.csv", sep=''), stringsAsFactors=TRUE, check.names=FALSE)
    data_rh <- read.csv(paste(rh_feature, "_harmo_no_all_plot.csv", sep=''), stringsAsFactors=TRUE, check.names=FALSE)
    
    # remove "lh_MeanThickness_thickness" or "eTIV" from data_lh
    # we need this step; otherwise, the left and right data table would have different column name
    # then they cannot be combined
    if ("lh_MeanThickness_thickness" %in% colnames(data_lh)) {
      # drop lh_MeanThickness_thickness column
      data_lh = subset(data_lh, select = -c(lh_MeanThickness_thickness))
    }
    
    # rename lh_entorhinal_thickness to entorhinal_thickness
    colnames(data_lh)[colnames(data_lh) == lh_feature] = feature
    # add column "hemsphere" and set value "left"
    data_lh["hemisphere"] = 'left'
    
    # remove "rh_MeanThickness_thickness" or "eTIV" from data_rh
    if ("rh_MeanThickness_thickness" %in% colnames(data_rh)) {
      # drop rh_MeanThickness_thickness column
      data_rh = subset(data_rh, select = -c(rh_MeanThickness_thickness))
    }
    
    # rename rh_entorhinal_thickness to entorhinal_thickness
    colnames(data_rh)[colnames(data_rh) == rh_feature] = feature
    # add column "hemsphere" and set value "right"
    data_rh["hemisphere"] = 'right'
    
    # combined the left and right table
    combined_data = rbind(data_lh, data_rh)
    
    outfile = paste(feature, "_hemisphere.csv", sep='')
    write.csv(combined_data, outfile, row.names=FALSE, quote=FALSE)
    
    draw_shade_hemisphere_plot(feature)
  }
}


process_all_melissa <- function(clean_leftover=TRUE) {
  region_list1 <- list("Left-Lateral-Ventricle",
                      "Left-Inf-Lat-Vent",
                      "Left-Cerebellum-White-Matter",
                      "Left-Cerebellum-Cortex",
                      "Left-Thalamus",
                      "Left-Caudate",
                      "Left-Putamen",
                      "Left-Pallidum",
                      "3rd-Ventricle",
                      "4th-Ventricle",
                      "Brain-Stem",
                      "Left-Hippocampus",
                      "Left-Amygdala",
                      "CSF",
                      "Left-Accumbens-area",
                      "Left-VentralDC",
                      "Left-vessel",
                      "Left-choroid-plexus",
                      "Right-Lateral-Ventricle",
                      "Right-Inf-Lat-Vent",
                      "Right-Cerebellum-White-Matter",
                      "Right-Cerebellum-Cortex",
                      "Right-Thalamus",
                      "Right-Caudate",
                      "Right-Putamen",
                      "Right-Pallidum",
                      "Right-Hippocampus",
                      "Right-Amygdala",
                      "Right-Accumbens-area",
                      "Right-VentralDC",
                      "Right-vessel",
                      "Right-choroid-plexus",
                      "WM-hypointensities",
                      "Optic-Chiasm",
                      "CC_Posterior",
                      "CC_Mid_Posterior",
                      "CC_Central",
                      "CC_Mid_Anterior",
                      "CC_Anterior",
                      "BrainSegVol",
                      "BrainSegVolNotVent",
                      #"lhCortexVol",
                      "rhCortexVol",
                      "CortexVol",
                      "lhCerebralWhiteMatterVol",
                      "rhCerebralWhiteMatterVol",
                      "CerebralWhiteMatterVol",
                      "SubCortGrayVol",
                      "TotalGrayVol",
                      "SupraTentorialVol",
                      "SupraTentorialVolNotVent",
                      "MaskVol",
                      "BrainSegVol-to-eTIV",
                      "MaskVol-to-eTIV",
                      "lhSurfaceHoles",
                      "rhSurfaceHoles",
                      "SurfaceHoles")

   region_list_x <- list("lhCortexVol", "rhCortexVol")

  gen_raw_harmo_data()

  data_raw <- read.csv("raw_clean.csv", stringsAsFactors = TRUE, check.names=FALSE)
  data_harmo <- read.csv("raw_clean_harmo.csv", stringsAsFactors = TRUE, check.names=FALSE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                          region_list=region_list1, outfile="anova_age_as_key.xlsx", 
                          genTukeyHSD=TRUE, genShadedPlot=TRUE)
  gen_plot_all_region(region_list1, 3)

  categorize_data(clean_leftover)
}



















process_all <- function(draw_hemi_plot=TRUE, clean_leftover=TRUE) {
  gen_raw_harmo_data()
  process_lh_curvind()
  process_rh_curvind(draw_hemi_plot)

  # process_lh_foldind()
  # process_rh_foldind(draw_hemi_plot)
  # process_lh_gauscurv()
  # process_rh_gauscurv(draw_hemi_plot)
  # process_lh_meancurv()
  # process_rh_meancurv(draw_hemi_plot)
  # process_lh_thicknessstd()
  # process_rh_thicknessstd(draw_hemi_plot)
  # process_lh_thickness()
  # process_rh_thickness(draw_hemi_plot)
  categorize_data(clean_leftover)
}


#  The followings are test code 
### 1. lh_curvind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_curvind_region_list, outfile="lh_curvind_anova_age_as_key.xlsx", 
                        genTukeyHSD=TRUE, genShadedPlot=TRUE)
lh_curvind_region_list1 <- list("lh_bankssts_curvind")
gen_plot_all_region(lh_curvind_region_list1, 3)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_curvind_region_list, outfile="lh_curvind_anova_male.xlsx")
gen_plot_all_region(lh_curvind_region_list, 1)

  
gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE,
                        region_list=lh_curvind_region_list, outfile="lh_curvind_anova_female.xlsx")
gen_plot_all_region(lh_curvind_region_list, 2)


### 2. rh_curvind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_curvind_region_list, outfile="rh_curvind_anova_male.xlsx")
gen_plot_all_region(rh_curvind_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_curvind_region_list, outfile="rh_curvind_anova_female.xlsx")
gen_plot_all_region(rh_curvind_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_curvind_region_list, outfile="rh_curvind_anova.xlsx")
gen_plot_all_region(rh_curvind_region_list, 3)

### 3. lh_foldind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_foldind_region_list, outfile="lh_foldind_anova_male.xlsx")
gen_plot_all_region(lh_foldind_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_foldind_region_list, outfile="lh_foldind_anova_female.xlsx")
gen_plot_all_region(lh_foldind_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_foldind_region_list, outfile="lh_foldind_anova.xlsx")
gen_plot_all_region(lh_foldind_region_list, 3)

### 4. rh_foldind
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_foldind_region_list, outfile="rh_foldind_anova_male.xlsx")
gen_plot_all_region(rh_foldind_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_foldind_region_list, outfile="rh_foldind_anova_female.xlsx")
gen_plot_all_region(rh_foldind_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_foldind_region_list, outfile="rh_foldind_anova.xlsx")
gen_plot_all_region(rh_foldind_region_list, 3)


### 5. lh_gauscurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_gauscurv_region_list, outfile="lh_gauscurv_anova_male.xlsx")
gen_plot_all_region(lh_gauscurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_gauscurv_region_list, outfile="lh_gauscurv_anova_female.xlsx")
gen_plot_all_region(lh_gauscurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_gauscurv_region_list, outfile="lh_gauscurv_anova.xlsx")
gen_plot_all_region(lh_gauscurv_region_list, 3)

### 6. rh_gauscurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_gauscurv_region_list, outfile="rh_gauscurv_anova_male.xlsx")
gen_plot_all_region(rh_gauscurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_gauscurv_region_list, outfile="rh_gauscurv_anova_female.xlsx")
gen_plot_all_region(rh_gauscurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_gauscurv_region_list, outfile="rh_gauscurv_anova.xlsx")
gen_plot_all_region(rh_gauscurv_region_list, 3)


### 7. lh_meancurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_meancurv_region_list, outfile="lh_meancurv_anova_male.xlsx")
gen_plot_all_region(lh_meancurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_meancurv_region_list, outfile="lh_meancurv_anova_female.xlsx")
gen_plot_all_region(lh_meancurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_meancurv_region_list, outfile="lh_meancurv_anova.xlsx")
gen_plot_all_region(lh_meancurv_region_list, 3)

### 8. rh_meancurv
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_meancurv_region_list, outfile="rh_meancurv_anova_male.xlsx")
gen_plot_all_region(rh_meancurv_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_meancurv_region_list, outfile="rh_meancurv_anova_female.xlsx")
gen_plot_all_region(rh_meancurv_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_meancurv_region_list, outfile="rh_meancurv_anova.xlsx")
gen_plot_all_region(rh_meancurv_region_list, 3)

### 9. lh_thicknessstd
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=lh_thicknessstd_region_list, outfile="lh_thicknessstd_anova_male.xlsx")
gen_plot_all_region(lh_thicknessstd_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=lh_thicknessstd_region_list, outfile="lh_thicknessstd_anova_female.xlsx")
gen_plot_all_region(lh_thicknessstd_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=lh_thicknessstd_region_list, outfile="lh_thicknessstd_anova.xlsx")
gen_plot_all_region(lh_thicknessstd_region_list, 3)

### 10. rh_thicknessstd
gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV=TRUE, 
                        region_list=rh_thicknessstd_region_list, outfile="rh_thicknessstd_anova_male.xlsx")
gen_plot_all_region(rh_thicknessstd_region_list, 1)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV=TRUE, 
                        region_list=rh_thicknessstd_region_list, outfile="rh_thicknessstd_anova_female.xlsx")
gen_plot_all_region(rh_thicknessstd_region_list, 2)

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV=TRUE, 
                        region_list=rh_thicknessstd_region_list, outfile="rh_thicknessstd_anova.xlsx")
gen_plot_all_region(rh_thicknessstd_region_list, 3)



rh_thicknessstd_region_list_1 <- list("rh_bankssts_thicknessstd")
rh_thicknessstd_region_list_2 <- list("rh_caudalanteriorcingulate_thicknessstd")

gen_plot_all_region(rh_thicknessstd_region_list, 1)

###################### test cmds begin ######################

rh_curvind_region_list_1 <- list("rh_bankssts_curvind")


gen_4_sheet_all_regions(data_raw, data_harmo, sex=1, byETIV =TRUE, 
                        region_list=rh_curvind_region_list_1, outfile="rh_curvind_anova_male.xlsx")

gen_4_sheet_all_regions(data_raw, data_harmo, sex=2, byETIV =  TRUE, 
                        region_list=rh_curvind_region_list_1, outfile="rh_curvind_anova_female.xlsx")

gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, byETIV =TRUE, 
                        region_list=rh_curvind_region_list_1, outfile="rh_curvind_anova.xlsx")


draw_plot("rh_bankssts_curvind", option=2, sex=1, is_boxplot=FALSE)

gen_plot_all_region(rh_curvind_region_list_1, 1)

###################### test cmds end ######################



