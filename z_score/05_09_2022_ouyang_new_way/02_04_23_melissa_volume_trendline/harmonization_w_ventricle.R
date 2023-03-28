# install.packages("XLConnect")
#install.packages("reticulate")
library(ggplot2)
library(dplyr)
library(XLConnect)

library(VGAM)
library(reshape2)
library(reticulate)  # this package enable python in R

library(gamlss)

use_python("/usr/local/bin/python3")

wd <- getwd()
setwd(wd)
source_python("draw_shaded_plot.py")
source_python("harmonDataBuilder_9_R_scannertype_key_melissa.py")
source_python("categorize_data.py")

getRobustZScoreByDataset <- function(data, feature) {
  grouped_data <- data %>% group_by(Dataset) %>% summarise_at({{feature}}, list(group_median = median, group_mad = mad))
  merged_data <- merge(data, grouped_data, by="Dataset")
  zscore_data<-merged_data %>% mutate(zscore=(get(feature) - group_median)/group_mad)
  
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
                low=-3.0, high=3.0, sex=3, wb=NULL, index=0, gen_shaded_plot=FALSE) {
  print(paste("working on ", feature, sep = ''))
  # step1: generation raw file
  data_raw_z <- getRobustZScoreByDataset(data_raw, feature)
  print("Evan 1233")
  rawSubset <- data_raw_z %>% dplyr::select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, zscore, {{feature}})
  print("Evan 1234")
  sexSubset <- sort_by_sex(rawSubset, sex)
  raw_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  raw_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  print("Evan 1.0")
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }

  # step2: generation raw_no_outlier file
  data_raw_z1 <- getRobustZScoreByDataset(data_raw, feature)
  rawNoOutlier <- rmOutlier(data_raw_z1, low, high)
  
  rawNoOutlierSubset <- rawNoOutlier %>% dplyr::select(Dataset, Age, Sex, Scanner_type, Magnetic_field_of_strength, zscore, {{feature}})
  
  sexSubset <- sort_by_sex(rawNoOutlierSubset, sex)
  rawNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  rawNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  print("Evan 1.01")
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
    # Note: the output raw_no_outlier file contains zscore column. But that zscore coloumn will be ignored during later on harmonization step
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)

    #generate plot file that has age round down to floor
    sexSubset_plot <- sexSubset
    #sexSubset_plot$Age <- floor(sexSubset_plot$Age)
    sexSubset_plot$Age <- round(sexSubset_plot$Age, 0)
    outfile_plot <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '_plot.csv', sep="")
    write.csv(sexSubset_plot, outfile_plot, row.names = FALSE, quote = FALSE)
  }

  # # step3: generating harmon file. Note: this harmon file is based on raw (before outlier removal) data 
  data_harmo_z <- getRobustZScoreByDataset(data_harmo, feature)
  harmoSubset <- data_harmo_z %>% dplyr::select(Dataset, Age, Sex, zscore, {{feature}})
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harm_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harm_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  if (gen_csv) {
    outfile <- paste(wd, '/', feature, '_harmo', gender_suffix(sex), '.csv', sep="")
    write.csv(sexSubset, outfile, row.names = FALSE, quote = FALSE)
  }

  # step4: generation harmo_no_outlier file. This is based on the step 2 output.
  src_no_file <- paste(wd, '/', feature, '_raw_no', gender_suffix(sex), '.csv', sep="")
  outfile <- paste(wd, '/', feature, '_harmo_no', gender_suffix(sex), '.csv', sep="")
  print("Evan 1.0212")
  print(paste("src_outfile:", src_no_file))
  print(paste("outfile:", outfile))
  # rslt <- harmonize(src_no_file, outfile, scanertype=FALSE, Eb=TRUE, Mean_only=FALSE, dataset=FALSE, Age=FALSE, gen_plot=FALSE)
  rslt <- harmonize(src_no_file, outfile, scanertype=FALSE, Eb=TRUE, Mean_only=FALSE, dataset=FALSE, Age=FALSE, gen_plot=TRUE)
  
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

  data_harmo_no <- read.csv(outfile, stringsAsFactors = TRUE)
  data_harmo_z1 <- getRobustZScoreByDataset(data_harmo_no, feature)
  # harmoNoOutlier <- rmOutlier(data_harmo_z1, low, high)
  harmoSubset <-  data_harmo_z1 %>% dplyr::select(Dataset, Age, Sex, zscore, {{feature}})
  #sexSubset <- sort_by_sex(harmoNoOutlierSubset, sex)
  sexSubset <- sort_by_sex(harmoSubset, sex)
  harmoNoOutlier_aov_rst <- aov(zscore ~ Dataset, data = sexSubset)
  harmoNoOutlier_Anova <- anova(lm(zscore ~ Dataset, data = sexSubset))
  print("Evan 1.05")
  if (!is.null(wb)) {
    writeWorksheet(wb, raw_Anova, sheet="workSheet", startRow=index*3+2, startCol = 3)
    writeWorksheet(wb, rawNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 9)
    writeWorksheet(wb, harm_Anova, sheet="workSheet", startRow=index*3+2, startCol = 15)
    writeWorksheet(wb, harmoNoOutlier_Anova, sheet="workSheet", startRow=index*3+2, startCol = 21)
  }
  print("Evan 1.06")
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
  gg_data0 <- read.csv(file_path, stringsAsFactors = FALSE)
  gg_data <- gg_data0 %>% filter(Age !="") %>% filter(get(feature)!=0 & get(feature)>0)
  
  if (is_boxplot) {
    v <- ggplot(data=gg_data, aes(x=Dataset, y=zscore, color=Dataset)) +
      geom_boxplot() + ggtitle(title) + ylab(feature) + xlab('Dataset') +
      theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5))
    return(v)
  }
  
  # dmelt <- NULL
  # fitted.values <- NULL
  # fit4 <- NULL
  # draw_vgam <- tryCatch( 
  #   {
  #     #fit4 <- vgam(get(feature) ~ s(Age, df = c(4, 2)), lms.bcn(zero = 1), data = gg_data, trace = TRUE)
  #     fit4 <- vgam(get(feature) ~ s(Age, df = 2), lms.bcn(zero = 1), data = gg_data, trace = TRUE, 
  #                   na.action=na.omit,  eps = 1e-16)
  #     if (is.null(fit4)) {
  #       print("fit4 is null")
  #     }
  #     fitted.values <- data.frame(qtplot.lmscreg(fit4, percentiles = c(1,25,50,75,99))$fitted.values)
  #     fitted.values[, 'Dataset'] <- gg_data[, 'Dataset']
  #     fitted.values[, 'Age'] <- gg_data[, 'Age']
  #     fitted.values[, 'Sex'] <- gg_data[, 'Sex']
  #     fitted.values[, feature] <- gg_data[, feature]
      
  #     dmelt <- melt(fitted.values, id.vars=c('Dataset', 'Age', 'Sex', feature))
  #     TRUE
  #   },
    
  #   error = function(err) {
  #     print(paste("Skipping vgam: ", file_path))
  #     print(paste("MY_ERROR: ", err))
  #     FALSE
  #   }
  # )
  
  draw_vgam <- FALSE
  
  if (draw_vgam) {
    u <- ggplot(data=dmelt, aes(x=as.numeric(Age), y=get(feature), color=Dataset))
    u <- u + geom_point(size=0.5) + ggtitle(title) + ylab(feature)+ xlab('Age')
    u <- u + geom_line(aes(Age, value, group=variable), color='black')
  } else {
    # scanner_type and Magnetic_field_of_strength maybe NA in gg_data that breaks gamlss. Remove these columns
    gamlssSubset <- gg_data %>% dplyr::select(Dataset, Age, Sex, {{feature}})
    # gg_data$predicted <- predict(gamlss(formula=get(feature)~Age,family=NO,data=gg_data))
    model_5 <- gamlss(get(feature) ~ poly((Age), 5), data=gamlssSubset, family=NO)
    #model_5 <- gamlss(get(feature) ~ pb(Age), data=gamlssSubset, family=NO)
    #model_5 <- gamlss(get(feature) ~ s(Age), data=gamlssSubset, family=GA(),method=CG())
    #model_18 <- gamlss(get(feature) ~ poly(scale(Age), 18), data=gamlssSubset, family=NO)
    model_18 <- gamlss(get(feature) ~ poly(Age, 18), data=gamlssSubset, family=NO)
    #model <- gamlss(BrainSegVolNotVent ~ poly(Age, order), data=gg_data, family=NO)
    gg_data$predicted_5 <- predict(model_5)
    gg_data$predicted_18 <- predict(model_18)
    #predicted <- predict(model, newdata = gg_data)    
    u <- ggplot(data=gg_data, aes(x=Age, y=get(feature), color=Dataset))
    u <- u + geom_point(size=0.2) +
          ggtitle(title) + ylab(feature)+ xlab('Age')
    # u <- u +  geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=5))
    #u <- u +  geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=8))

    u <- u + 
      #geom_smooth(data=gg_data, aes(x=Age, predicted_5), linewidth=0.5, color="blue")
      #geom_smooth(data=gg_data, aes(x=Age, predicted_18), linewidth=0.5, color="red") +
      #geom_smooth(data=gg_data, aes(x=Age, predicted_18), linewidth=0.5, color="red", size=0.5, method="gam", formula = y ~ s(x, bs = "cs", k=14), span=0.8) 
      geom_smooth(data=gg_data, aes(x=Age, predicted_5), linewidth=0.5, color="red", size=0.5, method="gam", formula = y ~ s(x, bs = "cs", k=5), span=0.8) 
      # geom_line(aes(x=Age, predicted_5), linewidth=0.5, color="green") +
      #geom_line(aes(x=Age, y=predicted_18), linewidth=0.5, color="purple")
  }
  
  u <- u + theme(plot.title = element_text(color="DarkBlue", size=11, family = "Courier", hjust=0.5)) +
    theme(axis.text.x=element_text(size=6), axis.text.y=element_text(size=6),
          axis.title=element_text(size=10))
  u <- u + theme(legend.position = c(0.90, 0.81), legend.background = element_rect(fill = "white", color = "black"), 
            legend.text=element_text(size=5), legend.title=element_text(size=7), legend.key.size = unit(0.4, 'cm'))
            # + guides(color = guide_legend(override.aes = list(size = 0.3)))
  u <- u + scale_x_continuous(breaks = seq(0,100, by =2))
  if (draw_vgam) {
    u <- u + annotate(geom='text',
                      x = 98,
                      y = unlist(fitted.values[which.max(fitted.values[, 'Age']), c(1:5)]),
                      label=c('1%', '25%', '50%', '75%', '99%'), size=3)
  }
  u
  # return(draw_vgam)
  return(TRUE)
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

gen_4_sheet_all_regions <- function(data_raw=NULL, data_harmo=NULL, sex=3,
                                    region_list=lh_thickness_region_list,
                                    outfile="unnamed_anova.xlsx", genTukeyHSD=FALSE,
                                    genShadedPlot=FALSE, zscore_threshold=3.0) {
  # generate skeleton anova output file
  wb <- loadWorkbook(outfile, create = TRUE)
  createSheet(wb, name="workSheet")
  set_anova_skeleton(wb, region_list)
  
  index <- 0
  for (region_name in region_list) {
    data_analyse(data_raw, data_harmo, region_name, TRUE, genTukeyHSD, -1.0*zscore_threshold, zscore_threshold,
               sex, wb, index, gen_shaded_plot=genShadedPlot)
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
    #for (option in 1:4) {
    # for now we only care about post outlier removal and post harmonization with after outliers removed data 
    for (option in c(1,2,4)) {
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
    data_lh <- read.csv(paste(lh_feature, "_harmo_no_all_plot.csv", sep=''), stringsAsFactors=TRUE)
    data_rh <- read.csv(paste(rh_feature, "_harmo_no_all_plot.csv", sep=''), stringsAsFactors=TRUE)
    
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


gen_raw_harmo_data <- function() {
  #file_list <- list("raw_raw_new_underscore.csv")
  file_list <- list("raw_raw_new_underscore_w_combined_ventricle.csv")
  
  for (file_path in file_list) {
    print(paste("working on :", file_path, sep = ''))
    file_name <- strsplit(file_path, "\\.")[[1]][1]
    file_extension <- strsplit(file_path, "\\.")[[1]][2]
    data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
    row_num_before <- nrow(data_raw)
    
    print("Evan 1")
    data_raw_1 <- data_raw[ which(data_raw$Age!="" & data_raw$Age!="Siemens"  &
                                    (data_raw$Sex==1 | data_raw$Sex==2 |
                                       data_raw$Sex=="M" | data_raw$Sex=="m" | data_raw$Sex=="F" |
                                       data_raw$Sex=="f")
                                       # & data_raw$Dataset!="BGSP"
    ),  ]
    print("Evan 1.1")
    data_raw_1$Sex[ data_raw_1$Sex=='1' | data_raw_1$Sex=='m' | data_raw_1$Sex=='M' | data_raw_1$Sex=="Male" ] <- "M"
    #data_raw_1 <- data_raw_1 %>% mutate(Sex = recode(Sex, '1'="M", 'm'="M", 'M'="M"))
    print("Evan 1.2")
    data_raw_1$Sex[ data_raw_1$Sex=='2' | data_raw_1$Sex=='f' | data_raw_1$Sex=='F' | data_raw_1$Sex=="Female"] <- "F"
    #data_raw_1 <- data_raw_1 %>% mutate(Sex = recode(Sex, '2'="F", 'f'="F", 'F'="F", "Female"="F"))
    print("Evan 1.3")
    row_num_after_sex <- nrow(data_raw_1)
    # drop the "Volume" column because Dataset "ABCD" does not provide the Volume date
    # alos drop "path" and "subjectId" columns because we do not need them for harmonization
    data_raw_1 <- subset(data_raw_1, select=-c(Pathway))
    print("Evan 2")
    outfile <- paste(file_name, '_clean', '.csv', sep="")
    print(paste("creating cleanfile: ", outfile, sep = ''))
    write.csv(data_raw_1, outfile, row.names = FALSE, quote = FALSE)
    
    # harmonize the outfile
    harmo_file <- paste(file_name, "_postharmo_clean", '.csv', sep="")
    print(paste("creating harmo file: ", harmo_file, sep = ''))
    print("------------")
    #harmonize(outfile, harmo_file, scanertype=FALSE, Eb=FALSE, Mean_only=TRUE, dataset=TRUE, Age=TRUE)
    harmonize(outfile, harmo_file, scanertype=FALSE, dataset=FALSE, Eb=FALSE, Mean_only=FALSE, Parametric=TRUE, Age=FALSE, gen_plot=FALSE)
  }
}

process_all_melissa <- function(clean_leftover=FALSE) {
  region_list0 <- list("Left_Lateral_Ventricle",
                      "Left_Inf_Lat_Vent",
                      "Left_Cerebellum_White_Matter",
                      "Left_Cerebellum_Cortex",
                      "Left_Thalamus",
                      "Left_Caudate",
                      "Left_Putamen",
                      "Left_Pallidum",
                      "Third_Ventricle",
                      "Forth_Ventricle",
                      "Brain_Stem",
                      "Left_Hippocampus",
                      "Left_Amygdala",
                      "CSF",
                      "Left_Accumbens_area",
                      "Left_VentralDC",
                      "Left_vessel",
                      "Left_choroid_plexus",
                      "Right_Lateral_Ventricle",
                      "Right_Inf_Lat_Vent",
                      "Right_Cerebellum_White_Matter",
                      "Right_Cerebellum_Cortex",
                      "Right_Thalamus",
                      "Right_Caudate",
                      "Right_Putamen",
                      "Right_Pallidum",
                      "Right_Hippocampus",
                      "Right_Amygdala",
                      "Right_Accumbens_area",
                      "Right_VentralDC",
                      "Right_vessel",
                      "Right_choroid_plexus",
                      "WM_hypointensities",
                      "Optic_Chiasm",
                      "CC_Posterior",
                      "CC_Mid_Posterior",
                      "CC_Central",
                      "CC_Mid_Anterior",
                      "CC_Anterior",
                      "BrainSegVol",
                      "BrainSegVolNotVent",
                      "lhCortexVol",
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
                      "BrainSegVol_to_eTIV",
                      "MaskVol_to_eTIV",
                      "lhSurfaceHoles",
                      "rhSurfaceHoles",
                      "SurfaceHoles",
                      "eTIV")

  region_list_0 <- list("eTIV")
  region_list_1 <- list("BrainSegVol", "TotalGrayVol")
  region_list_2 <- list("Brain_Stem")
  region_list <- list("BrainSegVol",
                      "BrainSegVolNotVent",
                      "Total_Ventricle")


  gen_raw_harmo_data()

  #data_raw <- read.csv("raw_raw_new_underscore_clean.csv", stringsAsFactors = TRUE, check.names=FALSE)
  data_raw <- read.csv("raw_raw_new_underscore_w_combined_ventricle_clean.csv", stringsAsFactors = TRUE, check.names=FALSE)
  #data_harmo <- read.csv("raw_raw_new_underscore_postharmo_clean.csv", stringsAsFactors = TRUE, check.names=FALSE)
  data_harmo <- read.csv("raw_raw_new_underscore_w_combined_ventricle_postharmo_clean.csv", stringsAsFactors = TRUE, check.names=FALSE)
  
  gen_4_sheet_all_regions(data_raw, data_harmo, sex=3, 
                          region_list=region_list, outfile="melissa_anova_scannertype_as_key.xlsx", 
                          genTukeyHSD=TRUE, genShadedPlot=TRUE, zscore_threshold=3.0)
  
  gen_plot_all_region(region_list, 3)
  
  categorize_data(clean_leftover)
}

##############














