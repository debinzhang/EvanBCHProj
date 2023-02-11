library(ggplot2)
library(dplyr)

wd <- getwd()
setwd(wd)

draw_trendline_by_age <- function(feature, srcfile) {
  print(paste("working on ", feature, " ...", sep = ''))
  file_path = paste(srcfile, ".csv", sep = '')
  title<-paste(feature, ' ', srcfile, " trendlines by Sex", sep='')
  data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
  
  print("Evan 1")
  
  u<- ggplot(data = data_raw, aes(x = Age, y=get(feature), group=Sex, color = Sex)) +
  #u<- ggplot(data = data_raw, aes(x = Age, y=feature, group=Sex, color = Sex)) +
    #geom_smooth(method = "lm") +
    geom_point(alpha=0) + 
    geom_smooth(aes(color=Sex), method="gam", formula = y ~ s(x, bs = "cs", k=5), linewidth=0.5) +
    labs(y=feature, x = "Age") + ggtitle(title) + 
    theme(legend.position = c(0.90, 0.90), legend.background = element_rect(fill = "white", color = "black"))
  
  print("Evan 2")
  
  u<- u + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=10))
  print("Evan 3")
  u
}


draw_trendline_by_hemisphere <- function(feature, srcfile) {
  print(paste("working on ", feature, " ...", sep = ''))
  
  file_path = paste(feature, srcfile, "_trendline_hemisphere.csv", sep = '')
  if (grepl("raw", srcfile)) {
    title<-paste("Raw ", feature, " with outlier removed trendlines by hemisphere", sep='')
  } else {
    title<-paste("Harmonized ", feature, " with outlier removed trendlines by hemisphere", sep='')
  }

  data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
  print("Evan 1000")
  u<- ggplot(data = data_raw, aes(x = Age, y=get(feature), group=Hemisphere, color = Hemisphere)) +
    #geom_smooth(method = "lm") +
    geom_point(alpha=0) + 
    geom_smooth(aes(color=Hemisphere), method="gam", formula = y ~ s(x, bs = "cs", k=5), linewidth=0.5) +
    labs(y=feature, x = "Age") + ggtitle(title) + 
    theme(legend.position = c(0.90, 0.90), legend.background = element_rect(fill = "white", color = "black"))
  
  u<- u + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=10))
  
  u
}

gen_gender_trendline_plots <- function(region_list) {
  srcfile_list <- list("_raw_all", "_raw_no_all", "_harmo_all", "_harmo_no_all")
  for (feature in region_list) {
    for (srcfile in srcfile_list) {
      fullSrcfile <- paste(feature, srcfile, sep = '')
      draw_trendline_by_age(feature, fullSrcfile)      
      print("Evan 100")
      filename <- paste(fullSrcfile,  "_trendlines_by_sex.png", sep = '')
      print("Evan 200")
      ggsave(filename)
    }
  }
}


# this function trendline plots that show left and right hemisphere comparision
# seem no need to draw them for pre-outlier removal data. So we only plot post-outlier data
gen_hemisphere_trendline_plots<- function(right_feature_list) {
  srcfile_list <- list("_raw_no_all", "_harmo_no_all")
  for (rh_feature in right_feature_list) {
    for (srcfile in srcfile_list) {
      # get feature name without hemsphere prefix. like 
      # Right_Lateral_Ventricle -> Lateral_Ventricle
      feature = substr(rh_feature, 4, nchar(rh_feature))
      lh_feature = paste("lh_", feature, sep='')
      data_lh <- read.csv(paste(lh_feature, srcfile, ".csv", sep=''), stringsAsFactors=TRUE, check.names=FALSE)
      data_rh <- read.csv(paste(rh_feature, srcfile, ".csv", sep=''), stringsAsFactors=TRUE, check.names=FALSE)
      
      # rename lh_entorhinal_thickness to entorhinal_thickness
      colnames(data_lh)[colnames(data_lh) == lh_feature] = feature
      # add column "hemsphere" and set value "left"
      data_lh["Hemisphere"] = 'Left'
      
      # rename rh_entorhinal_thickness to entorhinal_thickness
      colnames(data_rh)[colnames(data_rh) == rh_feature] = feature
      # add column "hemsphere" and set value "right"
      data_rh["Hemisphere"] = 'Right'
      
      # combined the left and right table
      combined_data = rbind(data_lh, data_rh)
      
      outfile = paste(feature, srcfile, "_trendline_hemisphere.csv", sep='')
      write.csv(combined_data, outfile, row.names=FALSE, quote=FALSE)
      
      print(paste("generating:", outfile))
      draw_trendline_by_hemisphere(feature, srcfile)
      
      filename <- paste(feature, srcfile, "_trendlines_by_hemisphere.png", sep = '')
      print("Evan 1000")
      ggsave(filename)
    }
  }
}

#region_list <- list("eTIV", "Volume", "BrainSegVolNotVent")
region_list_x <- list("BrainSegVolNotVent", "eTIV", "lh_bankssts_curvind", "lh_caudalanteriorcingulate_curvind")

lh_region_list <- list("BrainSegVolNotVent", "eTIV",
                               "lh_bankssts_curvind",
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








# gen_gender_trendline_plots(rh_curvind_region_list)
# gen_hemisphere_trendline_plots(rh_curvind_region_list)
# gen_hemisphere_trendline_plots(rh_curvind_region_list)
  
  
gen_gender_trendline_plots(lh_thickness_region_list)
gen_gender_trendline_plots(rh_thickness_region_list)
gen_hemisphere_trendline_plots(rh_thickness_region_list)



######################












hemi_list <- list("Right_Lateral_Ventricle",
                  "Right_Inf_Lat_Vent",
                  "Right_Cerebellum_White_Matter",
                  "Right_Cerebellum_Cortex",
                  "Right_Thalamus",
                  "Right_Caudate",
                  "Right_Putamen",
                  "Right_Pallidum",
                  "Right_Accumbens_area",
                  "Right_VentralDC",
                  "Right_vessel",
                  "Right_choroid_plexus",
                  "Right_Hippocampus",
                  "Right_Amygdala")

hemi_list_2 <- list("Right_SurfaceHoles")

gen_hemisphere_plots(hemi_list)
gen_hemisphere_plots(hemi_list_2)








