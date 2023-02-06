library(ggplot2)
library(dplyr)

wd <- getwd()
setwd(wd)

draw_trendline_by_age <- function(feature, srcfile) {
  print(paste("working on ", feature, " ...", sep = ''))
  file_path = paste(srcfile, ".csv", sep = '')
  title<-paste(srcfile, " trendlines by Sex", sep='')
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


draw_trendline_by_hemisphere <- function(feature) {
  print(paste("working on ", feature, " ...", sep = ''))
  
  file_path = paste(feature, "_hemisphere.csv", sep = '')
  title<-paste(feature, " trendlines by hemisphere", sep='')
  data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
  print("Evan 1000")
  u<- ggplot(data = data_raw, aes(x = Age, y=get(feature), group=hemisphere, color = hemisphere)) +
    #geom_smooth(method = "lm") +
    #geom_point(alpha=0) + 
    geom_smooth(aes(color=hemisphere), method="gam", formula = y ~ s(x, bs = "cs", k=5), linewidth=0.5) +
    labs(y=feature, x = "Age") + ggtitle(title) + 
    theme(legend.position = c(0.90, 0.90), legend.background = element_rect(fill = "white", color = "black"))
  
  u<- u + theme(plot.title = element_text(color="DarkBlue", size=15, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=10))
  
  u
}


# this function draw plots that shows shaded left and right hemisphere comparision
gen_hemisphere_plots<- function(Right_feature_list) {
  for (rh_feature in Right_feature_list) {
    # get feature name without hemsphere prefix. like 
    # Right_Lateral_Ventricle -> Lateral_Ventricle
    feature = substr(rh_feature, 7, nchar(rh_feature))
    lh_feature = paste("Left_", feature, sep='')
    data_lh <- read.csv(paste(lh_feature, "_raw_no_all.csv", sep=''), stringsAsFactors=TRUE, check.names=FALSE)
    data_rh <- read.csv(paste(rh_feature, "_raw_no_all.csv", sep=''), stringsAsFactors=TRUE, check.names=FALSE)
    
    # rename lh_entorhinal_thickness to entorhinal_thickness
    colnames(data_lh)[colnames(data_lh) == lh_feature] = feature
    # add column "hemsphere" and set value "left"
    data_lh["hemisphere"] = 'left'
    
    # rename rh_entorhinal_thickness to entorhinal_thickness
    colnames(data_rh)[colnames(data_rh) == rh_feature] = feature
    # add column "hemsphere" and set value "right"
    data_rh["hemisphere"] = 'right'
    
    # combined the left and right table
    combined_data = rbind(data_lh, data_rh)
    
    outfile = paste(feature, "_hemisphere.csv", sep='')
    write.csv(combined_data, outfile, row.names=FALSE, quote=FALSE)
    
    print(paste("generating:", outfile))
    #draw_shade_hemisphere_plot(feature)
    draw_trendline_by_hemisphere(feature)
    
    filename <- paste(feature, "_raw_trendlines_by_hemisphere.png", sep = '')
    print("Evan 1000")
    ggsave(filename)
  }
}


region_list <- list("Left_Lateral_Ventricle",
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
                    'eTIV')

draw_gender_shade_plots <- function(region_list) {
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

draw_gender_shade_plots(region_list)




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








