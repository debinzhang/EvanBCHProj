library(ggplot2)
library(dplyr)
# library(XLConnect)
# 
# library(reshape2)
# library(reticulate)  # this package enable python in R
# 
# library(gamlss)

clean_data <- function(file_name) {
  data_raw <- read.csv(file_name, stringsAsFactors = FALSE)
  row_num_before <- nrow(data_raw)
  data_clean <- data_raw[ which(data_raw$Age!="" & data_raw$Age!="Siemens"  & data_raw$Age<90 &
                                  (data_raw$Sex==1 | data_raw$Sex==2 |
                                   data_raw$Sex=="M" | data_raw$Sex=="m" | data_raw$Sex=="F" |
                                   data_raw$Sex=="f") &
                                   data_raw$Dataset!="" & data_raw$Total_Ventricle!="" &
                                   data_raw$BrainSegVol!="" & data_raw$BrainSegVolNotVent!=""
                                  
  ),  ]
  row_num_after <- nrow(data_clean)
  print(paste("row number before:", row_num_before, "...row number after:", row_num_after))
  outfile <- paste(file_name, "_clean", ".csv", sep="")
  write.csv(data_clean, outfile, row.names = FALSE, quote=FALSE)
  return(outfile)
}

draw_combined_plot <- function(file_name) {
  data <- read.csv(file_name, stringsAsFactors = FALSE)
  print(paste("number_of_plot_rows:", nrow(data)))
  
  colors <- c("BrainSegVol"= "red", "BrainSegVolNotVent"="blue", "Total_Ventricle"="green")
  
  # scanner_type and Magnetic_field_of_strength maybe NA in gg_data that breaks gamlss. Remove these columns
  gamlssSubset <- data %>% dplyr::select(Dataset, Age, Sex, Total_Ventricle, BrainSegVol, BrainSegVolNotVent)
  
  #model_BrainSegVol <- gamlss(BrainSegVol ~ poly(Age, 5), data=gamlssSubset, family=NO)
  model_BrainSegVol <- gamlss(BrainSegVol ~ pb(Age, df=c(5), bs="cs"), data=gamlssSubset, family=NO)
  #model_BrainSegVolNotVent <- gamlss(BrainSegVolNotVent ~ poly(Age, 5), data=gamlssSubset, family=NO)
  model_BrainSegVolNotVent <- gamlss(BrainSegVolNotVent ~ pb(Age, df=c(5), bs="cs"), data=gamlssSubset, family=NO)
  #model_Total_Ventricle <- gamlss(Total_Ventricle ~ poly(Age, 5), data=gamlssSubset, family=NO)
  model_Total_Ventricle <- gamlss(Total_Ventricle ~ pb(Age, df=c(5), bs="cs"), data=gamlssSubset, family=NO)
  
  data_trend_BrainSegVol <-data.frame(gamlssSubset$Age, fitted(model_BrainSegVol))
  data_trend_BrainSegVolNotVent <-data.frame(gamlssSubset$Age, fitted(model_BrainSegVolNotVent))
  data_trend_Total_Ventricle <-data.frame(gamlssSubset$Age, fitted(model_Total_Ventricle))
  
  title <- "BrainSegVol & BrainSegVolNotVent vs. Total_Ventricle"
  ylab_title <- "BrainSegVol & BrainSegVolNotVent"
  u <- ggplot(data=gamlssSubset, aes(x=Age, y=BrainSegVol))
  u <- u + ggtitle(title) + ylab(ylab_title) + xlab('Age') +
     theme(plot.title = element_text(color="DarkBlue", size=12, family = "Courier", hjust=0.5))
  u <- u+ geom_line(data=data_trend_BrainSegVol, aes(x=gamlssSubset.Age, y=fitted.model_BrainSegVol., color="BrainSegVol"), linewidth=1)
  u <- u+ geom_line(data=data_trend_BrainSegVolNotVent, aes(x=gamlssSubset.Age, y=fitted.model_BrainSegVolNotVent., color="BrainSegVolNotVent"), linewidth=1)
  u <- u+ geom_line(data=data_trend_Total_Ventricle, aes(x=gamlssSubset.Age, y=fitted.model_Total_Ventricle.*21, color="Total_Ventricle"), linewidth=1)
  u <- u+ scale_y_continuous(name="BrainSegVol & BrainSegVolNotVent", sec.axis = sec_axis(~./21, name="Total_Ventricle")) 
  u <- u+ scale_color_manual(values = colors)
  u
  
}

setwd("/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/02_04_23_melissa_volume_trendline/combined_trendlines")
clean_file <- clean_data("simplified_raw_raw_new_underscore_w_combined_ventricle.csv")
print(clean_file)

draw_combined_plot(clean_file)
