library(dplyr)
library(ggplot2)
#is_google <- TRUE
is_google <- FALSE
if (is_google) {
  df0 <- read.csv("raw_data_from_google_sheet.csv")
} else {
  #df0 <- read.csv("eTIV_raw_no_all.csv")
  #df0 <- read.csv("eTIV_raw_all.csv")
  #df0 <- read.csv("lh_rh_lobes_age_age_interval_clean_20230318.csv")
  #df0 <- read.csv("lh_rh_lobes_age_clean_20230318.csv")
  df0 <- read.csv("eTIV_raw_all_w_rockland.csv")
}


getRobustZScoreByDataset <- function(data, feature) {
  grouped_data <- data %>% group_by(Dataset) %>% summarise_at({{feature}}, list(group_median = median, group_mad = mad))
  merged_data <- merge(data, grouped_data, by="Dataset")
  zscore_data<-merged_data %>% mutate(zscore=(get(feature) - group_median)/group_mad)
  return(zscore_data)
}

rmOutlier <- function(data, low, high) {
  data_no_outlier <- subset(data, data$zscore>low & data$zscore < high)
  data_no_outlier
}

if (is_google) {
  df1 <- subset(df0, (df0$Dataset=="ABCD" | df0$Dataset=="ABIDE_I" | df0$Dataset=="BCH" |
                     df0$Dataset=="beijingEn" | df0$Dataset=="BGSP" | df0$Dataset=="DLBS" |
                     df0$Dataset=="IXI_600" | df0$Dataset=="MGH" | df0$Dataset=="NIH_PD" |
                     df0$Dataset=="OASIS_3") & df0$Age!="" & df0$eTIV!= "" &
                     !is.na(as.numeric(df0$Age)) & !is.na(as.numeric(df0$eTIV)),
                     select=c("Age", "Dataset", "eTIV"))
} else {
  df1 <- subset(df0, df0$Age!="" & df0$eTIV!= "" &
                  !is.na(as.numeric(df0$Age)) & !is.na(as.numeric(df0$eTIV)),
                select=c("Age", "Dataset", "eTIV"))
}

print(paste("Total rows of table:", nrow(df1)))

# change Age and eTIV from string to numeric
df1$Age <- as.numeric(df1$Age)
df1$eTIV <- as.numeric(df1$eTIV)

df2 <- getRobustZScoreByDataset(df1, "eTIV")
df <- rmOutlier(df2, -3.0, 3.0)

print(paste("Total rows of table after outlier removal:", nrow(df)))

if (is_google) {
 write.csv(df, "google_sheet_data_after_outlier_removal.csv")
}

# Create a new column for age range
df$age_range <- cut(df$Age, breaks = seq(0, 100, by = 2))

# Calculate mean weight by age range
df_mean <- aggregate(eTIV ~ age_range, data = df, FUN = mean)
age_grp_index <- gsub("(^\\(|\\]|\\[)(\\d+),.*", "\\2", levels(df_mean$age_range))[df_mean$age_range]
df_mean <- cbind(age_grp_index, df_mean)
df_mean$age_grp_index <- as.numeric(df_mean$age_grp_index) + 2

# output a line plot
u<- ggplot(data = df_mean, aes(x = age_grp_index, y = eTIV)) + geom_point(color="red") + 
  geom_line(color="blue") +
  labs(x = "Age Range", y = "Mean eTIV") +
  theme(plot.title = element_text(color="DarkBlue", size=12, family = "Courier", hjust=0.5)) +
  theme(axis.text.x=element_text(size=6), axis.text.y=element_text(size=8),
        axis.title=element_text(size=10))

  if (is_google) {
    u <- u + ggtitle("Google volume mean by age group for data with outlier removed")
  } else {
    u <- u + ggtitle("eTIV mean by age group for data with outlier removed")
  }
  u
  
  if (is_google) {
    ggsave("google_volume_mean_line_plot.png", width=8, height=6, dpi=300)
  } else {
    ggsave("etiv_mean_line_plot.png", width=8, height=6, dpi=300)
  }
  

# output barplot
t<- ggplot(data = df_mean, aes(x = age_range, y = eTIV)) +
  geom_col() +
  #geom_bar(stat = "identity") +
  labs(x = "Age Range", y = "Mean eTIV") +
  theme(plot.title = element_text(color="DarkBlue", size=12, family = "Courier", hjust=0.5)) +
  theme(axis.text.x=element_text(size=6, angle = 90, vjust = 0.5, hjust=1), axis.text.y=element_text(size=8),
        axis.title=element_text(size=10)) +
  ggtitle("eTIV mean by age group for data with outlier removed")
  if (is_google) {
    t <- t + ggtitle("Google volume mean by age group for data with outlier removed")
  } else {
    t <- t + ggtitle("eTIV mean by age group for data with outlier removed")
  }
  t
  
  if (is_google) {
    ggsave("google_volume_mean_bar_plot.png", width=8, height=6, dpi=300)
  } else {
    ggsave("etiv_mean_bar_plot.png", width=8, height=6, dpi=300)
  }
  
  
# --------------------------- the following are plots for median -------------------

df_median <- aggregate(eTIV ~ age_range, data = df, FUN = median)
age_grp_index <- gsub("(^\\(|\\]|\\[)(\\d+),.*", "\\2", levels(df_median$age_range))[df_median$age_range]
df_median <- cbind(age_grp_index, df_median)
df_median$age_grp_index <- as.numeric(df_median$age_grp_index) + 2

# output a line plot
um <- ggplot(data = df_median, aes(x = age_grp_index, y = eTIV)) + geom_point(color="red") + 
  geom_line(color="blue") +
  labs(x = "Age Range", y = "Median eTIV") +
  theme(plot.title = element_text(color="DarkBlue", size=12, family = "Courier", hjust=0.5)) +
  theme(axis.text.x=element_text(size=6), axis.text.y=element_text(size=8),
        axis.title=element_text(size=10)) 

  if (is_google) {
    um <- um + ggtitle("Google volume median by age group for data with outlier removed")
  } else {
    um <- um + ggtitle("eTIV median by age group for data with outlier removed")
  }
  um

  if (is_google) {
    ggsave("google_volume_median_line_plot.png", width=8, height=6, dpi=300)
  } else {
    ggsave("etiv_median_line_plot.png", width=8, height=6, dpi=300)
  }

  
tm <- ggplot(data = df_median, aes(x = age_range, y = eTIV)) +
  geom_col() +
  labs(x = "Age Range", y = "Median eTIV") +
  theme(plot.title = element_text(color="DarkBlue", size=12, family = "Courier", hjust=0.5)) +
  theme(axis.text.x=element_text(size=6, angle=90, vjust = 0.5, hjust=1), axis.text.y=element_text(size=8),
        axis.title=element_text(size=10)) 

  if (is_google) {
    tm <- tm + ggtitle("Google volume median by age group for data with outlier removed")
  } else {
    tm <- tm + ggtitle("eTIV median by age group for data with outlier removed")
  }
  tm 

 if (is_google) {
   ggsave("google_volume_median_bar_plot.png", width=8, height=6, dpi=300)
 } else {
   ggsave("etiv_median_bar_plot.png", width=8, height=6, dpi=300)
 }




