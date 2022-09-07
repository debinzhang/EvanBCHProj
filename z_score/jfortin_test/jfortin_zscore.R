library(ggplot2)
#library(tidyverse)
install.packages("dplyr")
library(dplyr)

# The following code is for testing only
setwd("/Users/dzhang/DebinProjs/R/z_score/jfortin_test/neuroCombat/testdata")
test_data <- read.csv("testdata.csv", stringsAsFactors = TRUE)

getZScore <- function(data, feature) {
  zscore_data<-data %>% mutate(zscore=({{feature}} - mean({{feature}})) / sd({{feature}}))
  #head(zscore_data)
  u<-ggplot(data=zscore_data, aes(y=zscore))
  u <- u + ylim(-2.5, 2.5)
  u + geom_boxplot()
}

getZScore(test_data, V1)
getZScore(test_data, V2)
getZScore(test_data, V3)
getZScore(test_data, V4)
getZScore(test_data, V5)
getZScore(test_data, V6)
getZScore(test_data, V7)
getZScore(test_data, V18)


getDiff <- function(data, feature, feature_name) {
  diff_data<-data %>% group_by(Dataset) %>% mutate(diff=({{feature}} - mean({{feature}})))
  #head(zscore_data)
  u<-ggplot(data=diff_data, aes(x=Dataset, y=diff, fill=Dataset))
  #u + geom_boxplot(alpha=0.5) + geom_jitter()
  u <- u + labs(title="Plot of diff from mean",
           x ="Dataset", y = paste("Diff for ", feature_name))
  #u <- u + ylim(-5,5)
  #u + geom_boxplot(aes(middle=mean(diff)), outlier.shape = NA)
  u + geom_boxplot(aes(middle=mean(diff)))
  #u + geom_boxplot() 
}

getZScore(test_data, V1)

#