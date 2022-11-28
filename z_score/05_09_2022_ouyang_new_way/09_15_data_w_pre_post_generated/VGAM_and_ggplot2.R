#install.packages("VGAM")
#install.packages("reshape2")
library(VGAM)
library(ggplot2)
library(reshape2)

#library(ggplot2)
library(dplyr)
library(XLConnect)

wd <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/09_15_data_w_pre_post_generated"
setwd(wd)

# fit4 <- vgam(BMI ~ s(age, df = c(4, 2)), lms.bcn(zero = 1), data = bmi.nz, trace = TRUE)
# qtplot(fit4, percentiles = c(5,50,90,99), main = "Quantiles", las = 1, xlim = c(15, 90), ylab = "BMI", lwd = 2, lcol = 4) 

dd1 <- bmi.nz
# fitted values from vgam 
fit4 <- vgam(BMI ~ s(age, df = c(4, 2)), lms.bcn(zero = 1), data = bmi.nz, trace = TRUE)
fitted.values <- data.frame(qtplot.lmscreg(fit4, percentiles = c(5,50,90,99))$fitted.values)
fitted.values[, 'age'] <- bmi.nz[, 'age']
# melt data.frame
dmelt <- melt(fitted.values, id.vars='age')
# ploting
ggplot(dmelt, aes(age, value, group=variable)) + 
  geom_line(color='blue') + 
  annotate(geom='text',
           x = max(bmi.nz[, 'age']) + 3,
           y = unlist(fitted.values[which.max(fitted.values[, 'age']), -ncol(fitted.values)]),
           label=c(' 5%', '50%', '90%', '99%')) +
  
  # lapply(2:8*10, function(i) {
  #   annotate(geom='text',
  #            x = i,
  #            y = 1+unlist(fitted.values[which.min(abs(fitted.values[, 'age'] - i)), -ncol(fitted.values)]),
  #            label=paste0(round(unlist(fitted.values[which.min(abs(fitted.values[, 'age'] - i)), -ncol(fitted.values)]),1), '%'))
  # }) +
  scale_y_continuous('BMI') +
  theme_bw(base_size=16)

#################

file_path <- "/Users/dzhang/Gits/EvanBCHProj/z_score/05_09_2022_ouyang_new_way/09_15_data_w_pre_post_generated/lh_insula_thickness_raw_no_outlier_male.csv"
feature <- 'lh_insula_thickness'
gg_data0 <- read.csv(file_path, stringsAsFactors = FALSE)
gg_data <- gg_data0 %>% filter(Age !="")
print(head(gg_data, n=5))

fit4 <- vgam(lh_insula_thickness ~ s(Age, df = c(4, 2)), lms.bcn(zero = 1), data = gg_data, trace = TRUE)
fitted.values <- data.frame(qtplot.lmscreg(fit4, percentiles = c(2,25,50,75,98))$fitted.values)
fitted.values[, 'Dataset'] <- gg_data[, 'Dataset']
fitted.values[, 'Age'] <- gg_data[, 'Age']
fitted.values[, 'Sex'] <- gg_data[, 'Sex']
fitted.values[, 'lh_insula_thickness'] <- gg_data[, 'lh_insula_thickness']

# melt data.frame
dmelt <- melt(fitted.values, id.vars=c('Dataset', 'Age', 'Sex', 'lh_insula_thickness'))

#u <- ggplot(data=gg_data, aes(x=as.numeric(Age), y=lh_insula_thickness, color=Dataset))
#u <- ggplot(data=dmelt, aes(x=as.numeric(Age), y=lh_insula_thickness, color=Dataset))
#u <- ggplot(data=dmelt, aes(x=as.numeric(Age), y=value, color=Dataset))
u <- ggplot(data=dmelt, aes(x=as.numeric(Age), y=lh_insula_thickness, color=Dataset))
u <- u + geom_line(aes(Age, value, group=variable), color='black')
u <- u + geom_point(size=0.5) + 
  #geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5)) + 
  geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=5), se=FALSE) + 
  #geom_smooth(method = "loess") +
  ggtitle("test title") + ylab(feature)+ xlab('Age') 
u<- u + theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5)) +
  theme(axis.text=element_text(size=5))
u <- u + scale_x_continuous(breaks = seq(0,100, by =2)) +
  annotate(geom='text',
           # x = max(bmi.nz[, 'age']) + 3,
           x = 92,
           y = unlist(fitted.values[which.max(fitted.values[, 'Age']), c(1:5)]),
           label=c('2%', '25%', '50%', '75%', '98%'))
#u <- u + scale_x_discrete(breaks = seq(0,100, by =2))
u





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




draw_plot_1 <- function(feature, option=1, sex=3, is_boxplot=FALSE) {
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
  print(head(gg_data, n=3))
  
  u <- ggplot(data=gg_data, aes(x=as.numeric(Age), y=get(feature), color=Dataset))
  u <- u + geom_point(size=0.5) + 
    #geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5)) + 
    #geom_smooth(aes(color=NULL), method="gam", formula = y ~ s(x, bs = "cs", k=5), se=FALSE) + 
    #geom_smooth(method = "loess") +
    ggtitle(title) + ylab(feature)+ xlab('Age') 
  u<- u + theme(plot.title = element_text(color="DarkBlue", size=9, family = "Courier", hjust=0.5)) +
    theme(axis.text=element_text(size=5))
  u <- u + scale_x_continuous(breaks = seq(0,100, by =2))
  #u <- u + scale_x_discrete(breaks = seq(0,100, by =2))
  u
}

draw_plot_1("lh_insula_thickness", 2, 1)
