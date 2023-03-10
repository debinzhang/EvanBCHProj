library(dplyr)
library(ggplot2)
df <- read.csv("eTIV_raw_no_all.csv")

# Create a new column for age range
df$age_range <- cut(df$Age, breaks = seq(0, 100, by = 2))

# Calculate mean weight by age range
df_mean <- aggregate(eTIV ~ age_range, data = df, FUN = mean)

# Plot mean weight by age range using ggplot2
ggplot(data = df_mean, aes(x = age_range, y = eTIV)) +
  geom_col() +
  labs(x = "Age Range", y = "Mean eTIV") +
  theme(plot.title = element_text(color="DarkBlue", size=12, family = "Courier", hjust=0.5)) +
  theme(axis.text.x=element_text(size=6), axis.text.y=element_text(size=8),
        axis.title=element_text(size=10)) +
  ggtitle("eTIV by age group for data with outlier removed")




