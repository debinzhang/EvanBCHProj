box_src_file <- "all_subjects_cortical_metrics_LH_curvind_09_15_2022_preHarmo_w_unknown_removed.csv"
box_data <- read.csv(box_src_file, stringsAsFactors = FALSE)

u_vol <-ggplot(box_data) + aes(x = Dataset, y = Volume) + geom_boxplot()