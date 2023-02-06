box_src_file <- "all_subjects_cortical_metrics_LH_curvind_09_15_2022_preHarmo_w_unknown_removed.csv"
box_data <- read.csv(box_src_file, stringsAsFactors = FALSE)

u_volume <- ggplot(box_data) + aes(x = Dataset, y = Volume) + geom_boxplot()
u_etiv <- ggplot(box_data) + aes(x = Dataset, y = eTIV) + geom_boxplot()

box_etiv_raw_zscore_file <- "eTIV_raw_all.csv"
box_etiv_raw_zscore_data <- read.csv(box_etiv_raw_zscore_file, stringsAsFactors=FALSE)
u_etiv_zscore_raw <- ggplot(box_etiv_raw_zscore_data) + aes(x = Dataset, y = zscore) + geom_boxplot()

box_etiv_raw_no_zscore_file <- "eTIV_raw_no_all.csv"
box_etiv_raw_no_zscore_data <- read.csv(box_etiv_raw_no_zscore_file, stringsAsFactors=FALSE)
u_etiv_zscore_raw_no <- ggplot(box_etiv_raw_no_zscore_data) + aes(x = Dataset, y = zscore) + geom_boxplot()
u_etiv_zscore_raw_no


box_etiv_harmo_no_zscore_file <- "eTIV_harmo_no_all.csv"
box_etiv_harmo_no_zscore_data <- read.csv(box_etiv_harmo_no_zscore_file, stringsAsFactors=FALSE)
u_etiv_zscore_harmo_no <- ggplot(box_etiv_harmo_no_zscore_data) + aes(x = Dataset, y = zscore) + geom_boxplot()
u_etiv_zscore_harmo_no

# calcuate standard deviation by group
box_etiv_raw_no_zscore_file <- "eTIV_raw_no_all.csv"
box_etiv_raw_no_zscore_data <- read.csv(box_etiv_raw_no_zscore_file, stringsAsFactors=FALSE)
box_etiv_raw_no_zscore_data %>% group_by(Dataset) %>% summarise_at(vars(zscore), list(zscore_sd_by_dataset=sd))

# A tibble: 9 × 2
  Dataset   zscore_sd_by_dataset
  <chr>                    <dbl>
1 ABIDE_I                  0.893
2 BCH                      0.830
3 beijingEn                0.891
4 BGSP                     0.849
5 DLBS                     0.875
6 IXI_600                  0.882
7 MGH                      0.923
8 NIH_PD                   0.844
9 OASIS_3                  0.857


box_etiv_harmo_no_zscore_file <- "eTIV_harmo_no_all.csv"
box_etiv_harmo_no_zscore_data <- read.csv(box_etiv_harmo_no_zscore_file, stringsAsFactors=FALSE)
box_etiv_harmo_no_zscore_data %>% group_by(Dataset) %>% summarise_at(vars(zscore), list(zscore_sd_by_dataset=sd))

  Dataset   zscore_sd_by_dataset
  <chr>                    <dbl>
1 ABIDE_I                  0.911
2 BCH                      0.847
3 beijingEn                0.908
4 BGSP                     0.861
5 DLBS                     0.889
6 IXI_600                  0.865
7 MGH                      0.942
8 NIH_PD                   0.802
9 OASIS_3                  0.868

# change categorical_cols = ['gender', 'scannerType']
# A tibble: 9 × 2
  Dataset   zscore_sd_by_dataset
  <chr>                    <dbl>
1 ABIDE_I                  0.910
2 BCH                      0.845
3 beijingEn                0.906
4 BGSP                     0.860
5 DLBS                     0.887
6 IXI_600                  0.869
7 MGH                      0.940
8 NIH_PD                   0.805
9 OASIS_3                  0.867



