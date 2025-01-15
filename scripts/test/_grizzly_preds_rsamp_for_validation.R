library(tidyverse)

#---Load the data----
## use this because it has the most articles
gbear_05_preds <- read_csv(here::here("output/predictions/grizzly_bear_05_preds_gamma.csv"))


print(nrow(gbear_05_preds) * 0.05)

#get a sample of 10
set.seed(2486)
samp_5pct <- sample(nrow(gbear_05_preds),273)

#data in the sample
samp_5pct_df <- gbear_05_preds[samp_5pct,]


hist(samp_5pct_df$reg_05_pred_class)
hist(samp_5pct_df$knn_05_pred_class)
hist(samp_5pct_df$rf_05_pred_class)

write_csv(samp_5pct_df, here::here(paste0("output/predictions/gbear_preds_sample_to_handcode_",
                                          Sys.Date(), ".csv")), append = FALSE)
