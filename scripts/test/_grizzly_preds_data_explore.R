library(tidyverse)


#---Load the data----
gbear_05_preds <- read_csv(here::here("output/predictions/grizzly_bear_05_preds_gamma.csv"))
gbear_055_preds <- read_csv(here::here("output/predictions/grizzly_bear_055_preds_gamma.csv"))
gbear_06_preds <- read_csv(here::here("output/predictions/grizzly_bear_06_preds_gamma.csv"))
gbear_065_preds <- read_csv(here::here("output/predictions/grizzly_bear_065_preds_gamma.csv"))

names(gbear_05_preds)

# what could I look at?
# compare histograms of the different models and thresholds 
## (also compare to the training data)
# any relationship between gamma and the classification
# any temporal trends in the predicted classification 