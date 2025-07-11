## Classifying Newspaper Articles for Wildlife Value Orientation :
### Objectives:
Classify a corpus of grizzly bear articles for wildlife value orientations (WVO) in order to:

- determine if there is a shift in WVO framing similar to Manfredo et al., 2021
- determine if external pressures impact those trends such as
  - the type of newspaper coverage (local and in bear range, local and not in bear range, "elite" national)
  - ownership of newspaper (conglomerate or not)
  - topic (bear attack or not)
  - time (year, months or years before or after a specific event)

### Methodology:
- Supervised classification. Final algorithm to be determined. 
- Some sort of multilevel or multivariate glm

### Anticipated Results:
- Time series showing the % mutual newspapers through year for the different newspaper types
- 

## Installation Instructions:
*Running this code requires the following libraries.*

## Usage Instructions:
**Note:** These usage instructions are for testing. This project is in progress, so there are still many testing scripts and few final scripts.

In this project there are three script directories:

  1. `scripts/final`
  2. `scripts/test`
  3. `quarto_docs`
  
Within the `scripts/final` directory are three (mostly) final scripts for 

 1. downloading the articles that were coded in the VIP class `00_article_download.R`
 2. creating the dataframe with the article text, the codes, and other variables `01_initial_create_article_dataframe.R`
 3. some text cleanup `02_text_preprocessing.R`. There is more text preprocessing in the `model_training_tuning.qmd` quarto doc

Within the `scripts/test` directory are *many* testing and scratch scripts. The only scripts that may be of interest are:

  1. `03_classification.R` the initial model training. However, using this script I could not actually use the trained models to predict unlabeled data. I then switched to using `textrecipes`.
  2. Scripts starting with `_tidymodels_*.R` are the scripts I used to test out the individual classification models using `tidymodels` and `textrecipes`. Once these were tested, I transferred the code in the `model_training_tuning.qmd` quarto doc.
  3. `_grizzly_article_classification_tdm.R` is the script with a copy of the code used in the TDM studio to classify the grizzly bear corpus using the trained models. *This code will only work in our group TDM workbench*
  4. `_grizzly_preds_local_national_edu.R` Within this script I attempt some visualizations with the resulting WVO predictions on an early version of the corpus using the models trained in the quarto doc. This code is not well commented, but I will continue to update comments and code in this script. 
     - The data files `grizzly_bear_05_preds_gamma.csv` contain the article ID, Title, Date, Total_Gamma, reg_05_pred_class, knn_05_pred_class, and rf_05_pred_class. This file was downloaded from the TDM studio where the actualy prediction had to take place. 

**Note:** the `05` in the file and variable name corresponds to a gamma threshold. The gamma value represents the... The `05` is a 0.5 gamma threshold, `055` is a 0.55 gamma threshold, etc.


Within the home project directory is the `quarto_docs` folder. This contains the `model_training_tuning.qmd` file. This is where three different supervised classification models were trained:

1. regression
2. k nearest neighbors
3. random forest

The trained models were saved as RDS files and uploaded to the TDM server. These models were then used to predict the values of a corpus of grizzly bear articles.
these predictions were saved as the `grizzly_bear_0*_preds_gamma.csv` files and downloaded from TDM. 



## Contributing:
- If you are open to contributions, provide guidelines for contributing to the project.

## License:
- [Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/deed.en)

## Contact Information:
- For questions related to the code please email Katie Murenbeeld at 
[katiemurenbeeld@boisestate.edu](mailto:katiemurenbeeld@boisestate.edu)



