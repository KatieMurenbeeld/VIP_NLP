## Classifying Newspaper Articles for Wildlife Value Orientation :
### Objectives:
Classify a corpus of grizzly bear articles for wildlife value orientations (WVO) in order to:
- determine if there is a shift in WVO framing similar to Manfredo et al., 2021
- determine if external pressures on values impact those trends such as
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
2. scripts starting with `_tidymodels_*.R` are the scripts I used to test out the individual classification models using `tidymodels` and `textrecipes`. Once these were tested, I transferred the code in the `model_training_tuning.qmd` quarto doc.
3. 


## Contributing:
- If you are open to contributions, provide guidelines for contributing to the project.

## License:
- [Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/deed.en)

## Contact Information:
- For questions related to the code please email Katie Murenbeeld at 
[katiemurenbeeld@boisestate.edu](mailto:katiemurenbeeld@boisestate.edu)



