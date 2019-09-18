# Earliest infections predict the age distribution of seasonal influenza A cases
Data and Code for [Arevalo, et al., 2019 (Earliest infections predict the age distribution of seasonal influenza A cases)](https://www.medrxiv.org/content/10.1101/19001875v2).

# Dependencies

* [Miniconda with python 3.7](https://docs.conda.io/en/latest/miniconda.html)

The required python and R packages can be installed by creating a conda environment with the included `FluAImprinting.yml` file as follows:

`conda env create -f FluAImprinting.yml`

# Directory contents

* `raw_data` contains raw case data used to fit the model and other data sources used in the calculation of imprinting probabilities.
* `data_processing` contains scripts to reformat and refactor data.
* `data` contains data inputs used for modelling and imprinting probability calculation.
* `models` contains all models tested.
* `fitting_scripts` contains scripts used to fit models and process fit outputs.
* `simulations` contains scripts used to fit models to simulated data.
* `vaccine_imprinting` contains scripts used to fit models with imprinting conferred via vaccination.
* `figures` contains jupyter notebooks with code used to produce all figures in the manuscript.
* `final_results_for_ms` contains model fits used in the manuscript.
