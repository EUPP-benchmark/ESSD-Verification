# ESSD-Verification
Repository for the verification routines and plots for ESSD.

The code is provided as supplementary material with

* Demaeyer, J., Bhend, J., Lerch, S., Primo, C., Van Schaeybroeck, B., Atencia, A., Ben Bouallègue, Z., Chen, J., Dabernig, M., Evans, G., Faganeli Pucer, J., Hooper, B., Horat, N., Jobst, D., Merše, J., Mlakar, P., Möller, A., Mestre, O., Taillardat, M., and Vannitsem, S.: The EUPPBench postprocessing benchmark dataset v1.0, Earth Syst. Sci. Data Discuss. [preprint], https://doi.org/10.5194/essd-2022-465, in review, 2023.

**Please cite this article if you use (a part of) this code for a publication.**

## Installation

Pull the repository from github:

```
git clone https://github.com/EUPP-benchmark/ESSD-Verification.git
cd ESSD-Verification
```

## Data
Download the verification data from Zenodo at https://doi.org/10.5281/zenodo.7469465

## Reproduce Figures

```
Rscript -e "if (!requireNamespace('rmarkdown')) install.packages('rmarkdown')" -e "rmarkdown::render('verification_for_ESSD.Rmd', output_format = 'pdf_document')"
```
