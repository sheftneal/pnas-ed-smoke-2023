# Emergency department visits respond non-linearly to wildfire smoke

Replication materials for Heft-Neal, Gould, Childs, Kiang, Nadeau, Duggan, Bendavid, and Burke 2023 PNAS paper [`Emergency department visits respond non-linearly to wildfire smoke'](link).

The materials in this repository includes public data and the scripts used to produce the figures and calculations appearing in the main text and extended data of the paper.

Nonpublic data including all measures of ED visits that are the outcomes for most of the analysis in the paper are not included here. Additional details included below. 

If you find meaningful errors in the code or have questions or suggestions, please contact Sam Heft-Neal at sheftneal@stanford.edu.

## Organization of repository

* **scripts**: scripts for downloading data and for producing figures and calculations.
* **figures/raw**: pdf figures output by R.
* **figures/published**: published versions of the figures. Cosmetic editing done in Adobe Illustrator to go from raw -> final versions.
* **data/inputs**: public data inputs for analysis.

* **PNAS-ed-smoke-replication.Rproj**: organizes the replication materials into an RStudio Project.





***A couple of notes on replication:***


* **A note about replication with HCAI data**: Ideally we would like to provide replication materials for the entire pipeline beginning with the raw individual level ED visit data and ending with the results presented in the paper. However, due to privacy concerns and the HCAI data use agreement, we cannot post the ED visit data. Instead, we have tried to include sufficiently detailed information so that any researcher with access to HCAI's ED visit dataset could reconstruct our analysis data exactly. If any steps are unclear please contact Sam at sheftneal@stanford.edu. 



## Instructions

***Overview of the main replication materials:***

The repository is ~XMb (and >XGb if you download all the raw data using script 00)

Users can manage replication through the R project "PNAS-ed-smoke_replication.Rproj". Alternatively users can set working directory to PNAS-ed-smoke and run scripts independently.

* **data/inputs/analysis_data.rds** This file includes the final processed data used in the analysis with all ED visit variables removed. This file can be merged with a processed ED visit dataset (not included, see above) to reproduce the main results.

* **data/inputs/** Includes a combination of empty directories that are populated when running script 00 and directories with pre-processed data that are not used in the main analysis but are used for generating some components of figures.

* **Scripts**

    Scripts with prefix 00_ download data inputs for the analysis. RUNNING THESE IS OPTIONAL. R scripts have been divided into processing scripts and scripts that generate results. Pre-processed versions of these data are inclueed in analysis_data.rds, or in files in the directory data/figure_data, which are called by the scripts that generate the figures. Therefore if you only want to generate the results and figures from pre-processed data then you can skip this step. If you want to replicate data processing then this step is required.

    Script 01 processes the data for figure 1 (script 00 must be run first)

    Script 02 generates figure 1

    Script 03 generates figure 2 panel a

    Script 04 processes data for figure 2 panels b and c (slow, estimated runtime >30 min if you run all 1,000 iterations)

    Script 05 generates figure 2 panels b and c 

    Script 06 generates figure 3 panel a

    Script 07 generates figure 3 panel b

    Script 08 processes data for figure 4 (script 00 must be run first. slow, estimated runtime >15 min if you run 1,000 iterations)

    Script 09 generates figure 4

    Script 10 processes data for figure 5 (script 00 must be run first. Estimated runtime >5 min if you run all 1,000 iterations)

    Script 11 plots figure 5 (> 1 min if you run 1,000 iterations)

    Script 12 plots figure ED1

    Script 13 processes the data for figure ED2 (script 00 must be run first. Estimated runtime >10 min)

    Script 14 generates figure ED2

    Script 15 processes the data for figure ED3 (script 00 must be run first)

    Script 16 generates figure ED3

    Script 17 generates figure ED4

    Script 18 generates figure ED5

    Script 19 will process the raw exposure data for all DHS locations. Note the processed output cannot be merged with the birth data. This script is a work in progress (see above for details).



## R packages required
* **classInt**
* **data.table**
* **fields**
* **latex2exp**
* **lfe**
* **plotrix**
* **R.utils**
* **RANN**
* **raster**
* **sp**
* **tidyverse**

Scripts were originally run with R 4.2.3.

Users can run the following command to install the most recent versions of these packages:

```R
install.packages(c('classInt', 'data.table', 'fields', 'latex2exp', 'lfe', 'plotrix', 'R.utils', 'RANN', 'raster', 'sp', 'tidyverse'), dependencies = T)
```


 



