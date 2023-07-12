# Emergency department visits respond non-linearly to wildfire smoke

Replication materials for Heft-Neal, Gould, Childs, Kiang, Nadeau, Duggan, Bendavid, and Burke 2023 PNAS paper [`Emergency department visits respond non-linearly to wildfire smoke'](link).

The materials in this repository allow users to reproduce the figures and calculations appearing in the main text and extended data of the paper.

If you find meaningful errors in the code or have questions or suggestions, please contact Sam Heft-Neal at sheftneal@stanford.edu.

## Organization of repository

* **scripts**: scripts for downloading data and for replication of figures and calculations.
* **figures/raw**: scripts will generate pdf figures in this directory.
* **figures/published**: published versions of the figures. Cosmetic editing done in Adobe Illustrator to go from raw -> final versions.
* **data/inputs**: data inputs for analysis.
* **data/figure_data**: data used for any figures that don't use the main analysis data. Directory includes pre-processed files with replication materials. Anytime a *dataPrep.R script runs it generates new figure data in files of the same name but ending with "_new" and all subsequent runs of plotting scripts bring in the new data instead of the pre-processed data included with replication materials.
* **PNAS-ed-smoke_replication.Rproj**: organizes the replication materials into an RStudio Project.





***A couple of notes on replication:***


* **A note about replication with HCAI data**: Ideally we would like to provide replication materials for the entire pipeline beginning with the raw individual level ED visit data and ending with the results presented in the paper. However, due to privacy concerns and the HCAI user agreement, we cannot post the individual level data. Instead, we have tried to include sufficiently detailed information so that any researcher with access to HCAI's ED visit dataset could reconstruct our analysis data exactly. If any steps are unclear please contact Sam at sheftneal@stanford.edu. 



## Instructions

***Overview of the main replication materials:***

The repository is ~XMb (and >XGb if you download all the raw data using script 00)

Users can manage replication through the R project "PNAS-ed-smoke_replication.Rproj". Alternatively users can set working directory to PNAS-ed-smoke and run scripts independently.

* **data/inputs/analysis_data.rds** This file includes the final processed data used in the analysis with all location identifying information removed. This file can be used to replicate the results in the paper but because identifying information has been removed, it cannot be used to replicate pre-processing since those steps require linking exposures to individual zipcode-day observations based on location and timing of births.

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


 



