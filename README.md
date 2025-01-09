# Emergency department visits respond non-linearly to wildfire smoke

(** IN PROGRESS **)

Code and select data for Heft-Neal, Gould, Childs, Kiang, Nadeau, Duggan, Bendavid, and Burke 2023 PNAS paper [`Emergency department visits respond non-linearly to wildfire smoke'](https://www.pnas.org/doi/10.1073/pnas.2302409120).

The materials in this repository includes public data and the scripts used to produce the figures and calculations appearing in the main text and extended data of the paper. Data frames storing the coefficients corresponding to all main results are also included here in the results directory.

Nonpublic data including all measures of ED visits that are the outcomes for most of the analysis in the paper are not included here. Additional details below. 

If you find meaningful errors in the code or have questions or suggestions, please contact Sam Heft-Neal at sheftneal@stanford.edu.

## Organization of repository

* **scripts**: scripts for producing figures and calculations.
* **figures/raw**: pdf figures output by R scripts.
* **figures/published**: published versions of the figures. Cosmetic editing done in Adobe Illustrator to go from raw -> final versions.
* **data/inputs**: public data inputs for analysis.
* **results**: includes data frames (rds + csv) with the coefficient estimates corresponding to the results presented in the paper. See results/readme.txt for more details on each results file.


* **PNAS-ed-smoke-replication.Rproj**: organizes the replication materials into an RStudio Project.





***A couple of notes on replication:***


* **A note about replication with HCAI data**: Due to privacy concerns and the HCAI data use agreement, we cannot post the ED visit data. In light of these restrictions, we have tried to include sufficiently detailed information so that any researcher with access to HCAI's ED visit dataset could reconstruct our analysis data exactly. If any steps are unclear please contact Sam at sheftneal@stanford.edu. 



## Instructions

***Overview of materials:***

* **data/analysis_data.rds** This file includes the final processed data used in the analysis with all ED visit variables removed. This file can be merged with a processed ED visit dataset (not included, see above) to reproduce the main results.

* **data/fig*** Pre-processed inputs for figure generation.

* **scripts**

    00_init.R: script defining functions and loading packages (called by other scripts) 
    1-processing: scripts that go from raw data to processed data
    2-analysis-main-figs: scripts for generating the main text figures and results
    3-analysis-sup-figs: scripts for generating supplemental figures and results

 
Scripts were originally run with R 4.2.3.
