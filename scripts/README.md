# Migratory species strongly affect seabird biomass in seasonal assemblages off northeast Aotearoa/New Zealand

This `.scripts/README` guide you through the scripts used.

## General workflow and format

The scripts are named following a numerical order, so its is easy to follow (numbers relate to script names):

![Fig 1. Scripts workflow](https://github.com/nwdaudt/far-out_seabirds/blob/main/scripts/scripts-workflow.png)

All scripts were developed using [RStudio IDE](https://posit.co/products/open-source/rstudio/) and, for the sake of facility, I structured them into 'code sections' (`####`). So, if you use RStudio, you can easily follow the *document outline*.

Below, I provide a brief description underlying what each script does.

***

These were the main scripts used for data wrangling and analyses (solid-line squares, in the figure above).

* **'00a': R environment**
  - Sets up R environment by calling `renv::restore()`.
* **'00b': Create `{sf}` transects**
  - Create the transects as spatial objects within R.
* **'01': Tidy up raw data**
  - Standardise, tidy up and merge two datasets;
  - (both are from Far Out, but used different data collection templates).
* **'02': Spatial and seabird count-duration filters**
  - Spatial and duration (time of each seabird count) filters.
* **'03-1': Temperature Depth Recorder (TDR) data**
  - Read TDR data, spatial filter, link each profile to its voyage;
  - Interpolate values and plot TDR + MLD (**Figure 4**);
  - Plot top-10m-averaged SST and MDL spatially.
* **'03-2': Chlorophyll-a (CHL) and Sea Surface Temperatures (SST) data**
  - Read, plot and extract CHL data for each voyage/seabird count;
  - Download, read and extract SST data for each voyage/seabird count;
  - Summarise CHL and SST data (violin plots and maps) (**Figures S2 and S3**).
* **'04': Exploratory Data Analysis (EDA) and data summaries**
  - Prepare 'biomass' data;
  - Summarise species richness, number of individuals, density and biomass by transect/season (**Figure 3**);
  - Map densities by groups (**Figure 2**) and species [not shown, following CARE];
  - Calculate frequencies of occurrence and numeric frequencies for each species (**Table S2**);
  - Summarise estimated biomass per season/group/species (**Figure 8**);
  - Explore the relationship between eight species and environmental gradients (**Figure S1**).
* **'05': Multivariate, model-based ordinations (GLLVMs)**
  - Fit null models (**Figure 5; Table S3; Figure S4**);
  - Fit full models;
  - Covariate selection based on best full GLLVM model (**Table S3; Figure S5**);
  - Coefficient plots for the chosen best GLLVM model including predictors (**Figures 6 and S6**);
  - Co-occurrence plots based on GLLVM models (**Figure 7**);
  - Rarefaction curves (**Figure S7**).

***

Script **'source'** has functions to help deal with spatial object manipulation within script '00b'.

**Figure 1** was tiled together outside R (using [Inkscape](https://inkscape.org/) software), but built from outputs from script **'study-area-map'** and stored in `./results/EDA/study-area/` [folder not tracked]. Currents and oceanographic features from Figure 1A were edited within Inkscape. 

**Table S1** was written manually and is not part of any script.

***

## Manuscript

After running the above scripts, you will get all the results needed to compile the manuscript. The source file for the manuscript is at `./rmd_tex/TeX/ms-JMARSYST.tex`. 

**Note:** To avoid duplicating files, I have **not** copied all Figs to this directory (as Elsevier requests that figures be in the same directory as the TeX file) -- so you may need to do it yourself if you want to compile the exact PDF. The only figures I have included in the TeX directory are two PNG figures that live in `./results/EDA/`, so you can rebuild the manuscript PDF yourself.
