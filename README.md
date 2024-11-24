# Seasonality rather than oceanography drives assemblages of seabird species off northeast Aotearoa New Zealand

**This is a work in progress. The README and files will be constantly being updated.**

In this study, we used a mix of descriptive spatial and summary analyses, diversity curves and model-based multivariate statistics to describe assemblages of seabird species off Northland, northeast Aotearoa New Zealand.

Data were collected with the [Far Out Ocean Research Collective](https://www.farout.org.nz/). The fieldwork of this study was in the rohe moana (territory) of the Māori iwi (tribe) Ngāti Kuri, who shares the custody of the data. Following the CARE principles and the wish from Ngāti Kuri to not release any data from their territory, the raw data won't be open access. However, we hope to make some data available eventually (likely the 'aggregated' data for multivariate analysis).

The processing of the data and analyses code is, however, all available in this repository for transparency.

***

## Project structure

Workspace is set as shown below. All folders in [square brackets] are not tracked with `git`

```shell
far-out_seabirds
├── README.md
├── [./data-raw] ** untracked folder containing raw data
├── [./data-processed] ** untracked folder containing tidy data
├── [./data-spatial] ** untracked folder containing spatial data
│   ├── [./environmental-data] 
│   │   ├── [./chl]
│   │   └── [./sst]
│   ├── [./nz]
│   └── [./transects]
├── ./renv
├── ./scripts
│   ├── [README.md]
│   ├── 00a_renv-set-up.R
│   ├── 00b_create-sf-transects.R
│   ├── 01_data-wrangling_raw-to-standardised.R
│   ├── 02_data-wrangling_spatial-and-duration-filters.R
│   ├── 03-1_environmental-data_tdr.R
│   ├── 03-2_environmental-data_chl-and-sst.R
│   ├── 04_data-analysis_summary.R
│   ├── 05_data-analysis_ordination-modelling-rarefaction-curves.R
│   ├── source.R
│   └── study-area-map.R
├── ./results
│   ├── [./EDA] ** untracked, although some figures are presented in the manuscript
│   └── PDF and CSV files with results... won't name them all here...
├── ./rmd_tex
│   ├── [./ms_seabirds-northland-nz] ** untracked folder containing Rmd files {rticle}
│   └── ./TeX
│       ├── .tex
│       └── .bib
└── far-out_seabirds.Rproj
```

## How to run

**(Work in progress...)**

### Environment

`R` packages and their dependencies were captured using `{renv} v. 1.0.3` in the [lock.file](https://github.com/nwdaudt/far-out_seabirds/blob/main/renv.lock)

However, `{renv}` does *not* capture all the computing environment. The Operational System (OS) and `R` version is detailed below. We did not 'dockerised' our computing environment, so any system dependency you will need to deal with yourself (sorry!).

```shell
R version 4.2.0 (2022-04-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.6 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C               LC_TIME=en_NZ.UTF-8        LC_COLLATE=en_NZ.UTF-8
 [5] LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8    LC_PAPER=en_NZ.UTF-8       LC_NAME=C             
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C 
```

A heads up -- you will need `R` **4.2.0** and then make sure to `renv::restore()` to load the project environment with the same package versions as ours (if you are not familiar with `{renv}`, see [their website](https://rstudio.github.io/renv/articles/renv.html)). 

***
## Contributors

[Nicholas W. Daudt](https://github.com/nwdaudt)

Any bugs, suggestions, or enquires, please feel free to contact me or open an issue.

***
## Citation

This repository is under CC BY 4.0 license.

Please refer to the original paper if using any piece of this repository. 

Daudt NW; Guerra M; Brough T; Dwyer SL; Zaeschmar JR; Schofield MR; Smith RO; Bugoni L; Woehler EJ; Rayment WJ. *(in prep)*. Seasonality rather than oceanography drives assemblages of seabird species off northeast Aotearoa New Zealand.
