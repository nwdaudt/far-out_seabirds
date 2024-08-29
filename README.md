# Seasonality drives assemblages of seabird species off northeast Aotearoa New Zealand

**This is a work in progress. The README and files will be constantly being updated.**

In this study, we use a mix of descriptive statistics, diversity curves, and model-based multivariate statistics to describe assemblages of seabird species off Northland, northeast Aotearoa New Zealand.

Data were collected with the [Far Out Ocean Research Collective](https://www.farout.org.nz/). The fieldwork of this study is in the rohe (territory) of the Māori iwi (tribe) Ngāti Kuri, who shares the custody of the data. The raw data (probably) won't be open access; however, we hope to make some data available eventually.

***

## Project structure

Workspace is set as follows: **(Work in progress...)**

```shell
far-out_seabirds
├── README.md
├── [./data-raw] ** untracked folder containing raw data
│   ├── 
│   └── 
├── ./data-processed
│   ├── [./raw-tidy] ** untracked folder containing tidy 'raw' data
│   ├── 
│   ├── 
│   └── 
├── ./data-spatial
│   ├── [./environmental-data] ** untracked folder containing CHL .nc files
│   ├── ./nz
│   └── ./transects
├── ./renv
├── ./scripts
│   ├── README.md
│   ├── 
│   ├── 
│   ├── 
│   ├── 
│   ├── 
│   ├── 
│   ├── 
│   ├── 
│   └── 
├── ./results
│   └── PDF, CSV, and PNG files with results and EDA analyses
├── ./rmd_tex
│   │  └── 
│   ├── manuscript.Rmd
│   └── references.bib
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

Please refer to the original paper if using any piece of this repository (code and/or data). 

Daudt NW; Guerra M; Brough T; Dwyer SL; Zaeschmar JR; Schofield MR; Smith RO; Woehler EJ; Bugoni L; Rayment WJ. *(in prerp)*. Seasonality drives assemblages of seabird species off northeast Aotearoa New Zealand.
