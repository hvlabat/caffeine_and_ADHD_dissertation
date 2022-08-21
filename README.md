# GitHub Repository for Caffeine & ADHD Study

#### By Hugo Labat

## Description

This is the repository where I processed and visualised the data from my MSc Cognitive Neuroscience and Human Neuroimaging dissertation.

The data used in this project comes from the Gorilla.sc experiment linked to this project, and from the ASRS and microsaccades data from Hampsey et al. (2019).

The Gorilla.sc data is from the consent form, Iowa Gambling Task (IGT), Rapid Visual information Processing test (RVP), Stop Signal Rection Task (SSRT), Test of Variables of Attention (TOVA), Caffeine Consumption (CaffCo) questionnaire, Adult ADHD Self-Report Scale (ASRS), and debrief form.

The information of use is from the four tests (RVP, TOVA, SSRT, IGT), which contained information on:

-   Reaction/response time (RVP, TOVA, SSRT)

-   Commission Errors (responding when they shouldn't respond) (RVP, TOVA, SSRT)

-   Omission Errors (not responding when they should) (RVP, TOVA, SSRT)

-   Riskiness (IGT)

## Contents

This project was coded using R. This project contains the following files and folders, listed as seen:

**".gitignore"** - Specifies untracked files that Git should ignore and those it should focus on.

**".Rhistory"** - Contains a history of the commands entered, generally unimportant.

**".Rprofile"** - USed to set options and environment variables, set-up by renv to maintain version control

**"data"** - Contains the **"raw"**" data used in this project, the source of which is outlined above. The **"refined"**" data folder contains the processed data from this project, described below, and a **"codebook.txt"** describing the nature of the each data vector.

**"scripts"** - Contains the scripts used to process the raw data into the outputs available in **"outputs"**.

**"outputs"** - Contains the output visualisations used in the project.

**"caffeine_and_ADHD_dissertation.Rproj"** - The R Project file of this project. Must be run if the scripts in **"docs"** are to work correctly.

**"README.md" / "README.html"** - This file, in different formats.

**"renv"** - A package-management system, used to ensure the isolation, portability, and reproducibility of this project. This is the R Studio updated version of packrat.

**"renv.lock"** - Contains data on the packages stored in renv. Open this in rstudio to view the packages.

## Instructions

If you would like to run the scripts, **please first run the R Project file titled "caffeine_and_ADHD_dissertation.Rproj"**

The scripts should ideally be run sequentially, in order of their numbers (i.e., "001_ppt_script.R" before "002_caffco_script.R").
Do not clear the data between scripts. The exception to this is the "microsacc_script.R", which should be run separately with environment cleared.

## References

Hampsey, E., Overton, P.G., Stafford, T. (2019). Microsaccade rate as a measure of drug response. 12(6). <https://doi.org/10.16910/jemr.12.6.12>

## Queries

If you have any queries, please direct them to [hugolabat\@doctors.org.uk](mailto:hugolabat@doctors.org.uk){.email}

Thanking you kindly.
