# About

This the data and code repository for the multi-lab RRR of the Fischer et al 2003 (doi: [10.1177/2515245920903079](http://dx.doi.org/10.1177/2515245920903079)).

The raw data is located in the folder `data`.  You can re-download the data from the lab's OSF pages by using the script `DownloadAllData.r`. Do to this, you will need to replace the string `'token_code'` with your OSF token.

## How to use this repository

To ensure that the code in this repository compiles correctly there are two recommended options. Option 1 allows you to run all the code locally and it involves installing software on your computer. Option 2 allows the whole process to happen in the cloud.

### Option 1 (using Docker)

  Option 1 involves using **Docker** and the corresponding **Docker container**. To install Docker go to [www.docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop). There is a pre-build container located on [docker hub](https://hub.docker.com/layers/lcolling/fischer-docker/canonical/images/sha256-fc9b99f538bcdd36f48bf83d926ac757266238912fd544336fd4092767083457). To pull this container simply use the command `docker pull lcolling/fischer-docker:canonical`. This is the recommended method. Alternatively, if you wish to include additional packages, then you can build the container locally using the `Dockerfile` located in this repository. You can use the bash script `builddocker.sh` to do this.

Once you have pulled the docker image, you can run it using the command `rundocker.sh`. This will create a new `RStudio` instance accessible at `http://localhost:8787`. To login use the username `rstudio` and the password `fischer-rrr`. The R project file is located in `/home/rstudio/manuscript/manuscript.Rproj`.

### Option 2 (using Binder)

As an alternative to using a local install of Docker, you can use [**Binder**](https://mybinder.org). Using **Binder** you can load up an instance of **RStudio** based on the **lcolling/fischer-docker** Docker image (including a few additions to make it work with **Binder**). To do this, simply click the launch badge below. This will spin up an instance containing the computational environment and the content of the this repository. Once the instance is running, the appropriate **R Project (`.Rproj`)** file will open and **this file** will be opened in the **source pane**. For best results, I recommend using Google Chrome.

[![badge](https://img.shields.io/badge/launch-RStudio-579ACA.svg?logo=data:image/png;base64,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)](https://mybinder.org/v2/gh/ljcolling/fischer-binder/master?urlpath=git-pull?repo=https%3A%2F%2Fgithub.com%2Fljcolling%2Fattentional_snarc%26%3Burlpath=rstudio%26%3Bbranch=master)



## How to compile the manuscript

All pre-processing and data analysis steps have already been performed so the manuscript can be built simply my loading the file `manuscript_files/manuscript.Rmd` and clicking the `Knit` button at the top of the script editing window. This will build the manuscript at `manuscript.pdf`.

## Re-running the analysis

If you wish to re-run the analysis then you will need to perform the following steps. For easy access files referred to in this guide, it is recommended that you load this file (`README.md`) into an editor window in `RStudio`. Files can then be accessed by clicking on the links below (cmd + click [mac] / shift + click [windows]).

**Optional step**: Download the data using [DownloadAllData.r](helper_functions/DownloadAllData.r) (the data is already contained in this repository so this step is optional).

1. Run the pre-processing script at [Pre_processing.Rmd](processing/Pre_processing.Rmd).
2. Run the data processing script at [data_processing.Rmd](processing/data_processing.Rmd).
3. Run the data analysis script at [analysis.Rmd](processing/analysis.Rmd)
  - This script will produce a number of files that will need to analysed using the online meta-analysis tool located at [https://blakemcshane.shinyapps.io/mlmvmeta/](https://blakemcshane.shinyapps.io/mlmvmeta/). Instructions are provided by the analysis script.
4. Run the script to produce the statistics at [make_statistics.R](helper_functions/make_statistics.R)
5. Run the script to produce the figures at [make_figures.R](helper_functions/make_figures.R)
6. Compile the manuscript at [manuscript_files/manuscript.Rmd](manuscript_files/manuscript.Rmd)


If you'd like to update the citation counts of the original paper in the manuscript then just run the command python `get_citation_count.py > cites.txt` at the terminal

## Other Materials

### Experimental Task

The PsychToolBox Code for the main tasks are available in two repositories. The eye-tracker version is available [here](http://dx.doi.org/10.5281/zenodo.3406447) and the non eye-tracker version is available [here](http://dx.doi.org/10.5281/zenodo.3406449)

### Preregistration information

The main OSF page containing information about the pre-registration is available [here](http://dx.doi.org/10.17605/OSF.IO/HE5ZA)

### Participating labs

Information about the project and the participating labs is available [here](http://dx.doi.org/10.17605/OSF.IO/7ZYXJ) with more detail available [here](https://osf.io/7zyxj/wiki/home/)
