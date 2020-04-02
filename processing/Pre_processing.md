---
title: "R Notebook"
output: html_notebook
---


```r
# PREAMBLE 
# This section loads the required packages for this notebook. 
# All the required packages are already installed in the container

require(tidyverse)
require(ReplicationProjectTools)
require(glue)
require(furrr)


# Make some folders where I'll store stuff
system('mkdir generated_data')
system('mkdir generated_figures')
```


# Introduction

This *R Markdown* notebook contains all the code for doing the pre-processing steps for the Fischer et al RRR project.  

To use this notebook simply run each of the code chucks (either by running each chunk individually or by Run All from the run dropdown menu).

It only needs to be run once. All the data cleaning and meta-analyes analyes are done in a different notebook, so those notebooks can be used if you want to make any changes to the data cleaning and analysis parameters. 


## Downloading the data

```r
# Data already downlaoded in data folder so I will just check the data against my record of checksums 
# You can also re-download the data and check it against my record of checksums

dataFolder = "data/"
checksums = read_csv(file = "other_info/checksums.csv", col_types = cols(
  NAME = col_character(),
  md5sum = col_character()
))


datafile.list = list.files(path = dataFolder, recursive = T)
plan(multiprocess)
future_map_lgl(datafile.list, 
    function(x) unname((checksums %>% filter(NAME == x) %>% pull(md5sum)) == tools::md5sum(file.path(dataFolder,x)))) -> checkedsums
if(all(checkedsums) == TRUE){
  cat("I've checked all the files and there are no problems. Proceed to the next chuck.")
} else {
  cat("Something went wrong! Re-download the files")
}
```

```
## I've checked all the files and there are no problems. Proceed to the next chuck.
```

```r
rm("datafile.list") # tidying up!
rm("checkedsums") # tidying up!
rm("checksums") # tidying up!
```

## Tidying up

Some of the files are inconsistently named (for example, inconsistent cases), and their details are not recorded in a consistent manner in the file catalouge files (`FileList.csv`). So I'll do some tidying up and also double check all the files. 


```r
# This r script tidies up the file catalogue files
# This is necessary for several reasons
# 1. labs were not consistent in what they put in the Exclude Data col
# 2. labs were not consistent in what they put in the Secondary Data col
# when they were using the single-file version of the task
# 3. some labs had case errors in the their file names. For this reason the
# downloading script writes all the filenames in all lower case and this tidying script
# rewrites the catalogue file to match the all lower case of the filenames

# This writes out a file called TidyFileList.sav which can be used in any subsequent steps

# Tidy up the FileLists
ReplicationProjectTools::GetLabNames()

FileList = map(Nodes, function(x)
  readxl::read_xls(GetNodeFiles(dataFolder, x)[GetNodeFiles(dataFolder, x) %>% grepl(pattern = "filelist")]))
names(FileList) = Nodes
map(FileList, function(x)
  x %>% filter(!is.na(`Subject Code`)) %>% FixExcludeData()) -> FileList
```

```
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 4: Recoding 1/0 to TRUE/FALSE...**
## DONE!
## **TYPE 2: Recoding strings to boolen...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 3: Recoding YES/NO to TRUE/FALSE...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 5: Recoding missing values to FALSE...**
## DONE!
## **TYPE 1: Can use as is...**
## DONE!
## **TYPE 6: Recoding missing values to FALSE...**
## DONE!
```


```r
# make all the file names in the catalouge lower case
# this is to make things easier for case sensitive filesystems

map(names(FileList), function(x)
  FileList[[x]] %>% mutate(Node = x) %>% 
    mutate(`Main Data` = tolower(`Main Data`)) %>%
    mutate(`Secondary Data` = tolower(`Secondary Data`)) %>%
    mutate(`Finger Counting Data` = tolower(`Finger Counting Data`))) -> FileList
names(FileList) = Nodes

naToNa<-function(input){
  
  if(is.na(input)){
    return(input)
  }
  
  if(input == "n/a"){
    return(NA)
  } else if (input == "na"){
    return(NA)
  } else{
    
    return(input)
  }
}

for(node in Nodes){
  FileList[[node]]$`Secondary Data` <- unlist(map(FileList[[node]]$`Secondary Data`, naToNa))
  FileList[[node]]$`Subject Code` <- unlist(map(FileList[[node]]$`Subject Code`, naToNa))
  FileList[[node]]$`Main Data` <- unlist(map(FileList[[node]]$`Main Data`, naToNa))
  FileList[[node]]$`Finger Counting Data` <- unlist(map(FileList[[node]]$`Finger Counting Data`, naToNa))
}

rm("naToNa") # Tidying up!
rm("node") # tidying up!

# Quickly check the integrity of the all the matlab files
# At time of writing there were 4 files that are corrupt
# These files are corrupt on the OSF repo (that is, their canonical version is corrupt) 
CheckMat<-function (filename) 
{
  if (file.exists(filename) == FALSE) {
    cat("!!!!", filename, "IS MISSING!!!")
    cat("\n")
    return(filename)
  }
  header = rawToChar(readBin(filename, "raw", 19))
  if ((header == "MATLAB 5.0 MAT-file") == FALSE) {
    cat("Unexpected file format! Ignoring file!")
    cat("\n")
    return(filename)
  }
  return(NULL)
}

complete.files = list.files(path = file.path(dataFolder), pattern = "*.mat", recursive = T, full.names = T) # get all the mat files
FilesWithErrors = map(.f = function(x) CheckMat(x), .x = as.vector(complete.files)) %>% unlist() # get a list of the files with errors
```

```
## Unexpected file format! Ignoring file!
## Unexpected file format! Ignoring file!
## Unexpected file format! Ignoring file!
## Unexpected file format! Ignoring file!
## Unexpected file format! Ignoring file!
```

```r
rm("CheckMat") # tidying up!

cat("The following files had errors so they will be ignored\n\n",paste(FilesWithErrors,collapse = "\n"))
```

```
## The following files had errors so they will be ignored
## 
##  data//6ea38/lukavsky_9_170512_data.mat
## data//6ea38/lukavsky_9b_170512_data.mat
## data//kwg95/ansari_participant_001_data.mat
## data//kwg95/ansari_participant_007_data.mat
## data//xmyd9/ortiz_6_170524_secondary_data.mat
```

```r
# determine the node codes of the corrupt files
nodes.wCorrupt = substr(FilesWithErrors,str_locate(FilesWithErrors,pattern = "data//")[,2] + 1, str_locate(FilesWithErrors,pattern = "data//")[,2] + 5)

# determine the file names of the corrupt files
corrupt.names = substr(FilesWithErrors,str_locate(FilesWithErrors,pattern = nodes.wCorrupt)[,2]+2, str_length(FilesWithErrors))

# determine the file types of the corrupt files
corrupt.types = map_chr(1: length(nodes.wCorrupt),
    function(x) case_when(any(grepl(FileList[[nodes.wCorrupt[x]]]$`Main Data`, pattern = corrupt.names[x])) == TRUE ~ "Main Data", 
          any(grepl(FileList[[nodes.wCorrupt[x]]]$`Secondary Data`, pattern = corrupt.names[x])) == TRUE ~ "Secondary Data", 
          TRUE ~  "Finger Counting Data"))


FixCorruptFile<-function(node,name,type,FileList){
  # function for marking corrupt files are exclusions
  FileList[[node]] %<>% mutate(`Exclude Data` = ifelse(!! rlang::sym(type) == name,yes = TRUE, no = `Exclude Data`))
  FileList[[node]] %<>% mutate(`Reason` = ifelse(!! rlang::sym(type) == name,yes = "File error", no = `Reason`))
  
  return(FileList)
}

CorruptFiles = tibble(node = nodes.wCorrupt, 
                          name = corrupt.names,
                          type = corrupt.types)

rm(list = c("nodes.wCorrupt","corrupt.names","corrupt.types")) # tidying up!


for(i in 1 : dim(CorruptFiles)[1]){
  node = CorruptFiles$node[i]
  name = CorruptFiles$name[i]
  type = CorruptFiles$type[i]
  FileList = FixCorruptFile(node,name,type,FileList = FileList )  
}

rm(list = c("node","name","type","i")) # tidying up!
rm("complete.files") # tidying up!
rm("FixCorruptFile") # tidying up!

# This fixes up an issue with duplicated file names being used by some labs

for(i in 1 : length(FileList)){
  FileList[[names(FileList)[i]]][FileList[[names(FileList)[i]]] %>% pull("Subject Code") %>% duplicated() %>% which(),] %>% pull(`Subject Code`) -> DuplicatedCodes
  FileList[[names(FileList)[i]]] %>% mutate(dups = case_when(`Subject Code` %in% DuplicatedCodes ~ TRUE, TRUE ~ FALSE)) %>% mutate(`Exclude Data` = `Exclude Data` + dups) %>% mutate(`Exclude Data` = `Exclude Data` == 1) %>% select(-dups) -> FileList[[names(FileList)[i]]]
}

# now save the FileList so I can use it at a later date
save("FileList",file = "generated_data/TidyFileList.sav")
```

## Getting the data into R

The next step is to actually read the data into `R`. Because I wanted a pure `R` solution, I first convert the files from `Matlab` files into something more `R` friendly. These files are then saved into the `generated_data` folder. This set is very time consuming, because the `R` routines for reading matlab files are very slow, so this step only needs to be done once, and the generated data files can be used for all subsequent data cleaning and analysis steps. 


```r
# This generates .dat files from the .mat files
# These contain the data that will actually be analysed.
# This serves as an intermediate step. Because the .dat
# files can be read into r a lot quicker than working with the raw
# .mat files






source("matlab_file_helpers.R")

plan(multiprocess)
cat("Processing matlab files")
```

```
## Processing matlab files
```

```r
furrr::future_walk(Nodes, function(this.node) {

  
  this.FileList = FileList[[this.node]]
  
  this.FileList %<>% filter(`Exclude Data` == F) # Don't process excluded files!
  
  EyeTrackerData = new.env()
  ResponseData = new.env()
  SecondaryData = new.env()
  FingerData = new.env()
  HasEyeTracker = new.env()
  
  for(this.subject in 1: dim(this.FileList)[1]){
    
    
    hasSecondary = ifelse(grepl(".mat",this.FileList[["Secondary Data"]][this.subject]) == FALSE, 
                          yes = 1, no = 0)
  
    
    this.Code = this.FileList[["Subject Code"]][this.subject]
    cat("Reading the data from participant: ", this.Code,"\n")
    this.file = file.path(dataFolder,this.FileList[["Node"]][1],this.FileList[["Main Data"]][this.subject])
    
      
    this.Data = ReadFischerData(this.file,hasSecondary = hasSecondary)
    this.EyeTrackerStruct = this.Data$eyeTrackerData
    this.ResponseStruct = this.Data$responseData
    if(hasSecondary == 1){
      this.SecondaryStuct = this.Data$secondaryData
    } else {
      this.Sec = ReadFicherData.SecondaryOnly(file.path(dataFolder,this.FileList[["Node"]][1],this.FileList[["Secondary Data"]][this.subject]))
      this.SecondaryStuct = this.Sec$secondaryData
    }
    
    
    this.FingerData = R.matlab::readMat(file.path(dataFolder,this.FileList[["Node"]][1],this.FileList[["Finger Counting Data"]][this.subject]))
    
    # Add whether they guessed the purpose, because it lives with the finger counting data
    add_column(this.SecondaryStuct,guess = ExtractExperimentGuess(this.FingerData)) -> this.SecondaryStuct
    
    this.FingerStruct = ExtractFingerData(this.FingerData)
    
    # Now add node codes and subject codes to all the data
    # this.EyeTrackerStruct = eye tracking data
    # this.ResponseStruct = reaction time dta
    # this.SecondaryStuct = all the secondary measures (excluding the finger counting) and the debriefing question answer
    # this.FingerStruct = the finger counting data
    
    this.EyeTrackerStruct %<>% add_column(node = this.node, code = this.Code, .before = 1)
    this.ResponseStruct %<>% add_column(node = this.node, code = this.Code, .before = 1)
    this.SecondaryStuct %<>% add_column(node = this.node, code = this.Code, .before = 1)
    this.FingerStruct %<>% add_column(node = this.node, code = this.Code, .before = 1)
    
    # Package this subject's data for saving
    EyeTrackerData[[this.Code]] = this.EyeTrackerStruct
    ResponseData[[this.Code]] = this.ResponseStruct
    SecondaryData[[this.Code]] = this.SecondaryStuct
    FingerData[[this.Code]] = this.FingerStruct
    HasEyeTracker[[this.Code]] = (this.Data$hasEye == 1)
  }
  
  # Save this Nodes data
  ext = '.dat'
  save(file = glue('./generated_data/{this.node}-EyeTrackerData{ext}'), EyeTrackerData)
  save(file = glue('./generated_data/{this.node}-ResponseData{ext}'), ResponseData)
  save(file = glue('./generated_data/{this.node}-SecondaryData{ext}'), SecondaryData)
  save(file = glue('./generated_data/{this.node}-FingerData{ext}'), FingerData)
  save(file = glue('./generated_data/{this.node}-HasEyeTracker{ext}'), HasEyeTracker)
}, .progress = F)
```

```
## Reading the data from participant:  Corballis_01_170410 
## Reading the data from participant:  Corballis_02_170411 
## Reading the data from participant:  Corballis_03_170411 
## Reading the data from participant:  Corballis_04_170411 
## Reading the data from participant:  Corballis_05_170412 
## Reading the data from participant:  Corballis_06_170412 
## Reading the data from participant:  Corballis_07_170419 
## Reading the data from participant:  Corballis_08_170420 
## Reading the data from participant:  Corballis_09_170421 
## Reading the data from participant:  Corballis_10_170426 
## Reading the data from participant:  Corballis_11_170426 
## Reading the data from participant:  Corballis_12_170428 
## Reading the data from participant:  Corballis_13_170504 
## Reading the data from participant:  Corballis_14_170505 
## Reading the data from participant:  Corballis_15_170505 
## Reading the data from participant:  Corballis_16_170508 
## Reading the data from participant:  Corballis_17_170508 
## Reading the data from participant:  Corballis_18_170508 
## Reading the data from participant:  Corballis_19_170509 
## Reading the data from participant:  Corballis_20_170509 
## Reading the data from participant:  Corballis_21_170509 
## Reading the data from participant:  Corballis_22_170509 
## Reading the data from participant:  Corballis_24_170511 
## Reading the data from participant:  Corballis_25_170517 
## Reading the data from participant:  Corballis_26_170517 
## Reading the data from participant:  Corballis_27_170517 
## Reading the data from participant:  Corballis_28_170519 
## Reading the data from participant:  Corballis_29_170523 
## Reading the data from participant:  Corballis_30_170523 
## Reading the data from participant:  Corballis_31_170523 
## Reading the data from participant:  Corballis_32_170523 
## Reading the data from participant:  Corballis_33_170523 
## Reading the data from participant:  Corballis_34_170525 
## Reading the data from participant:  Corballis_35_170525 
## Reading the data from participant:  Corballis_36_170526 
## Reading the data from participant:  Corballis_37_170526 
## Reading the data from participant:  Corballis_38_170530 
## Reading the data from participant:  Corballis_39_170531 
## Reading the data from participant:  Corballis_40_170531 
## Reading the data from participant:  Corballis_41_170607 
## Reading the data from participant:  Corballis_42_170609 
## Reading the data from participant:  Corballis_43_170613 
## Reading the data from participant:  Corballis_44_170710 
## Reading the data from participant:  Corballis_45_170717 
## Reading the data from participant:  Corballis_46_170718 
## Reading the data from participant:  Corballis_47_170721 
## Reading the data from participant:  Corballis_48_170724 
## Reading the data from participant:  Corballis_49_170725 
## Reading the data from participant:  Corballis_50_170725 
## Reading the data from participant:  Corballis_52_170727 
## Reading the data from participant:  Corballis_53_170727 
## Reading the data from participant:  Corballis_54_170728 
## Reading the data from participant:  Corballis_55_170728 
## Reading the data from participant:  Corballis_56_170801 
## Reading the data from participant:  Corballis_57_170801 
## Reading the data from participant:  Corballis_58_170801 
## Reading the data from participant:  Corballis_59_170801 
## Reading the data from participant:  Corballis_60_170801 
## Reading the data from participant:  Corballis_61_170801 
## Reading the data from participant:  Corballis_62_170802 
## Reading the data from participant:  Corballis_63_170802 
## Reading the data from participant:  Corballis_64_170802 
## Reading the data from participant:  Corballis_65_170802 
## Reading the data from participant:  Corballis_66_170802 
## Reading the data from participant:  Corballis_67_170803 
## Reading the data from participant:  Corballis_68_170803 
## Reading the data from participant:  Chen_p0001_010517 
## Reading the data from participant:  Chen_p0002_010517 
## Reading the data from participant:  Chen_p0003_020517 
## Reading the data from participant:  Chen_p0004_020517 
## Reading the data from participant:  Chen_p0005_020517 
## Reading the data from participant:  Chen_p0006_030517 
## Reading the data from participant:  Chen_p0007_030517 
## Reading the data from participant:  Chen_p0008_030517 
## Reading the data from participant:  Chen_p0009_040517 
## Reading the data from participant:  Chen_p0010_040517 
## Reading the data from participant:  Chen_p0011_040517 
## Reading the data from participant:  Chen_p0012_040517 
## Reading the data from participant:  Chen_p0013_050517 
## Reading the data from participant:  Chen_p0014_050517 
## Reading the data from participant:  Chen_p0015_050517 
## Reading the data from participant:  Chen_p0016_050517 
## Reading the data from participant:  Chen_p0017_080517 
## Reading the data from participant:  Chen_p0018_080517 
## Reading the data from participant:  Chen_p0019_080517 
## Reading the data from participant:  Chen_p0020_080517 
## Reading the data from participant:  Chen_p0021_090517 
## Reading the data from participant:  Chen_p0022_090517 
## Reading the data from participant:  Chen_p0023_090517 
## Reading the data from participant:  Chen_p0024_090517 
## Reading the data from participant:  Chen_p0025_100517 
## Reading the data from participant:  Chen_p0026_100517 
## Reading the data from participant:  Chen_p0027_100517 
## Reading the data from participant:  Chen_p0028_110517 
## Reading the data from participant:  Chen_p0029_110517 
## Reading the data from participant:  Chen_p0031_120517 
## Reading the data from participant:  Chen_p0032_120517 
## Reading the data from participant:  Chen_p0033_130517 
## Reading the data from participant:  Chen_p0034_150517 
## Reading the data from participant:  Chen_p0035_150517 
## Reading the data from participant:  Chen_p0036_150517 
## Reading the data from participant:  Chen_p0037_150517 
## Reading the data from participant:  Chen_p0038_170517 
## Reading the data from participant:  Chen_p0039_170517 
## Reading the data from participant:  Chen_p0040_170517 
## Reading the data from participant:  Chen_p0041_170517 
## Reading the data from participant:  Chen_p0042_180517 
## Reading the data from participant:  Chen_p0043_180517 
## Reading the data from participant:  Chen_p0044_190517 
## Reading the data from participant:  Chen_p0045_190517 
## Reading the data from participant:  Chen_p0046_210517 
## Reading the data from participant:  Chen_p0047_220517 
## Reading the data from participant:  Chen_p0048_220517 
## Reading the data from participant:  Chen_p0049_220517 
## Reading the data from participant:  Chen_p0050_230517 
## Reading the data from participant:  Chen_p0051_230517 
## Reading the data from participant:  Chen_p0052_250517 
## Reading the data from participant:  Chen_p0053_250517 
## Reading the data from participant:  Chen_p0054_260517 
## Reading the data from participant:  Chen_p0055_260517 
## Reading the data from participant:  Chen_p0056_260517 
## Reading the data from participant:  Chen_p0057_290517 
## Reading the data from participant:  Chen_p0058_020617 
## Reading the data from participant:  Chen_p0059_060617 
## Reading the data from participant:  Chen_p0060_060617 
## Reading the data from participant:  Chen_p0061_070617 
## Reading the data from participant:  Chen_p0062_140617 
## Reading the data from participant:  9s 
## Reading the data from participant:  10s 
## Reading the data from participant:  11s 
## Reading the data from participant:  12s 
## Reading the data from participant:  13s 
## Reading the data from participant:  14s 
## Reading the data from participant:  15s 
## Reading the data from participant:  16s 
## Reading the data from participant:  17s 
## Reading the data from participant:  18s 
## Reading the data from participant:  19s 
## Reading the data from participant:  20s 
## Reading the data from participant:  21s 
## Reading the data from participant:  22s 
## Reading the data from participant:  23s 
## Reading the data from participant:  25s 
## Reading the data from participant:  26s 
## Reading the data from participant:  27s 
## Reading the data from participant:  28s 
## Reading the data from participant:  29s 
## Reading the data from participant:  30s 
## Reading the data from participant:  31s 
## Reading the data from participant:  32s 
## Reading the data from participant:  33s 
## Reading the data from participant:  34s 
## Reading the data from participant:  35s 
## Reading the data from participant:  36s 
## Reading the data from participant:  37s 
## Reading the data from participant:  38s 
## Reading the data from participant:  39s 
## Reading the data from participant:  40s 
## Reading the data from participant:  41s 
## Reading the data from participant:  42s 
## Reading the data from participant:  43s 
## Reading the data from participant:  44s 
## Reading the data from participant:  45s 
## Reading the data from participant:  46s 
## Reading the data from participant:  47s 
## Reading the data from participant:  48s 
## Reading the data from participant:  52s 
## Reading the data from participant:  53s 
## Reading the data from participant:  54s 
## Reading the data from participant:  55s 
## Reading the data from participant:  56s 
## Reading the data from participant:  57s 
## Reading the data from participant:  58s 
## Reading the data from participant:  59s 
## Reading the data from participant:  60s 
## Reading the data from participant:  61s 
## Reading the data from participant:  62s 
## Reading the data from participant:  63s 
## Reading the data from participant:  64s 
## Reading the data from participant:  66s 
## Reading the data from participant:  67s 
## Reading the data from participant:  69s 
## Reading the data from participant:  70s 
## Reading the data from participant:  71s 
## Reading the data from participant:  72s 
## Reading the data from participant:  73s 
## Reading the data from participant:  74s 
## Reading the data from participant:  75s 
## Reading the data from participant:  76s 
## Reading the data from participant:  77s 
## Reading the data from participant:  78s 
## Reading the data from participant:  79s 
## Reading the data from participant:  80s 
## Reading the data from participant:  81s 
## Reading the data from participant:  82s 
## Reading the data from participant:  83s 
## Reading the data from participant:  84s 
## Reading the data from participant:  85s 
## Reading the data from participant:  86s 
## Reading the data from participant:  87s 
## Reading the data from participant:  88s 
## Reading the data from participant:  89s 
## Reading the data from participant:  90s 
## Reading the data from participant:  91s 
## Reading the data from participant:  92s 
## Reading the data from participant:  93s 
## Reading the data from participant:  94s 
## Reading the data from participant:  95s 
## Reading the data from participant:  96s 
## Reading the data from participant:  97s 
## Reading the data from participant:  98s 
## Reading the data from participant:  99s 
## Reading the data from participant:  100s 
## Reading the data from participant:  101s 
## Reading the data from participant:  102s 
## Reading the data from participant:  103s 
## Reading the data from participant:  104s 
## Reading the data from participant:  105s 
## Reading the data from participant:  106s 
## Reading the data from participant:  107s 
## Reading the data from participant:  108s 
## Reading the data from participant:  109s 
## Reading the data from participant:  110s 
## Reading the data from participant:  111s 
## Reading the data from participant:  112s 
## Reading the data from participant:  113s 
## Reading the data from participant:  114s 
## Reading the data from participant:  115s 
## Reading the data from participant:  116s 
## Reading the data from participant:  117s 
## Reading the data from participant:  118s 
## Reading the data from participant:  119s 
## Reading the data from participant:  120s 
## Reading the data from participant:  121s 
## Reading the data from participant:  122s 
## Reading the data from participant:  123s 
## Reading the data from participant:  124s 
## Reading the data from participant:  125s 
## Reading the data from participant:  Treccani_S01_130417 
## Reading the data from participant:  Treccani_S02_130417 
## Reading the data from participant:  Treccani_S03_140417 
## Reading the data from participant:  Treccani_S04_050517 
## Reading the data from participant:  Treccani_S05_100517 
## Reading the data from participant:  Treccani_S06_100517 
## Reading the data from participant:  Treccani_S07_110517 
## Reading the data from participant:  Treccani_S08_150517 
## Reading the data from participant:  Treccani_S09_150517 
## Reading the data from participant:  Treccani_S10_160517 
## Reading the data from participant:  Treccani_S11_160517 
## Reading the data from participant:  Treccani_S12_190517 
## Reading the data from participant:  Treccani_S13_230517 
## Reading the data from participant:  Treccani_S14_240517 
## Reading the data from participant:  Treccani_S15_240517 
## Reading the data from participant:  Treccani_S16_250517 
## Reading the data from participant:  Treccani_S17_250517 
## Reading the data from participant:  Treccani_S18_260517 
## Reading the data from participant:  Treccani_S19_290517 
## Reading the data from participant:  Treccani_S20_290517 
## Reading the data from participant:  Treccani_S21_290517 
## Reading the data from participant:  Treccani_S22_300517 
## Reading the data from participant:  Treccani_S23_300517 
## Reading the data from participant:  Treccani_S24_310517 
## Reading the data from participant:  Treccani_S25_310517 
## Reading the data from participant:  Treccani_S26_310517 
## Reading the data from participant:  Treccani_S27_010617 
## Reading the data from participant:  Treccani_S28_050617 
## Reading the data from participant:  Treccani_S29_060617 
## Reading the data from participant:  Treccani_S30_090617 
## Reading the data from participant:  Treccani_S31_090617 
## Reading the data from participant:  Treccani_S32_120617 
## Reading the data from participant:  Treccani_S33_120617 
## Reading the data from participant:  Treccani_S34_120617 
## Reading the data from participant:  Treccani_S35_130617 
## Reading the data from participant:  Treccani_S36_130617 
## Reading the data from participant:  Treccani_S37_130617 
## Reading the data from participant:  Treccani_S38_140617 
## Reading the data from participant:  Treccani_S39_140617 
## Reading the data from participant:  Treccani_S40_150617 
## Reading the data from participant:  Treccani_S41_150617 
## Reading the data from participant:  Treccani_S42_160617 
## Reading the data from participant:  Treccani_S43_160617 
## Reading the data from participant:  Treccani_S44_199617 
## Reading the data from participant:  Treccani_S45_190617 
## Reading the data from participant:  Treccani_S46_200617 
## Reading the data from participant:  Treccani_S47_210617 
## Reading the data from participant:  Treccani_S48_210617 
## Reading the data from participant:  Treccani_S49_220617 
## Reading the data from participant:  Treccani_S50_220617 
## Reading the data from participant:  Treccani_S51_230617 
## Reading the data from participant:  Treccani_S52_270617 
## Reading the data from participant:  Treccani_S53_060717 
## Reading the data from participant:  Treccani_S54_060717 
## Reading the data from participant:  Treccani_S55_110717 
## Reading the data from participant:  Treccani_S56_110717 
## Reading the data from participant:  Treccani_S57_180717 
## Reading the data from participant:  Treccani_S58_180717 
## Reading the data from participant:  Treccani_S59_190717 
## Reading the data from participant:  Treccani_S60_200717 
## Reading the data from participant:  Ortiz_1_171027 
## Reading the data from participant:  Ortiz_2_170522 
## Reading the data from participant:  Ortiz_3_170522 
## Reading the data from participant:  Ortiz_4_170522 
## Reading the data from participant:  Ortiz_5_170524 
## Reading the data from participant:  Ortiz_7_170524 
## Reading the data from participant:  Ortiz_8_171107 
## Reading the data from participant:  Ortiz_9_170525 
## Reading the data from participant:  Ortiz_10_171107 
## Reading the data from participant:  Ortiz_11_170526 
## Reading the data from participant:  Ortiz_12_170925 
## Reading the data from participant:  Ortiz_13_170926 
## Reading the data from participant:  Ortiz_14_170926 
## Reading the data from participant:  Ortiz_15_170927 
## Reading the data from participant:  Ortiz_16_170927 
## Reading the data from participant:  Ortiz_17_170928 
## Reading the data from participant:  Ortiz_18_170927 
## Reading the data from participant:  Ortiz_19_170929 
## Reading the data from participant:  Ortiz_20_171004 
## Reading the data from participant:  Ortiz_21_171004 
## Reading the data from participant:  Ortiz_22_171005 
## Reading the data from participant:  Ortiz_23_171010 
## Reading the data from participant:  Ortiz_24_171010 
## Reading the data from participant:  Ortiz_25_171010 
## Reading the data from participant:  Ortiz_26_171011 
## Reading the data from participant:  Ortiz_27_171017 
## Reading the data from participant:  Ortiz_28_171018 
## Reading the data from participant:  Ortiz_30_171025 
## Reading the data from participant:  Ortiz_1_171010 
## Reading the data from participant:  Ortiz_2_171016 
## Reading the data from participant:  Ortiz_3_171017 
## Reading the data from participant:  Ortiz_4_171017 
## Reading the data from participant:  Ortiz_5_171017 
## Reading the data from participant:  Ortiz_6_171017 
## Reading the data from participant:  Ortiz_7_171017 
## Reading the data from participant:  Ortiz_8_171017 
## Reading the data from participant:  Ortiz_9_171017 
## Reading the data from participant:  Ortiz_10_171017 
## Reading the data from participant:  Ortiz_11_171019 
## Reading the data from participant:  Ortiz_12_171017 
## Reading the data from participant:  Ortiz_13_171017 
## Reading the data from participant:  Ortiz_14_171010 
## Reading the data from participant:  Ortiz_15_171018 
## Reading the data from participant:  Ortiz_16_171018 
## Reading the data from participant:  Ortiz_17_171018 
## Reading the data from participant:  Ortiz_18_171018 
## Reading the data from participant:  Ortiz_19_171018 
## Reading the data from participant:  Ortiz_20_171018 
## Reading the data from participant:  Ortiz_21_171018 
## Reading the data from participant:  Ortiz_22_171018 
## Reading the data from participant:  Ortiz_23_171018 
## Reading the data from participant:  Ortiz_24_171018 
## Reading the data from participant:  Ortiz_25_171018 
## Reading the data from participant:  Ortiz_26_171018 
## Reading the data from participant:  Ortiz_27_171018 
## Reading the data from participant:  Ortiz_28_171019 
## Reading the data from participant:  Ortiz_30_171019 
## Reading the data from participant:  lukavsky_10_170522 
## Reading the data from participant:  lukavsky_11_170522 
## Reading the data from participant:  lukavsky_12_170522 
## Reading the data from participant:  lukavsky_13_170523 
## Reading the data from participant:  lukavsky_14_170523 
## Reading the data from participant:  lukavsky_15_170523 
## Reading the data from participant:  lukavsky_17_170524 
## Reading the data from participant:  lukavsky_18_170524 
## Reading the data from participant:  lukavsky_19_170524 
## Reading the data from participant:  lukavsky_20_170524 
## Reading the data from participant:  lukavsky_23_170525 
## Reading the data from participant:  lukavsky_25_170525 
## Reading the data from participant:  lukavsky_26_170525 
## Reading the data from participant:  lukavsky_27_170529 
## Reading the data from participant:  lukavsky_28_170529 
## Reading the data from participant:  lukavsky_29_170529 
## Reading the data from participant:  lukavsky_30_170529 
## Reading the data from participant:  lukavsky_31_170529 
## Reading the data from participant:  lukavsky_32_170530 
## Reading the data from participant:  lukavsky_34_170530 
## Reading the data from participant:  lukavsky_36_170531 
## Reading the data from participant:  lukavsky_38_170602 
## Reading the data from participant:  lukavsky_39_170602 
## Reading the data from participant:  lukavsky_42_170606 
## Reading the data from participant:  lukavsky_43_170606 
## Reading the data from participant:  lukavsky_44_170606 
## Reading the data from participant:  lukavsky_45_170607 
## Reading the data from participant:  lukavsky_46_170608 
## Reading the data from participant:  lukavsky_47_170608 
## Reading the data from participant:  lukavsky_48_170614 
## Reading the data from participant:  lukavsky_51_170615 
## Reading the data from participant:  lukavsky_52_170615 
## Reading the data from participant:  lukavsky_53_170616 
## Reading the data from participant:  lukavsky_54_170616 
## Reading the data from participant:  lukavsky_55_170616 
## Reading the data from participant:  lukavsky_57_170619 
## Reading the data from participant:  lukavsky_58_170619 
## Reading the data from participant:  lukavsky_59_170619 
## Reading the data from participant:  lukavsky_61_170623 
## Reading the data from participant:  lukavsky_62_170623 
## Reading the data from participant:  lukavsky_63_170623 
## Reading the data from participant:  lukavsky_65_170630 
## Reading the data from participant:  lukavsky_66_170704 
## Reading the data from participant:  lukavsky_68_170710 
## Reading the data from participant:  lukavsky_70_170712 
## Reading the data from participant:  lukavsky_71_170713 
## Reading the data from participant:  lukavsky_72_170726 
## Reading the data from participant:  lukavsky_73_170726 
## Reading the data from participant:  lukavsky_74_170726 
## Reading the data from participant:  lukavsky_75_170727 
## Reading the data from participant:  lukavsky_76_170727 
## Reading the data from participant:  lukavsky_77_170727 
## Reading the data from participant:  lukavsky_78_170731 
## Reading the data from participant:  lukavsky_79_170801 
## Reading the data from participant:  lukavsky_80_170801 
## Reading the data from participant:  lukavsky_81_170802 
## Reading the data from participant:  lukavsky_82_170802 
## Reading the data from participant:  lukavsky_83_170802 
## Reading the data from participant:  lukavsky_84_170803 
## Reading the data from participant:  lukavsky_85_170818 
## Reading the data from participant:  lukavsky_86_170818 
## Reading the data from participant:  Lindemann_Participant_1 
## Reading the data from participant:  Lindemann_Participant_2 
## Reading the data from participant:  Lindemann_Participant_3 
## Reading the data from participant:  Lindemann_Participant_4 
## Reading the data from participant:  Lindemann_Participant_5 
## Reading the data from participant:  Lindemann_Participant_6 
## Reading the data from participant:  Lindemann_Participant_7 
## Reading the data from participant:  Lindemann_Participant_8 
## Reading the data from participant:  Lindemann_Participant_9 
## Reading the data from participant:  Lindemann_Participant_10 
## Reading the data from participant:  Lindemann_Participant_11 
## Reading the data from participant:  Lindemann_Participant_12 
## Reading the data from participant:  Lindemann_Participant_13 
## Reading the data from participant:  Lindemann_Participant_14 
## Reading the data from participant:  Lindemann_Participant_15 
## Reading the data from participant:  Lindemann_Participant_16 
## Reading the data from participant:  Lindemann_Participant_17 
## Reading the data from participant:  Lindemann_Participant_18 
## Reading the data from participant:  Lindemann_Participant_19 
## Reading the data from participant:  Lindemann_Participant_20 
## Reading the data from participant:  Lindemann_Participant_21 
## Reading the data from participant:  Lindemann_Participant_22 
## Reading the data from participant:  Lindemann_Participant_23 
## Reading the data from participant:  Lindemann_Participant_24 
## Reading the data from participant:  Lindemann_Participant_25 
## Reading the data from participant:  Lindemann_Participant_26 
## Reading the data from participant:  Lindemann_Participant_27 
## Reading the data from participant:  Lindemann_Participant_28 
## Reading the data from participant:  Lindemann_Participant_29 
## Reading the data from participant:  Lindemann_Participant_30 
## Reading the data from participant:  Lindemann_Participant_31 
## Reading the data from participant:  Lindemann_Participant_32 
## Reading the data from participant:  Lindemann_Participant_33 
## Reading the data from participant:  Lindemann_Participant_34 
## Reading the data from participant:  Lindemann_Participant_35 
## Reading the data from participant:  Lindemann_Participant_36 
## Reading the data from participant:  Lindemann_Participant_37 
## Reading the data from participant:  Lindemann_Participant_38 
## Reading the data from participant:  Lindemann_Participant_39 
## Reading the data from participant:  Lindemann_Participant_40 
## Reading the data from participant:  Lindemann_Participant_41 
## Reading the data from participant:  Lindemann_Participant_42 
## Reading the data from participant:  Lindemann_Participant_43 
## Reading the data from participant:  Lindemann_Participant_44 
## Reading the data from participant:  Lindemann_Participant_45 
## Reading the data from participant:  Lindemann_Participant_46 
## Reading the data from participant:  Lindemann_Participant_47 
## Reading the data from participant:  Lindemann_Participant_48 
## Reading the data from participant:  Lindemann_Participant_49 
## Reading the data from participant:  Lindemann_Participant_50 
## Reading the data from participant:  cipora_01_290517 
## Reading the data from participant:  cipora_02_310517 
## Reading the data from participant:  cipora_03_010617 
## Reading the data from participant:  cipora_04_010617 
## Reading the data from participant:  cipora_05_140617 
## Reading the data from participant:  cipora_06_210617 
## Reading the data from participant:  cipora_07_210617 
## Reading the data from participant:  cipora_08_210617 
## Reading the data from participant:  cipora_09_210617 
## Reading the data from participant:  cipora_10_210617 
## Reading the data from participant:  cipora_11_210617 
## Reading the data from participant:  cipora_12_210617 
## Reading the data from participant:  cipora_13_220617 
## Reading the data from participant:  cipora_14_220617 
## Reading the data from participant:  cipora_15_220617 
## Reading the data from participant:  cipora_16_240617 
## Reading the data from participant:  cipora_17_240617 
## Reading the data from participant:  cipora_18_240617 
## Reading the data from participant:  cipora_19_240617 
## Reading the data from participant:  cipora_20_240617 
## Reading the data from participant:  cipora_22_260617 
## Reading the data from participant:  cipora_23_260617 
## Reading the data from participant:  cipora_24_280617 
## Reading the data from participant:  cipora_25_280617 
## Reading the data from participant:  cipora_26_280617 
## Reading the data from participant:  cipora_27_280617 
## Reading the data from participant:  cipora_28_280617 
## Reading the data from participant:  cipora_29_280617 
## Reading the data from participant:  cipora_30_290617 
## Reading the data from participant:  cipora_31_290617 
## Reading the data from participant:  cipora_32_290617 
## Reading the data from participant:  cipora_33_290617 
## Reading the data from participant:  cipora_34_010717 
## Reading the data from participant:  cipora_35_010717 
## Reading the data from participant:  cipora_36_010717 
## Reading the data from participant:  cipora_37_010717 
## Reading the data from participant:  cipora_38_010717 
## Reading the data from participant:  cipora_39_010717 
## Reading the data from participant:  cipora_41_030717 
## Reading the data from participant:  cipora_42_050717 
## Reading the data from participant:  cipora_44_050717 
## Reading the data from participant:  cipora_45_050717 
## Reading the data from participant:  cipora_46_050717 
## Reading the data from participant:  cipora_47_060717 
## Reading the data from participant:  cipora_48_060717 
## Reading the data from participant:  cipora_49_060717 
## Reading the data from participant:  cipora_50_060717 
## Reading the data from participant:  cipora_51060717 
## Reading the data from participant:  cipora_52_080717 
## Reading the data from participant:  cipora_53_080717 
## Reading the data from participant:  cipora_54_080717 
## Reading the data from participant:  cipora_55_080717 
## Reading the data from participant:  cipora_56080717 
## Reading the data from participant:  cipora_57_100717 
## Reading the data from participant:  cipora_58_100717 
## Reading the data from participant:  cipora_60_120717 
## Reading the data from participant:  cipora_61_120717 
## Reading the data from participant:  cipora_62_120717 
## Reading the data from participant:  cipora_63_120717 
## Reading the data from participant:  cipora_64_120717 
## Reading the data from participant:  cipora_65_130717 
## Reading the data from participant:  cipora_66_130717 
## Reading the data from participant:  cipora_67_130717 
## Reading the data from participant:  cipora_68_130717 
## Reading the data from participant:  cipora_69_130717 
## Reading the data from participant:  cipora_70_130717 
## Reading the data from participant:  cipora_71_150717 
## Reading the data from participant:  cipora_72_150717 
## Reading the data from participant:  cipora_73_170717 
## Reading the data from participant:  cipora_74_170717 
## Reading the data from participant:  cipora_74_190717 
## Reading the data from participant:  cipora_76_190717 
## Reading the data from participant:  cipora_77_190717 
## Reading the data from participant:  cipora_78_190717 
## Reading the data from participant:  cipora_79_220717 
## Reading the data from participant:  cipora_80_240717 
## Reading the data from participant:  cipora_81_260717 
## Reading the data from participant:  cipora_82_260717 
## Reading the data from participant:  cipora_83_260717 
## Reading the data from participant:  cipora_84_270717 
## Reading the data from participant:  cipora_85_270717 
## Reading the data from participant:  cipora_86_310717 
## Reading the data from participant:  cipora_87_310717 
## Reading the data from participant:  cipora_88_020817 
## Reading the data from participant:  cipora_89_020817 
## Reading the data from participant:  cipora_90_020817 
## Reading the data from participant:  cipora_91_020817 
## Reading the data from participant:  cipora_92_020817 
## Reading the data from participant:  cipora_93_020817 
## Reading the data from participant:  cipora_94_020817 
## Reading the data from participant:  cipora_95_030817 
## Reading the data from participant:  cipora_96_030817 
## Reading the data from participant:  Mieth_S2_180417 
## Reading the data from participant:  Mieth_S3_180417 
## Reading the data from participant:  Mieth_S4_180417 
## Reading the data from participant:  Mieth_S5_180417 
## Reading the data from participant:  Mieth_S6_180417 
## Reading the data from participant:  Mieth_S7_180417 
## Reading the data from participant:  Mieth_S8_190417 
## Reading the data from participant:  Mieth_S9_190417 
## Reading the data from participant:  Mieth_S10_190417 
## Reading the data from participant:  Mieth_S11_190417 
## Reading the data from participant:  Mieth_S12_190417 
## Reading the data from participant:  Mieth_S13_190417 
## Reading the data from participant:  Mieth_S14_190417 
## Reading the data from participant:  Mieth_S15_200417 
## Reading the data from participant:  Mieth_S16_200417 
## Reading the data from participant:  Mieth_S17_200417 
## Reading the data from participant:  Mieth_S18_200417 
## Reading the data from participant:  Mieth_S19_200417 
## Reading the data from participant:  Mieth_S20_200417 
## Reading the data from participant:  Mieth_S21_200417 
## Reading the data from participant:  Mieth_S22_200417 
## Reading the data from participant:  Mieth_S23_180885 
## Reading the data from participant:  Mieth_S24_210417 
## Reading the data from participant:  Mieth_S25_210417 
## Reading the data from participant:  Mieth_S26_210417 
## Reading the data from participant:  Mieth_S27_210417 
## Reading the data from participant:  Mieth_S28_210417 
## Reading the data from participant:  Mieth_S29_210417 
## Reading the data from participant:  Mieth_S30_210417 
## Reading the data from participant:  Mieth_S31_210417 
## Reading the data from participant:  Mieth_S32_240417 
## Reading the data from participant:  Mieth_S34_240417 
## Reading the data from participant:  Mieth_S35_250417 
## Reading the data from participant:  Mieth_S36_250417 
## Reading the data from participant:  Mieth_S37_250417 
## Reading the data from participant:  Mieth_S38_250417 
## Reading the data from participant:  Mieth_S39_260417 
## Reading the data from participant:  Mieth_S40_260417 
## Reading the data from participant:  Mieth_S41_260417 
## Reading the data from participant:  Mieth_S42_260417 
## Reading the data from participant:  Mieth_S43_260417 
## Reading the data from participant:  Mieth_S44_260417 
## Reading the data from participant:  Mieth_S45_260417 
## Reading the data from participant:  Mieth_S46_260417 
## Reading the data from participant:  Mieth_S47_260417 
## Reading the data from participant:  Mieth_S48_260417 
## Reading the data from participant:  Mieth_S49_270417 
## Reading the data from participant:  Mieth_S50_270417 
## Reading the data from participant:  Mieth_S51_270417 
## Reading the data from participant:  Mieth_S52_270417 
## Reading the data from participant:  Mieth_S53_270417 
## Reading the data from participant:  Mieth_S54_280417 
## Reading the data from participant:  Mieth_S55_280417 
## Reading the data from participant:  Mieth_S56_280417 
## Reading the data from participant:  Mieth_S57_280417 
## Reading the data from participant:  Mieth_S58_280417 
## Reading the data from participant:  Mieth_S59_280417 
## Reading the data from participant:  Mieth_S60_280417 
## Reading the data from participant:  Mieth_S61_280417 
## Reading the data from participant:  Mieth_S62_020517 
## Reading the data from participant:  Mieth_S63_020517 
## Reading the data from participant:  Mieth_S64_020517 
## Reading the data from participant:  Mieth_S65_020517 
## Reading the data from participant:  Mieth_S66_020517 
## Reading the data from participant:  Mieth_S67_020517 
## Reading the data from participant:  Mieth_S68_030517 
## Reading the data from participant:  Mieth_S69_030517 
## Reading the data from participant:  Mieth_S70_030517 
## Reading the data from participant:  Mieth_S71_030517 
## Reading the data from participant:  Mieth_S72_030517 
## Reading the data from participant:  Mieth_S73_030517 
## Reading the data from participant:  Mieth_S74_030517 
## Reading the data from participant:  Mieth_S75_030517 
## Reading the data from participant:  Mieth_S76_030517 
## Reading the data from participant:  Mieth_S77_030517 
## Reading the data from participant:  Mieth_S78_030517 
## Reading the data from participant:  Mieth_S79_040517 
## Reading the data from participant:  Mieth_S80_040517 
## Reading the data from participant:  Mieth_S81_040517 
## Reading the data from participant:  Mieth_S82_040517 
## Reading the data from participant:  Mieth_S83_040517 
## Reading the data from participant:  Mieth_S84_050517 
## Reading the data from participant:  Mieth_S85_050517 
## Reading the data from participant:  Mieth_S86_080517 
## Reading the data from participant:  Mieth_S87_080517 
## Reading the data from participant:  Mieth_S88_080517 
## Reading the data from participant:  Mieth_S89_080517 
## Reading the data from participant:  Mieth_S90_080517 
## Reading the data from participant:  Mieth_S91_080517 
## Reading the data from participant:  Mieth_S92_080517 
## Reading the data from participant:  Mieth_S93_080517 
## Reading the data from participant:  Mieth_S94_080517 
## Reading the data from participant:  Mieth_S95_090517 
## Reading the data from participant:  Mieth_S96_090517 
## Reading the data from participant:  Mieth_S97_090517 
## Reading the data from participant:  Mieth_S98_090517 
## Reading the data from participant:  Mieth_S99_090517 
## Reading the data from participant:  Mieth_S100_090517 
## Reading the data from participant:  Mieth_S101_090517 
## Reading the data from participant:  Mieth_S102_090517 
## Reading the data from participant:  Mieth_S103_100517 
## Reading the data from participant:  Mieth_S104_100517 
## Reading the data from participant:  Mieth_S105_100517 
## Reading the data from participant:  Mieth_S106_100517 
## Reading the data from participant:  Mieth_S107_100517 
## Reading the data from participant:  Mieth_S108_100517 
## Reading the data from participant:  Mieth_S109_100517 
## Reading the data from participant:  Mieth_S110_100517 
## Reading the data from participant:  Mieth_S111_100517 
## Reading the data from participant:  Mieth_S112_100517 
## Reading the data from participant:  Mieth_S113_110517 
## Reading the data from participant:  Mieth_S114_110517 
## Reading the data from participant:  Mieth_S115_110517 
## Reading the data from participant:  Mieth_S116_110517 
## Reading the data from participant:  Mieth_S117_110517 
## Reading the data from participant:  Mieth_S118_110517 
## Reading the data from participant:  Mieth_S119_110517 
## Reading the data from participant:  Mieth_S120_110517 
## Reading the data from participant:  Mieth_S121_120517 
## Reading the data from participant:  Mieth_S122_120517 
## Reading the data from participant:  Mieth_S123_120517 
## Reading the data from participant:  Mieth_S124_120517 
## Reading the data from participant:  2 
## Reading the data from participant:  3 
## Reading the data from participant:  4 
## Reading the data from participant:  5 
## Reading the data from participant:  Hancock_Participant6_11042017 
## Reading the data from participant:  Hancock_Participant_7_11042017 
## Reading the data from participant:  Hancock_Participant_11042017 
## Reading the data from participant:  Hancock_Participant9_11042017 
## Reading the data from participant:  Hancock_Participant10_12042017 
## Reading the data from participant:  Hancock_Participant11_12042017 
## Reading the data from participant:  Hancock_Participant12_12042017 
## Reading the data from participant:  Hancock_Participant13_12042017 
## Reading the data from participant:  Hancock_Participant14_13042017 
## Reading the data from participant:  Hancock_Participant15_14042017 
## Reading the data from participant:  Hancock_Participant16_14042017 
## Reading the data from participant:  Hancock_Participant17_17042017 
## Reading the data from participant:  Hancock_Participant18_18042017 
## Reading the data from participant:  Hancock_Participant19_18042017 
## Reading the data from participant:  Hancock_Participant20_18042017 
## Reading the data from participant:  Hancock_Participant21_19042017 
## Reading the data from participant:  Hancock_Participant22_19042017 
## Reading the data from participant:  Hancock_Participant23_19042017 
## Reading the data from participant:  Hancock_Participant24_19042017 
## Reading the data from participant:  Hancock_Participant25_20042017 
## Reading the data from participant:  Hancock_Participant26_20042017 
## Reading the data from participant:  Hancock_Participant27_20042017 
## Reading the data from participant:  Hancock_Participant28_20042017 
## Reading the data from participant:  Hancock_Participant29_21042017 
## Reading the data from participant:  Hancock_Participant30_24042017 
## Reading the data from participant:  Hancock_Participant31_24042017 
## Reading the data from participant:  Hancock_Participant32_24042017 
## Reading the data from participant:  Hancock_Participant38_03052017 
## Reading the data from participant:  Hancock_Participant39_04052017 
## Reading the data from participant:  Hancock_Participant40_04052017 
## Reading the data from participant:  Hancock_Participant41_04052017 
## Reading the data from participant:  Hancock_Participant42_04052017 
## Reading the data from participant:  Hancock_Participant43_05052017 
## Reading the data from participant:  Hancock_Participant44_05052017 
## Reading the data from participant:  Hancock_Participant45_05052017 
## Reading the data from participant:  Hancock_Participant46_05052017 
## Reading the data from participant:  Hancock_Participant47_05052017 
## Reading the data from participant:  Hancock_Participant48_08052017 
## Reading the data from participant:  Hancock_Participant49_08052017 
## Reading the data from participant:  Hancock_Participant50_08052017 
## Reading the data from participant:  Hancock_Participant51_08052017 
## Reading the data from participant:  Hancock_Participant52_08052017 
## Reading the data from participant:  Hancock_Participant53_09052017 
## Reading the data from participant:  Hancock_Participant54_09052017 
## Reading the data from participant:  Hancock_Participant55_09052017 
## Reading the data from participant:  Hancock_Participant56_10052017 
## Reading the data from participant:  Hancock_Participant57_10052017 
## Reading the data from participant:  Hancock_Participant58_10052017 
## Reading the data from participant:  Hancock_Participant59_10052017 
## Reading the data from participant:  Hancock_Participant60_11052017 
## Reading the data from participant:  Hancock_Participant61_11052017 
## Reading the data from participant:  Hancock_Participant62_11052017 
## Reading the data from participant:  Hancock_Participant63_11052017 
## Reading the data from participant:  Hancock_Participant64_11052017 
## Reading the data from participant:  Hancock_Participant65_12052017 
## Reading the data from participant:  Hancock_Participant66_12052017 
## Reading the data from participant:  Hancock_Participant67_12052017 
## Reading the data from participant:  Hubbard_01_170411 
## Reading the data from participant:  Hubbard_03_170411 
## Reading the data from participant:  Hubbard_04_170411 
## Reading the data from participant:  Hubbard_05_170411 
## Reading the data from participant:  Hubbard_06_170411 
## Reading the data from participant:  Hubbard_07_170411 
## Reading the data from participant:  Hubbard_08_170412 
## Reading the data from participant:  Hubbard_09_170412 
## Reading the data from participant:  Hubbard_10_170412 
## Reading the data from participant:  Hubbard_11_170412 
## Reading the data from participant:  Hubbard_12_170412 
## Reading the data from participant:  Hubbard_13_170412 
## Reading the data from participant:  Hubbard_14_170413 
## Reading the data from participant:  Hubbard_15_170413 
## Reading the data from participant:  Hubbard_16_170413 
## Reading the data from participant:  Hubbard_17_170413 
## Reading the data from participant:  Hubbard_18_170413 
## Reading the data from participant:  Hubbard_19_170413 
## Reading the data from participant:  Hubbard_20_170413 
## Reading the data from participant:  Hubbard_21_170413 
## Reading the data from participant:  Hubbard_22_170413 
## Reading the data from participant:  Hubbard_23_170414 
## Reading the data from participant:  Hubbard_24_170414 
## Reading the data from participant:  Hubbard_25_170414 
## Reading the data from participant:  Hubbard_26_170414 
## Reading the data from participant:  Hubbard_27_170417 
## Reading the data from participant:  Hubbard_28_170417 
## Reading the data from participant:  Hubbard_29_170418 
## Reading the data from participant:  Hubbard_30_170418 
## Reading the data from participant:  Hubbard_32_170418 
## Reading the data from participant:  Hubbard_33_170419 
## Reading the data from participant:  Hubbard_34_170419 
## Reading the data from participant:  Hubbard_35_170419 
## Reading the data from participant:  Hubbard_36_170419 
## Reading the data from participant:  Hubbard_37_170420 
## Reading the data from participant:  Hubbard_38_170420 
## Reading the data from participant:  Hubbard_39_170420 
## Reading the data from participant:  Hubbard_40_170420 
## Reading the data from participant:  Hubbard_41_170420 
## Reading the data from participant:  Hubbard_42_170420 
## Reading the data from participant:  Hubbard_43_170420 
## Reading the data from participant:  Hubbard_44_170420 
## Reading the data from participant:  Hubbard_45_170420 
## Reading the data from participant:  Hubbard_47_170421 
## Reading the data from participant:  Hubbard_48_170421 
## Reading the data from participant:  Hubbard_50_170421 
## Reading the data from participant:  Hubbard_51_170425 
## Reading the data from participant:  Hubbard_52_170425 
## Reading the data from participant:  Hubbard_53_170426 
## Reading the data from participant:  Hubbard_54_170426 
## Reading the data from participant:  Hubbard_55_170426 
## Reading the data from participant:  Hubbard_56_170426 
## Reading the data from participant:  Hubbard_57_170426 
## Reading the data from participant:  Hubbard_58_170426 
## Reading the data from participant:  Hubbard_59_170427 
## Reading the data from participant:  Hubbard_60_170427 
## Reading the data from participant:  Hubbard_61_170427 
## Reading the data from participant:  Hubbard_62_170427 
## Reading the data from participant:  Hubbard_63_170427 
## Reading the data from participant:  Hubbard_64_170427 
## Reading the data from participant:  Hubbard_65_170428 
## Reading the data from participant:  Hubbard_66_170428 
## Reading the data from participant:  Hubbard_67_170428 
## Reading the data from participant:  Hubbard_68_170428 
## Reading the data from participant:  Hubbard_69_170501 
## Reading the data from participant:  Hubbard_70_170501 
## Reading the data from participant:  Hubbard_71_170501 
## Reading the data from participant:  Hubbard_72_170501 
## Reading the data from participant:  Hubbard_73_170502 
## Reading the data from participant:  Hubbard_74_170502 
## Reading the data from participant:  Holmes_S01_170327_3pm 
## Reading the data from participant:  Holmes_S02_170327_4pm 
## Reading the data from participant:  Holmes_S03_170327_5pm 
## Reading the data from participant:  Holmes_S05_170328_3pm 
## Reading the data from participant:  Holmes_S06_170328_4pm 
## Reading the data from participant:  Holmes_S07_170328_5pm 
## Reading the data from participant:  Holmes_S08_170329_3pm 
## Reading the data from participant:  Holmes_S09_170329_4pm 
## Reading the data from participant:  Holmes_S10_170329_5pm 
## Reading the data from participant:  Holmes_S11_170330_3pm 
## Reading the data from participant:  Holmes_S12_170330_4pm 
## Reading the data from participant:  Holmes_S13_170330_5pm 
## Reading the data from participant:  Holmes_S14_170331_3pm 
## Reading the data from participant:  Holmes_S15_170331_4pm 
## Reading the data from participant:  Holmes_S16_170331_5pm 
## Reading the data from participant:  Holmes_S17_170406_1pm 
## Reading the data from participant:  Holmes_S18_170406_2pm 
## Reading the data from participant:  Holmes_S19_170406_3pm 
## Reading the data from participant:  Holmes_S20_170406_4pm 
## Reading the data from participant:  Holmes_S21_170406_5pm 
## Reading the data from participant:  Holmes_S22_170406_8pm 
## Reading the data from participant:  Holmes_S23_170407_12pm 
## Reading the data from participant:  Holmes_S24_170407_1pm 
## Reading the data from participant:  Holmes_S25_170407_4pm 
## Reading the data from participant:  Holmes_S26_170407_5pm 
## Reading the data from participant:  Holmes_S28_170409_8pm 
## Reading the data from participant:  Holmes_S29_170409_9pm 
## Reading the data from participant:  Holmes_S30_170410_2pm 
## Reading the data from participant:  Holmes_S31_170410_3pm 
## Reading the data from participant:  Holmes_S33_170410_7pm 
## Reading the data from participant:  Holmes_S34_170410_8pm 
## Reading the data from participant:  Holmes_S35_170410_9pm 
## Reading the data from participant:  Holmes_S36_170411_1pm 
## Reading the data from participant:  Holmes_S37_170411_3pm 
## Reading the data from participant:  Holmes_S38_170411_4pm 
## Reading the data from participant:  Holmes_S39_170411_5pm 
## Reading the data from participant:  Holmes_S40_170411_7pm 
## Reading the data from participant:  Holmes_S41_170411_8pm 
## Reading the data from participant:  Holmes_S42_170412_12pm 
## Reading the data from participant:  Holmes_S43_170412_1pm 
## Reading the data from participant:  Holmes_S44_170412_2pm 
## Reading the data from participant:  Holmes_S45_170412_3pm 
## Reading the data from participant:  Holmes_S46_170412_4pm 
## Reading the data from participant:  Holmes_S48_170412_7pm 
## Reading the data from participant:  Holmes_S49_170412_9pm 
## Reading the data from participant:  Holmes_S50_170413_8am 
## Reading the data from participant:  Holmes_S51_170413_1pm 
## Reading the data from participant:  Holmes_S52_170413_2pm 
## Reading the data from participant:  Holmes_S53_170413_4pm 
## Reading the data from participant:  Holmes_S55_170413_7pm 
## Reading the data from participant:  Holmes_S56_170413_8pm 
## Reading the data from participant:  Holmes_S57_170414_12pm 
## Reading the data from participant:  Holmes_S58_170414_2pm 
## Reading the data from participant:  Holmes_S59_170414_3pm 
## Reading the data from participant:  Holmes_S60_170414_5pm 
## Reading the data from participant:  Holmes_S61_170415_8am 
## Reading the data from participant:  Holmes_S62_170415_12pm 
## Reading the data from participant:  Holmes_S63_170415_4pm 
## Reading the data from participant:  Holmes_S64_150417_6pm 
## Reading the data from participant:  Holmes_S66_170627_4pm 
## Reading the data from participant:  Holmes_S67_170416_3pm 
## Reading the data from participant:  Holmes_S68_170416_4pm 
## Reading the data from participant:  Holmes_S69_170416_5pm 
## Reading the data from participant:  Holmes_S70_170416_6pm 
## Reading the data from participant:  Holmes_S71_170416_7pm 
## Reading the data from participant:  Holmes_S72_170416_8pm 
## Reading the data from participant:  Holmes_S73_170416_9pm 
## Reading the data from participant:  Holmes_S74_170510_1pm 
## Reading the data from participant:  Holmes_S75_170510_3pm 
## Reading the data from participant:  Holmes_S76_170614_12pm 
## Reading the data from participant:  Holmes_S77_170621_3pm 
## Reading the data from participant:  Holmes_S78_170622_2pm 
## Reading the data from participant:  Holmes_S79_170623_2pm 
## Reading the data from participant:  Holmes_S80_170623_3pm 
## Reading the data from participant:  Ocampo_S18_170412 
## Reading the data from participant:  Ocampo_S20_170412 
## Reading the data from participant:  Ocampo_S21_170413 
## Reading the data from participant:  Ocampo_S22_170413 
## Reading the data from participant:  Ocampo_S23_170413 
## Reading the data from participant:  Ocampo_S24_170413 
## Reading the data from participant:  Ocampo_S25_170413 
## Reading the data from participant:  Ocampo_S26_170413 
## Reading the data from participant:  Ocampo_S28_170502 
## Reading the data from participant:  Ocampo_S29_170502 
## Reading the data from participant:  Ocampo_S30_170502 
## Reading the data from participant:  Ocampo_S91_170315 
## Reading the data from participant:  Ocampo_S92_170315 
## Reading the data from participant:  Ocampo_S93_170315 
## Reading the data from participant:  Ocampo_S94_170315 
## Reading the data from participant:  Ocampo_S95_170315 
## Reading the data from participant:  Ocampo_S96_170322 
## Reading the data from participant:  Ocampo_S97_170322 
## Reading the data from participant:  Ocampo_S98_170322 
## Reading the data from participant:  Ocampo_S99_170322 
## Reading the data from participant:  Ocampo_S100_170322 
## Reading the data from participant:  Ocampo_S101_170322 
## Reading the data from participant:  Ocampo_S103_170329 
## Reading the data from participant:  Ocampo_S104_170329 
## Reading the data from participant:  Ocampo_S105_170329 
## Reading the data from participant:  Ocampo_S106_170329 
## Reading the data from participant:  Ocampo_S107_170329 
## Reading the data from participant:  Ocampo_S108_170329 
## Reading the data from participant:  Ocampo_S109_170331 
## Reading the data from participant:  Ocampo_S110_170331 
## Reading the data from participant:  Ocampo_S111_170331 
## Reading the data from participant:  Ocampo_S112_170405 
## Reading the data from participant:  Ocampo_S113_170405 
## Reading the data from participant:  Ocampo_S114_170405 
## Reading the data from participant:  Ocampo_S115_170405 
## Reading the data from participant:  Ocampo_S116_170405 
## Reading the data from participant:  Ocampo_S117_170405 
## Reading the data from participant:  Ocampo_S118_170405 
## Reading the data from participant:  Ocampo_S119_170412 
## Reading the data from participant:  Ocampo_S120_170412 
## Reading the data from participant:  Ocampo_S121_170412 
## Reading the data from participant:  Ocampo_S122_170412 
## Reading the data from participant:  Ocampo_S123_170412 
## Reading the data from participant:  Ocampo_S124_170412 
## Reading the data from participant:  Ocampo_S125_170412 
## Reading the data from participant:  Ocampo_S126_170412 
## Reading the data from participant:  Ocampo_S127_170419 
## Reading the data from participant:  Ocampo_S128_170419 
## Reading the data from participant:  Ocampo_S129_170419 
## Reading the data from participant:  Ocampo_S130_170419 
## Reading the data from participant:  Ocampo_S131_170419 
## Reading the data from participant:  Ocampo_S132_170419 
## Reading the data from participant:  Ocampo_S133_170424 
## Reading the data from participant:  Ocampo_S134_170424 
## Reading the data from participant:  Ocampo_S135_170424 
## Reading the data from participant:  Ocampo_S136_170424 
## Reading the data from participant:  Ocampo_S137_170424 
## Reading the data from participant:  Ocampo_S138_170424 
## Reading the data from participant:  Ocampo_S139_170424 
## Reading the data from participant:  Ocampo_S140_170424 
## Reading the data from participant:  Ansari_Participant_002 
## Reading the data from participant:  Ansari_Participant_003 
## Reading the data from participant:  Ansari_Participant_004 
## Reading the data from participant:  Ansari_Participant_005 
## Reading the data from participant:  Ansari_Participant_006 
## Reading the data from participant:  Ansari_Participant_008 
## Reading the data from participant:  Ansari_Participant_009 
## Reading the data from participant:  Ansari_Participant_010 
## Reading the data from participant:  Ansari_Participant_011 
## Reading the data from participant:  Ansari_Participant_012 
## Reading the data from participant:  Ansari_Participant_013 
## Reading the data from participant:  Ansari_Participant_014 
## Reading the data from participant:  Ansari_Participant_015 
## Reading the data from participant:  Ansari_Participant_016 
## Reading the data from participant:  Ansari_Participant_017 
## Reading the data from participant:  Ansari_Participant_018 
## Reading the data from participant:  Ansari_Participant_019 
## Reading the data from participant:  Ansari_Participant_020 
## Reading the data from participant:  Ansari_Participant_021 
## Reading the data from participant:  Ansari_Participant_022 
## Reading the data from participant:  Ansari_Participant_023 
## Reading the data from participant:  Ansari_Participant_024 
## Reading the data from participant:  Ansari_Participant_025 
## Reading the data from participant:  Ansari_Participant_026 
## Reading the data from participant:  Ansari_Participant_027 
## Reading the data from participant:  Ansari_Participant_028 
## Reading the data from participant:  Ansari_Participant_029 
## Reading the data from participant:  Ansari_Participant_030 
## Reading the data from participant:  Ansari_Participant_031 
## Reading the data from participant:  Ansari_Participant_032 
## Reading the data from participant:  Ansari_Participant_033 
## Reading the data from participant:  Ansari_Participant_034 
## Reading the data from participant:  Ansari_Participant_035 
## Reading the data from participant:  Ansari_Participant_036 
## Reading the data from participant:  Ansari_Participant_037 
## Reading the data from participant:  Ansari_Participant_038 
## Reading the data from participant:  Ansari_Participant_039 
## Reading the data from participant:  Ansari_Participant_040 
## Reading the data from participant:  Ansari_Participant_041 
## Reading the data from participant:  Ansari_Participant_042 
## Reading the data from participant:  Ansari_Participant_043 
## Reading the data from participant:  Ansari_Participant_044 
## Reading the data from participant:  Ansari_Participant_045 
## Reading the data from participant:  Ansari_Participant_046 
## Reading the data from participant:  Ansari_Participant_047 
## Reading the data from participant:  Ansari_Participant_048 
## Reading the data from participant:  Ansari_Participant_049 
## Reading the data from participant:  Ansari_Participant_050 
## Reading the data from participant:  Ansari_Participant_051 
## Reading the data from participant:  Ansari_Participant_052 
## Reading the data from participant:  Ansari_Participant_053 
## Reading the data from participant:  Ansari_Participant_054 
## Reading the data from participant:  Ansari_Participant_055 
## Reading the data from participant:  Ansari_Participant_056 
## Reading the data from participant:  Ansari_Participant_057 
## Reading the data from participant:  Ansari_Participant_058 
## Reading the data from participant:  Ansari_Participant_059 
## Reading the data from participant:  Ansari_Participant_060 
## Reading the data from participant:  Ansari_Participant_061 
## Reading the data from participant:  Ansari_Participant_062 
## Reading the data from participant:  Ansari_Participant_063 
## Reading the data from participant:  Ansari_Participant_064 
## Reading the data from participant:  Ansari_Participant_065 
## Reading the data from participant:  Ansari_Participant_066 
## Reading the data from participant:  Ansari_Participant_067 
## Reading the data from participant:  Ansari_Participant_068 
## Reading the data from participant:  Bryce_S1_170324 
## Reading the data from participant:  Bryce_S10_170510 
## Reading the data from participant:  Bryce_S11_170510 
## Reading the data from participant:  Bryce_S12_170510 
## Reading the data from participant:  Bryce_S13_170511 
## Reading the data from participant:  Bryce_S14_170512 
## Reading the data from participant:  Bryce_S15_170512 
## Reading the data from participant:  Bryce_S16_170512 
## Reading the data from participant:  Bryce_S17_170512 
## Reading the data from participant:  Bryce_S18_170512 
## Reading the data from participant:  Bryce_S19_170515 
## Reading the data from participant:  Bryce_S2_170424 
## Reading the data from participant:  Bryce_S20_170515 
## Reading the data from participant:  Bryce_S21_170515 
## Reading the data from participant:  Bryce_S22_170517 
## Reading the data from participant:  Bryce_S23_170516 
## Reading the data from participant:  Bryce_S24_170516 
## Reading the data from participant:  Bryce_S25_170516 
## Reading the data from participant:  Bryce_S26_170516 
## Reading the data from participant:  Bryce_S27_170516 
## Reading the data from participant:  Bryce_S28_170516 
## Reading the data from participant:  Bryce_S29_170517 
## Reading the data from participant:  Bryce_S3_170508 
## Reading the data from participant:  Bryce_S30_170517 
## Reading the data from participant:  Bryce_S31_170517 
## Reading the data from participant:  Bryce_S32_170517 
## Reading the data from participant:  Bryce_S33_170518 
## Reading the data from participant:  Bryce_S34_170519 
## Reading the data from participant:  Bryce_S35_170519 
## Reading the data from participant:  Bryce_S36_170519 
## Reading the data from participant:  Bryce_S37_170519 
## Reading the data from participant:  Bryce_S38_170519 
## Reading the data from participant:  Bryce_S39_170522 
## Reading the data from participant:  Bryce_S4_170508 
## Reading the data from participant:  Bryce_S40_170522 
## Reading the data from participant:  Bryce_S41_170522 
## Reading the data from participant:  Bryce_S42_170522 
## Reading the data from participant:  Bryce_S43_170522 
## Reading the data from participant:  Bryce_S44_170523 
## Reading the data from participant:  Bryce_S45_170523 
## Reading the data from participant:  Bryce_S46_170523 
## Reading the data from participant:  Bryce_S47_170523 
## Reading the data from participant:  Bryce_S48_170524 
## Reading the data from participant:  Bryce_S49_170524 
## Reading the data from participant:  Bryce_S5_170509 
## Reading the data from participant:  Bryce_S50_170524 
## Reading the data from participant:  Bryce_S51_170529 
## Reading the data from participant:  Bryce_S52_170529 
## Reading the data from participant:  Bryce_S53_170530 
## Reading the data from participant:  Bryce_S54_170530 
## Reading the data from participant:  Bryce_S55_170531 
## Reading the data from participant:  Bryce_S56_170531 
## Reading the data from participant:  Bryce_S57_170531 
## Reading the data from participant:  Bryce_S58_170601 
## Reading the data from participant:  Bryce_S59_170601 
## Reading the data from participant:  Bryce_S6_170509 
## Reading the data from participant:  Bryce_S60_170602 
## Reading the data from participant:  Bryce_S61_170602 
## Reading the data from participant:  Bryce_S62_170607 
## Reading the data from participant:  Bryce_S63_170626 
## Reading the data from participant:  Bryce_S64_170629 
## Reading the data from participant:  Bryce_S65_170630 
## Reading the data from participant:  Bryce_S66_170710 
## Reading the data from participant:  Bryce_S67_170710 
## Reading the data from participant:  Bryce_S68_170714 
## Reading the data from participant:  Bryce_S7_170510 
## Reading the data from participant:  Bryce_S8_170510 
## Reading the data from participant:  Bryce_S9_170510 
## Reading the data from participant:  COLLING_S001_170313 
## Reading the data from participant:  COLLING_S002_170313 
## Reading the data from participant:  COLLING_S004_170313 
## Reading the data from participant:  COLLING_S005_170314 
## Reading the data from participant:  COLLING_S006_170314 
## Reading the data from participant:  COLLING_S007_170314 
## Reading the data from participant:  COLLING_S008_170314 
## Reading the data from participant:  COLLING_S009_170314 
## Reading the data from participant:  COLLING_S011_170320 
## Reading the data from participant:  COLLING_S012_170320 
## Reading the data from participant:  COLLING_S015_170321 
## Reading the data from participant:  COLLING_S016_170321 
## Reading the data from participant:  COLLING_S017_170321 
## Reading the data from participant:  COLLING_S018_170322 
## Reading the data from participant:  COLLING_S019_170322 
## Reading the data from participant:  COLLING_S020_170322 
## Reading the data from participant:  COLLING_S021_170322 
## Reading the data from participant:  COLLING_S022_170322 
## Reading the data from participant:  COLLING_S023_170323 
## Reading the data from participant:  COLLING_S024_170324 
## Reading the data from participant:  COLLING_S025_170324 
## Reading the data from participant:  COLLING_S026_170325 
## Reading the data from participant:  COLLING_S027_170327 
## Reading the data from participant:  COLLING_S028_170327 
## Reading the data from participant:  COLLING_S029_170328 
## Reading the data from participant:  COLLING_S030_170403 
## Reading the data from participant:  COLLING_S031_170403 
## Reading the data from participant:  COLLING_S032_170403 
## Reading the data from participant:  COLLING_S033_170403 
## Reading the data from participant:  COLLING_S034_170403 
## Reading the data from participant:  COLLING_S035_170403 
## Reading the data from participant:  COLLING_S036_170404 
## Reading the data from participant:  COLLING_S037_170410 
## Reading the data from participant:  COLLING_S038_170410 
## Reading the data from participant:  COLLING_S039_170412 
## Reading the data from participant:  COLLING_S040_170413 
## Reading the data from participant:  COLLING_S041_170413 
## Reading the data from participant:  COLLING_S042_170413 
## Reading the data from participant:  COLLING_S043_170424 
## Reading the data from participant:  COLLING_S044_170424 
## Reading the data from participant:  COLLING_S045_170425 
## Reading the data from participant:  COLLING_S046_170508 
## Reading the data from participant:  COLLING_S047_170508 
## Reading the data from participant:  COLLING_S048_170508 
## Reading the data from participant:  COLLING_S049_170509 
## Reading the data from participant:  COLLING_S050_170515 
## Reading the data from participant:  COLLING_S051_170518 
## Reading the data from participant:  COLLING_S052_170519 
## Reading the data from participant:  COLLING_S053_170523 
## Reading the data from participant:  COLLING_S054_170526 
## Reading the data from participant:  COLLING_S055_170531 
## Reading the data from participant:  COLLING_S056_170606 
## Reading the data from participant:  COLLING_S057_170606 
## Reading the data from participant:  COLLING_S058_170606 
## Reading the data from participant:  COLLING_S059_170608 
## Reading the data from participant:  COLLING_S060_170613 
## Reading the data from participant:  COLLING_S061_170616 
## Reading the data from participant:  COLLING_S062_170616 
## Reading the data from participant:  COLLING_S063_170619 
## Reading the data from participant:  COLLING_S064_170621 
## Reading the data from participant:  COLLING_S065_170622 
## Reading the data from participant:  COLLING_S066_170622 
## Reading the data from participant:  COLLING_S067_170622 
## Reading the data from participant:  COLLING_S068_170623 
## Reading the data from participant:  COLLING_S069_170626 
## Reading the data from participant:  COLLING_S070_170628 
## Reading the data from participant:  COLLING_S071_170705 
## Reading the data from participant:  COLLING_S072_170705 
## Reading the data from participant:  moeller_02_170512 
## Reading the data from participant:  moeller_03_170523 
## Reading the data from participant:  moeller_04_170523 
## Reading the data from participant:  moeller_05_170530 
## Reading the data from participant:  moeller_06_170530 
## Reading the data from participant:  moeller_07_170607 
## Reading the data from participant:  moeller_09_170613 
## Reading the data from participant:  moeller_10_170613 
## Reading the data from participant:  moeller_11_170616 
## Reading the data from participant:  moeller_12_170619 
## Reading the data from participant:  moeller_13_170623 
## Reading the data from participant:  moeller_15_170627 
## Reading the data from participant:  moeller_18_170707 
## Reading the data from participant:  moeller_20_170707 
## Reading the data from participant:  moeller_22_170711 
## Reading the data from participant:  moeller_23_170714 
## Reading the data from participant:  moeller_24_170714 
## Reading the data from participant:  moeller_25_170717 
## Reading the data from participant:  moeller_26_170717 
## Reading the data from participant:  moeller_28_170718 
## Reading the data from participant:  moeller_29_170718 
## Reading the data from participant:  moeller_30_170724 
## Reading the data from participant:  moeller_31_170725 
## Reading the data from participant:  moeller_32_170725 
## Reading the data from participant:  moeller_35_170801 
## Reading the data from participant:  moeller_36_170801 
## Reading the data from participant:  moeller_37_170807 
## Reading the data from participant:  moeller_38_170807 
## Reading the data from participant:  moeller_39_170807 
## Reading the data from participant:  moeller_40_170808 
## Reading the data from participant:  moeller_42_170808 
## Reading the data from participant:  moeller_43_170808 
## Reading the data from participant:  moeller_44_170811 
## Reading the data from participant:  moeller_46_170927 
## Reading the data from participant:  moeller_47_170927 
## Reading the data from participant:  moeller_49_170928 
## Reading the data from participant:  moeller_50_170928 
## Reading the data from participant:  moeller_51_170928 
## Reading the data from participant:  moeller_52_170928 
## Reading the data from participant:  moeller_53_170928 
## Reading the data from participant:  moeller_54_170929 
## Reading the data from participant:  moeller_55_170929 
## Reading the data from participant:  moeller_56_170929 
## Reading the data from participant:  moeller_57_171002 
## Reading the data from participant:  moeller_58_171002 
## Reading the data from participant:  moeller_59_171002 
## Reading the data from participant:  moeller_60_171002 
## Reading the data from participant:  moeller_61_171004 
## Reading the data from participant:  moeller_62_171004 
## Reading the data from participant:  moeller_63_171004 
## Reading the data from participant:  moeller_64_171004 
## Reading the data from participant:  moeller_65_171005 
## Reading the data from participant:  moeller_66_171005 
## Reading the data from participant:  moeller_67_171005 
## Reading the data from participant:  moeller_68_171006 
## Reading the data from participant:  moeller_69_171006 
## Reading the data from participant:  moeller_70_171009 
## Reading the data from participant:  moeller_71_171009 
## Reading the data from participant:  moeller_72_171009 
## Reading the data from participant:  moeller_73_171010 
## Reading the data from participant:  moeller_74_171010 
## Reading the data from participant:  moeller_75_171010 
## Reading the data from participant:  moeller_76_171010 
## Reading the data from participant:  moeller_77_171011
```


You can load the [Data processing notebook](data_processing.Rmd).
