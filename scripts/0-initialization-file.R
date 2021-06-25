# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Load packages
# Note: I believe that all of these packages are used in this code (or were used in various iterations of it, some
# of which may be deprecated).

library(ggalt)
library(tidyverse)
library(foreign)
library(readr)
library(readxl)
library(data.table)
library(statar)
library(sp)
library(rgdal)
library(stringr)
library(fuzzyjoin)
library(rlang)
library(dplyr)
library(magrittr)
library(randomForest)
library(gbm)
library(boot)
library(caret)
library(xgboost)
library(broom)
library(ggrepel)
library(glmnet)
library(zoo)
library(maps)
library(tidycensus)
library(vcd)

# Two directories of interest here:
# (1) Data directory, from Elaine's data folder from the NCHS.
# (2) My personal directory, which connects my Box folder on my external hard-drive.

# Set directory
# Note: I set my working directory to the root of my research folder (which is synced in Box due to
# organizational preferences). 

setwd("C:/Users/aboslett/Box")

# Set folder structure in your research directory.
# Note: I create a project folder for data, scratch (e.g., intermediate files), and folders
# for documents and figures created in support of the paper.

dir.create('Opioids_ML_2', showWarnings = TRUE)

for(fff in c('Data', 'Scratch', 'Documents', 'Figures', 'Tables')) {
  
  dir.create(paste0('Opioids_ML_2/', fff), showWarnings = TRUE)
  
}

# Our data folder is in a remote network drive with greater security than what is available from Box. Our base mortality
# files are saved as CSV files in a CSV folder in the network drive. We save intermediate files derived from the mortality
# CSV files as RDS files into a RDS folder in the remote drive.

remote_drive <- '//smdnas/Hill_Lab/Mortality'

remote_drive_csv <- paste0(remote_drive, '/CSV')
remote_drive_rds <- paste0(remote_drive, '/RDS')