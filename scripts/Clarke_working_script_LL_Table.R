## Header --------------------------------------------------------------------
##
## Script name: Clarke_working_script_LL_Table
##
## Purpose of script: For Clarke to write loop/function to make LL table
##
## Author: Clarke Geagan
##
## Date Created: 2020-05-21
##
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes: Use this script to write loop/function to make an LL table using
## the data stores in an Mclust object. You can use the pisa500n data to 
## experiment with. I'll write the code to load that data. 
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(scipen = 6, digits = 4, width=80)
set.seed(1014)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Required packages ---------------------------------------------------------- 

require(tidyverse)
require(mclust)

## END HEADER

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load pisa data 
library(readr)
pisa <- read_csv("data/pisa500n.csv") 
#this will only work if you have the folder "internprojects" set as the working
# directory. You couls also just specify the path to the file on your computer.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clarke's work starts here ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
