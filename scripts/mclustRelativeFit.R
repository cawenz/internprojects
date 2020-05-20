## Header --------------------------------------------------------------------
##
## Script name: RelativeFitFunctions
##
## Purpose of script: explore creating mclust functions for relative fit
##
## Author: Chris Wenz and Clarke Geagan
##
## Date Created: 2020-05-20
##
## Copyright (c) Chris Wenz, 2020
## Email: cawenz@gmail.com
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes: Bef
##   
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(scipen = 6, digits = 4, width=80)
set.seed(1014)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Required packages ---------------------------------------------------------- 

require(tidyverse)
require(mclust)
require(readr)
require(data.table)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in pisa500n data
library(readr)
pisa <- read_csv("data/pisa500n.csv")
head (pisa)
## END HEADER ##
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- Get an Mclust object "xtest" to use throughout this script ---- 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
??Mclust # load the documentation for Mclust in Rstudio Help pane
#
xtest <- Mclust (pisa, # data
                 G=1:5,# of possible clusters
                 initialization=list(hcpairs=randomPairs(pisa))) #random start
#
# Look at xtest: it is an mclust object containing results of the mclust run
summary (xtest, parameters=T) # show results of the best model (using BIC)
#uncomment the next two lines if you want to see the plots mclust generates
# plot (xtest, what="BIC")
#plot (xtest, what="density")
##
xtest$BIC ## all of the models that were run, with corresponding BIC value
# BIC is the "Bayesian Information Criterion" and is used in lots of
# situations to compare models based on how well they fit the data.
## The larger the BIC value, the better the model fits the data. 
## 
# Save some values from the xtest object; these are all numeric so they can be
# used in place of other
xtest$bic # the BIC of the best model
xtest$loglik # the log-likelihood of the best model
xtest$d # the dimension of the data (i.e. how many variables)
xtest$n # number of observations in the data
xtest$df # number of estimated parameters in the best model
#
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- Some Code from the HTML document titled "mclustfunctionsforClarke" ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1. Make a table from xtest$BIC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if we use: negBIC <- as.data.frame(xtest$BIC) we get an error that we can't
# coerce xtest$BIC to a data.frame. BUT, if we tell as.data.frame to grab all
# to rows and columns of BIC values (and not all the information 
# stored in xstest$BIC) we can coerce to data.frame.
negBIC <- as_tibble(xtest$BIC[,])
#using the expression [,] tells as_tibble to take all rows and columns 
# 
# Now we can transform this tibble and reverse the sign of the 
# dumb BIC values in mclust. 
bictbl<- -1*negBIC
# this multiplies every cell in negBIC by -1
bictbl
# remember that the empty cells are totally fine. They are empty because those
# models did not "converge"
#
# 2. Get the log-likelihood (LL) for a model in xtext 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 