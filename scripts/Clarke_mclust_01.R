## Header --------------------------------------------------------------------
##
## Script name: mclust_steps_for_Clarke
##
## Purpose of script: Get Clarke started writing loops in R and understanding
## how mclust works
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes: Start by running the code in this header. That will install the
##        packages you need. The options below are just the preferences I use
##        in every script file I create. 
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(scipen = 6, digits = 4, width=80)
set.seed(1014)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Required packages ---------------------------------------------------------- 
require(readr) #to load data into R
require(tidyverse) # essential tools for wrangling and visualizing data 
require(mclust) # package for model-based clustering
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- Loading Data in R  ---- 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readr)
pisaUSA15 <- read_csv("data/pisaUSA15.csv")
pisaUSA15na <- tidyr::drop_na(pisaUSA15) #remove all NA values
##
pisa <- dplyr::sample_n(pisaUSA15na , 500) # randomly select 500 rows
summary (pisa)
## END HEADER ##
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- Get familiar with Mclust  ---- 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
??Mclust # load the documentation for Mclust command
xtest <- Mclust (pisa, # data
                 G=1:5,# of possible clusters
                 initialization=list(hcpairs=randomPairs(pisa)) #random start
                 )
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
xtest$bic # the BIC of the best model
#
# Here's how BIC calculated: 
##
LL <- xtest$loglik      # Log Likelihood of the best model in xtest run (EEI, G=5)
logn <- log(nrow(pisa)) # natural log of n (the number of observations in data)
k <- xtest$df           # number of estimated parameters in best model

# To get the BIC using the mclust definition/formula: 
xtestBIC <- 2*LL - (k*logn)
# a more common way to calculate BIC is: 
xtestothBIC <- -2*LL + (k*logn)
##
## One issue with this method is that we can't assume that the alogorithm
## as chosen the best model. So, we at the very least we want to run the same 
## model multiple times with different random starting points. Even better, we
## want to be able to use a variety of metrics to assess models. 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- Looking for "best model" ---- 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?randomPairs
?mclustBIC
?mclustBICupdate
# create two sets of random pairs
randpair1 <- mclust::randomPairs(pisa, seed=1)
randpair2 <- mclust::randomPairs(pisa, seed=2)
# The mclustBIC command can be run repeatedly, and then, mclustBIC
# can update based on the best model across the two runs
pisaBIC <- mclustBIC(pisa, G=1:5, 
                     ititialization=list(hcpairs=randpair1)
                     )

pisaBIC2 <- mclustBIC(pisa, G=1:5, 
                     ititialization=list(hcpairs=randpair2)
)
pisaBICbest <- mclustBICupdate(pisaBIC, pisaBIC2)

# This is essentially the loop we want to run.
# Goal is to run mclust x times (lets say 25 for now), ensuring that
# each run represents a different random partition of the data as 
# a starting point (that's what the RandomPairs command does) for the E-step
# of the algorithm. 

## Eventually, however, what I'd like to do is choose the best model 
## based on a number of dfferent criteria. Mcust allows for doing this with
## the Integrated Likelihood Criteria and then using the mclustBICupdate command
## to look across multiple model runs. 
?mclustICL
# IF we go back to the best model based on xtest run above: 
icl(xtest)


pisaICL <- mclustICL(pisa, G=1:5, 
                     ititialization=list(hcpairs=randpair1)
)

pisaICL2 <- mclustICL(pisa, G=1:5, 
                      ititialization=list(hcpairs=randpair2)
)
pisaICLbest <- mclustBICupdate(pisaICL, pisaICL2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- Task 1 ---- 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## A good first task would be to program 2 loops:
##                              one using mclustBIC and one using mclustICL
## Let's run each loop 25 times with random starts each time.
##
## Then, combine the results of the loop and use the mclustBICupdate function (or
## some other method) to identify the best three models according to BIC 
## AND the best three models accoring to ICL.
##
##
##
##