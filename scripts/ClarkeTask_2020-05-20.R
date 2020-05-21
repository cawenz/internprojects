## Header --------------------------------------------------------------------
##
## Script name: Making log-likelihood table from Mclust Object
##
## Purpose of script: to introduce important ideas to Clarke
##
## Author: Chris Wenz
##
## Date Created: 2020-05-20
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes: Run the code and read through the MANY comments in this script. The 
## script opens by loading required packages. 
## 
## Your job is to write a loop/function that will produce a table 
## of log-likelihood (LL) values for all of the models estimated in a call to Mclust. 
## Knowing those LL values makes it incredibly easy to then calculate any number of
## relative fit statistics.
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(scipen = 6, digits = 4, width=80)
set.seed(1014) # to ensure reproducability of results

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Required packages ---------------------------------------------------------- 

require(tidyverse) 
?tidyverse #opens documentation in Rstudio
require(mclust)
require(readr)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in required data
library(readr)
pisa <- read_csv("data/pisa500n.csv") #pull from data folder in project directory
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
## ---- 1. Make a table of all BIC values from xtest$BIC ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if we use: negBIC <- as.data.frame(xtest$BIC) we get an error that we can't
# coerce xtest$BIC to a data.frame. BUT, if we tell as.data.frame to grab all
# to rows and columns of BIC values (and not all the information 
# stored in xstest$BIC) we can coerce to data.frame.
negBIC <- as_tibble(xtest$BIC[,])
#using the expression [,] tells as_tibble to take all rows and columns 
# 
# Now we can transform this tibble and reverse the sign of the 
# dumb BIC values in mclust by multiplying every cell by -1.
bictbl<- -1*negBIC
head(bictbl)
# remember that the empty cells are totally fine. They are empty because those
# models did not "converge"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2. ---- Get log-likelihood (LL) of models other than the "best" model ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The formula to get the BIC values in bictbl is: 
# BIC = -2*LL + (df * log(n)) where df is the number of estimated parameters in the
# model and n is the sample size of the data (xtest$n)
# 
# if we know df and n, we can solve for LL: 
# LL = (BIC - (df*log(n)))/-2
#
# we can test this formula for LL with the information from the "best" model in xtest
xtest$BIC # best model is EEE with 4 clusters (g=4)
xtest$df # number of estimated parameters of best model (EEE, G=4)
# we could get the same information about df of best model by using this function
# in mclust; d=dimension of the data (i.e. how many variables)
xtest$n # n of data (pisa)
nMclustParams("EEE", d=4, G=4)
 # LL for best model
xtest$bic # BIC for best model, but we want the value with the reversed sign
bestbic <- bictbl[4,7] # EEE 4 is row 4 column 7 in bictbl
# even though we know LL for this model, let's just test our formula
# make value of df*log(n) for this model
dflogn <- xtest$df*(log(xtest$n)) # log in R is natural logarithm
LLbestmodel <- (bestbic - dflogn)/-2
LLbestmodel
# compare to xtest$loglik
xtest$loglik
#
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3. ---- Get df for all of the models ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Knowing the formula for LL works, we want to be able to do this for all of the
# BIC values in bictbl. To do that we need to know the df for all of those 
# models. 
#
# We can get df for any model using nMclustParams
??nMclustParams
# We want to be able to loop through nMClustParams for every model and for multiple
# values of G. 
# The code below is a very clunky way to get the table of estimated parameters. I use
# mapply to cycle through the mclust model names, which can be called using: 
mclust.options("emModelNames")
# This is the process that a loop could accomplish. The loop would cycle through
# the 14 "emModelNames" and a range of values of G. d can remain fixed using 
# the value of xtest$d. "as_tibble_row" is a function in a tibble package (part of
# the tidyverse) and it ensures that the output is stored as rows that we can then bind 
# together to make a single table of df values. 
# A tibble is still a dataframe, it's just a little easier to manipulate
# https://tibble.tidyverse.org/
#
g1 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 1))
g2 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 2))
g3 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 3))
g4 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 4))
g5 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 5))
# now that we have the individual rows, we can bring them all together using rbind and
# save it as a tibble. 
dfbind <- as_tibble(rbind(g1,g2, g3, g4, g5))
dfbind
# now we have a tibble that is exactly the same dimensions as the dataframe of BIC values we made
# called bictbl. 
head(bictbl)
# so the BIC value of the model EEI when G=3 will 
# have the coordinates [3,3] in bictbl
# the df for model EEI when G=3 will have the same coordinates in dfbind
# 
# because they line up perfectly, we can just subtract (or add, multiple etc)
# the data frames from each other. This will be super handy when we want to get
# LL for all of the models. 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4. ---- Make LL table for all of the models ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now we know the BIC, df and the n of all of the models. 
# We can use the bictbl and df bind to get a table of LL values. 
# If the formula for LL = BIC - (df*log(n)))/-2 we should first make a table of
# values of df*log(n)
# 
xtestlogn <- log(xtest$n) # store value of log(n)
# Make a function for df*log(n)
lognmult <- function(x) (x * xtestlogn)
# apply function to all values in dfbind using mutate_all in dplyr
??mutate_all
dfbylogn <- mutate_all(dfbind, funs(lognmult))
head(dfbylogn)
# we now have a table of all values of df multiplied by log()
# to get the table of LL values we can subtract dfbylogn from bictbl
# then divide that table by -2. 
xtestLLtbl <- (bictbl - dfbylogn)/-2
xtestLLtbl
# Let's check our work quickly. The best model in xtest was EEE when G=4. 
# The LL value for that model in our new table is in row 4, column 7. 
xtestLLtbl[4,7]
# compare to value in xtest object
xtest$loglik
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5. ---- Moving forward ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# So the goal is to make this process of getting the table of log-likelihood 
# values a bit more automated. The key will be to generate the table of df values
# based on these values stored in the mclust object:  
# the models tested 
# the dimension of the data in an mclust object (d) 
# the values of G
# xtest$d
# 
# All of that information is in the mclust object. using modelName$d will give you
# the dimension of the data in a usable form. We need to pull the specific values 
# from the Mclust object, because we need the table of df values to perfectly match 
# the table of BIC values.  
# Take for example, this Mclust call, which specifies only a few Model types, and
# G=2:6. 
clust1 <- Mclust(pisa, G=2:6, modelNames = c("EEE", "EVE", "VEE", "VVE"))
# Make BIC table
data.frame(clust1$BIC[,])
# we have a 6x4 table of BIC values, and there is no row for G=1. 
# So any loop to get df values via nMclustParams, will need to take the 
# particulars of this and any Mclust object. 
#
# Getting d from the Mclust object is nice and easy
clust1$d

# Getting G and modelNames is a little trickier. The "call" will tell you what you need but that 
# is a "language" object and a pain to deal with. 
# tricky to deal with. 
clust1$call
typeof(clust1$call)
# 
# We need to find the specific attributes of somewhere in the Mclust object.
# We can look specifically at the attributes associated with the BIC of the Mclust object.
# look at all of the attributes of clust1$BIC
??base::attributes #get documentation on attributes
attributes(clust1$BIC)
# there's an attribute of G
attr(clust1$BIC, "G")
# and there's an attribute of "modelNames"
attr(clust1$BIC, "modelNames")
# these could be pulled from any Mclust object and then used in the loop to get a table of DF values
clust1G <- attr(clust1$BIC, "G") 
#this will save G as set of values. Could also be turned into a tibble too. 
typeof(clust1G) 
# the values are numberic (not characters) 
# which is good because that's what nMclustParams needs for the G= argument
clust1Mods <- attr(clust1$BIC, "modelNames") 
typeof(clust1Mods) 
# the values are characters (hence the quotation marks)
# which is good because that's what nMclustParams needs for the modelName arguments. 

# We can use the attribute we stored as clust1Mods 
# to tell mapply to run for all of the clust1 models
# in the same way we used mclust.options("emModelNames)
# Let's do that for G=2 just for an example
mapply(nMclustParams,clust1Mods, d=clust1$d, G = 2)
# So all we would need to do is cycle through all of the models for all of the values of G
# and store that in a table. 

# THATS YOUR TASK! 
