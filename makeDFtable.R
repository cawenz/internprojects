## Header --------------------------------------------------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Chris Wenz
##
## Date Created: 2020-05-20
##
## Copyright (c) Chris Wenz, 2020
## Email: cawenz@gmail.com
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Notes:
##   
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(scipen = 6, digits = 8, width=80)
set.seed(1014)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Required packages ---------------------------------------------------------- 

require(tidyverse)
require(mclust)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 g1 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 1))
 g2 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 2))
 g3 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 3))
 g4 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 4))
 g5 <- as_tibble_row(mapply(nMclustParams, mclust.options("emModelNames"), d=xtest$d, G = 5))

dfbind <- as_tibble(rbind(g1,g2, g3, g4, g5))

lognmult <- function(x) (x * xtestlogn)
dfbylogn <- mutate_all(dfbind, funs(lognmult))
xtestLLtbl <- (bictbl - dfbylogn)/-2
xtest$loglik

SABICxtest <- xtestLLtbl
sabicN <- log((xtest$n + 2)/24)
sabicmult <- function(x) (x * sabicN)
minustwo <- function(x) (x * -2)


SABICdflog <- mutate_all(dfbind, funs(sabicmult))
SABICminus2 <- mutate_all(bictbl, funs(minustwo))
SABICxtest <- SABICminus2 + SABICdflog

