#=============================================#
#====== DISCRETE EVENT HISTORY ANALYSIS ======#
#=============================================#

# Author: Jonah Gabry (jsg2201@columbia.edu)
# Written using R version 3.1.1 on Mac OS X 10.9.3
# Last Edited: 07/21/2014



# Setup -------------------------------------------------------------------
# _________________________________________________________________________

# set working directory (replace text inside quotes with path to the desired directory)
setwd("INSERT PATH TO DIRECTORY") 

# Load packages
library(QMSS)
library(plyr)
library(psych)

# load GSS panel data 
load("GSS_panel.RData")



# One-time events ---------------------------------------------------------
# _________________________________________________________________________

### What factors affect someone’s probability of becoming employed full-time? ###

pd$fulltime <- ifelse(pd$wrkstat == 1, 1, 0)
with(pd, XTab(fulltime, panelwave))