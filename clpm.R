library(tidyverse)
library(lavaan)

library(haven)
library(psych)
library(ggplot2)

## Read utilities
source("~/Projects/starts/scripts/usefulFunctions.R")

## Read script to build models
source("~/Projects/code-generator/buildModel.R")

## Read data
## data <- read_csv("nhatsCleaned_new.csv")
data <- read_csv("data/nhatsCleaned.csv")


## Get Data
dataSelect <- data %>%
  select(starts_with("PositiveEmotion"),
         starts_with("health"))
names(dataSelect) <- c(paste0("x", 1:11),
                       paste0("y", 1:11))


test <- buildStarts2(11)
testFit <- lavaan(test, data=dataSelect, estimator = "MLR", missing="fiml")
summary(testFit)


riclpm <- buildRiclpm(11)
riclpmFit <- lavaan(ricplm, data=dataSelect, estimator = "MLR", missing="fiml")
summary(riclpmFit)


## Model with no trait variance
test <- buildModel(11, trait=FALSE, stationarity=TRUE)
testFit <- lavaan(test, data=dataSelect, estimator = "MLR", missing="fiml")
summary(testFit, fit.measures=TRUE)

## Regular CLPM
clpm <- buildClpm(11)
clpmFit <- lavaan(clpm, data = dataSelect, estimator = "MLR", missing = "fiml")
summary(clpmFit, fit.measures = TRUE)

