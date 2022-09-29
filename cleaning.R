## Note: You should create a directory labeled "data" in whatever your working
## Directory is. Then you can put all the data files in that directory.

## Load packages
library(tidyverse)
library(haven)
library(psych)

## Read data
data <- read_sav("data/NHatsFull090822.sav")

## Take a look at the data
names(data)
str(data)

## Describe some of the data (using psych package)
describe(data[, 2:45])

## Correlate health vars
cor(data[, 2:12], use = "pair") # 'use = "pair"' for pairwise deletion

## Correlate Purpose vars
cor(data[, 13:23], use = "pair")

## Correlate Recall vars
cor(data[, 24:34], use = "pair")

## Correlate Positive Emotion vars
cor(data[, 35:45], use = "pair")

write_csv(data, "data/nhatsCleaned.csv")
