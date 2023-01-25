################################################################################
## Load Packages
################################################################################
library(tidyverse)
library(lavaan)

################################################################################
## Read Data
################################################################################
data <- read_csv("data/nhatsCleaned_new.csv")

################################################################################
## Read script for generating model
################################################################################

source("~/Projects/code-generator/buildModel.R")

################################################################################
## Growth Model
################################################################################

gc_model <- '

## Intercept
i =~ 1*v1W1 +
1*v1W2 +
1*v1W3 +
1*v1W4 +
1*v1W5 +
1*v1W6 +
1*v1W7 +
1*v1W8 +
1*v1W9 +
1*v1W10 +
1*v1W11

## Slope
s =~ 0*v1W1 +
1*v1W2 +
2*v1W3 +
3*v1W4 +
4*v1W5 +
5*v1W6 +
6*v1W7 +
7*v1W8 +
8*v1W9 +
9*v1W10 +
10*v1W11

'


gc_model_un <- '

## Intercept
i =~ 1*v1W1 +
1*v1W2 +
1*v1W3 +
1*v1W4 +
1*v1W5 +
1*v1W6 +
1*v1W7 +
1*v1W8 +
1*v1W9 +
1*v1W10 +
1*v1W11

## Slope
s =~ 0*v1W1 +
v1W2 +
v1W3 +
v1W4 +
v1W5 +
v1W6 +
v1W7 +
v1W8 +
v1W9 +
v1W10 +
1*v1W11

'



## Sometimes it helps to use a generic model and then rename vars each time
dataSelect <- data %>%
    select(starts_with("PositiveEmotion"),
           starts_with("health"))

names(dataSelect) <- c(paste0("v1W", 1:11),
                       paste0("v2W", 1:11))

gc_fit <- growth(gc_model, data=dataSelect) ## Lavaan command to run growth model
summary(gc_fit) ## Command to show results
fitMeasures(gc_fit)
standardizedSolution(gc_fit)

gc_fit_un <- growth(gc_model_un, data=dataSelect) ## Lavaan command to run growth model
summary(gc_fit_un) ## Command to show results
fitMeasures(gc_fit_un)
standardizedSolution(gc_fit_un)


################################################################################
## Bivariate Growth Model
################################################################################

## We have to specify the model slightly differently when doing bivariate model.
## When moving to bivariate version, we have to specify some things lavaan took
## care of in the univariate setting.

gcb_model <- '
## Variable 1
## Intercept
i =~ 1*v1W1 +
1*v1W2 +
1*v1W3 +
1*v1W4 +
1*v1W5 +
1*v1W6 +
1*v1W7 +
1*v1W8 +
1*v1W9 +
1*v1W10 +
1*v1W11

## Slope
s =~ 0*v1W1 +
1*v1W2 +
2*v1W3 +
3*v1W4 +
4*v1W5 +
5*v1W6 +
6*v1W7 +
7*v1W8 +
8*v1W9 +
9*v1W10 +
10*v1W11

## Variable 2
## Intercept
i2 =~ 1*v2W1 +
1*v2W2 +
1*v2W3 +
1*v2W4 +
1*v2W5 +
1*v2W6 +
1*v2W7 +
1*v2W8 +
1*v2W9 +
1*v2W10 +
1*v2W11

## Slope
s2 =~ 0*v2W1 +
1*v2W2 +
2*v2W3 +
3*v2W4 +
4*v2W5 +
5*v2W6 +
6*v2W7 +
7*v2W8 +
8*v2W9 +
9*v2W10 +
10*v2W11

#### Everything below this line needs to be specified in bivariate model
## Factor Means
i ~ 1
s ~ 1
i2 ~ 1
s2 ~ 1

## Manifest means fixed at 0
v1W1 ~ 0
v1W2 ~ 0
v1W3 ~ 0
v1W4 ~ 0
v1W5 ~ 0
v1W6 ~ 0
v1W7 ~ 0
v1W8 ~ 0
v1W9 ~ 0
v1W10 ~ 0
v1W11 ~ 0

v2W1 ~ 0
v2W2 ~ 0
v2W3 ~ 0
v2W4 ~ 0
v2W5 ~ 0
v2W6 ~ 0
v2W7 ~ 0
v2W8 ~ 0
v2W9 ~ 0
v2W10 ~ 0
v2W11 ~ 0

## Factor Variances
i ~~ i
s ~~ s
i2 ~~ i2
s2 ~~ s2

## Factor covariances
i ~~ s
i ~~ i2
i ~~ s2
s ~~ i2
s ~~ s2
i2 ~~ s2

## Variances of observed variables
v1W1 ~~ v1W1
v1W2 ~~ v1W2
v1W3 ~~ v1W3
v1W4 ~~ v1W4
v1W5 ~~ v1W5
v1W6 ~~ v1W6
v1W7 ~~ v1W7
v1W8 ~~ v1W8
v1W9 ~~ v1W9
v1W10 ~~ v1W10
v1W11 ~~ v1W11

v2W1 ~~ v2W1
v2W2 ~~ v2W2
v2W3 ~~ v2W3
v2W4 ~~ v2W4
v2W5 ~~ v2W5
v2W6 ~~ v2W6
v2W7 ~~ v2W7
v2W8 ~~ v2W8
v2W9 ~~ v2W9
v2W10 ~~ v2W10
v2W11 ~~ v2W11
'

#### Positive Emotion and Health
#### Select vars and rename to use generic model

## Sometimes it helps to use a generic model and then rename vars each time
dataSelect <- data %>%
    select(starts_with("PositiveEmotion"),
           starts_with("health"))
names(dataSelect) <- c(paste0("v1W", 1:11),
                       paste0("v2W", 1:11))


gcb_fit <- sem(gcb_model, data=dataSelect)
summary(gcb_fit)
standardizedSolution(gcb_fit)
fitMeasures(gcb_fit)


## Plot Trajectory

## Create dataframe with predicted values
## Pull out coefficients from model fit object using "coef()"
coefs <- c(0,coef(gc_fit_un)[1:9], 1)

## Create predicted values by multiplying coefficients by slope intercept
predicted <- coefs * coef(gc_fit_un)[25]

## Create dataframe for plot data
plotData <- data.frame(matrix(c(1:11, predicted), nrow = 11, ncol = 2))
names(plotData) <- c("x", "y")

## Check data
plotData

## Use ggplot2 to plot
testPlot <- ggplot(data = plotData, aes(x = x, y = y)) +
    geom_line()

testPlot


################################################################################
## CLPM
################################################################################

## Get Data
dataSelect <- data %>%
  select(starts_with("PositiveEmotion"),
         starts_with("health"))
names(dataSelect) <- c(paste0("x", 1:11),
                       paste0("y", 1:11))

clpm11_c <- '
## Regressions
##
## Wave 2
x2 ~ a*x1
y2 ~ b*y1
x2 ~ c*y1
y2 ~ d*x1
## Wave 3
x3 ~ a*x2
y3 ~ b*y2
x3 ~ c*y2
y3 ~ d*x2
## Wave 4
x4 ~ a*x3
y4 ~ b*y3
x4 ~ c*y3
y4 ~ d*x3
## Wave 5
x5 ~ a*x4
y5 ~ b*y4
x5 ~ c*y4
y5 ~ d*x4
## Wave 6
x6 ~ a*x5
y6 ~ b*y5
x6 ~ c*y5
y6 ~ d*x5
## Wave 7
x7 ~ a*x6
y7 ~ b*y6
x7 ~ c*y6
y7 ~ d*x6
## Wave 8
x8 ~ a*x7
y8 ~ b*y7
x8 ~ c*y7
y8 ~ d*x7
## Wave 9
x9 ~ a*x8
y9 ~ b*y8
x9 ~ c*y8
y9 ~ d*x8
## Wave 10
x10 ~ a*x9
y10 ~ b*y9
x10 ~ c*y9
y10 ~ d*x9
## Wave 11
x11 ~ a*x10
y11 ~ b*y10
x11 ~ c*y10
y11 ~ d*x10
##
## Variances
x1 ~~ x1
y1 ~~ y1
x2 ~~ x2
y2 ~~ y2
x3 ~~ x3
y3 ~~ y3
x4 ~~ x4
y4 ~~ y4
x5 ~~ x5
y5 ~~ y5
x6 ~~ x6
y6 ~~ y6
x7 ~~ x7
y7 ~~ y7
x8 ~~ x8
y8 ~~ y8
x9 ~~ x9
y9 ~~ y9
x10 ~~ x10
y10 ~~ y10
x11 ~~ x11
y11 ~~ y11
##
## Covariances
x1 ~~ y1
x2 ~~ y2
x3 ~~ y3
x4 ~~ y4
x5 ~~ y5
x6 ~~ y6
x7 ~~ y7
x8 ~~ y8
x9 ~~ y9
x10 ~~ y10
x11 ~~ y11
'

clpm_fit <- sem(clpm11_c, data=dataSelect, estimator = "MLR", missing = "fiml")
summary(clpm_fit)

standardizedSolution(clpm_fit)
fitMeasures(clpm_fit)


################################################################################
## RI-CLPM
################################################################################

ri_clpm11_c <- '
## Random Intercepts
##
ri_x =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8 + 1*x9 + 1*x10 + 1*x11
ri_y =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6 + 1*y7 + 1*y8 + 1*y9 + 1*y10 + 1*y11
##
## Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wx6 =~ 1*x6
  wx7 =~ 1*x7
  wx8 =~ 1*x8 
  wx9 =~ 1*x9
  wx10 =~ 1*x10
  wx11 =~ 1*x11
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  wy6 =~ 1*y6
  wy7 =~ 1*y7
  wy8 =~ 1*y8
  wy9 =~ 1*y9
  wy10 =~ 1*y10
  wy11 =~ 1*y11
##
## Regressions
##
## Stabilities #the straight AR arrow
wx2 ~ a*wx1
wx3 ~ a*wx2
wx4 ~ a*wx3
wx5 ~ a*wx4
wx6 ~ a*wx5
wx7 ~ a*wx6
wx8 ~ a*wx7
wx9 ~ a*wx8
wx10 ~ a*wx9
wx11 ~ a*wx10
wy2 ~ b*wy1
wy3 ~ b*wy2
wy4 ~ b*wy3
wy5 ~ b*wy4
wy6 ~ b*wy5
wy7 ~ b*wy6
wy8 ~ b*wy7
wy9 ~ b*wy8
wy10 ~ b*wy9
wy11 ~ b*wy10
##
##
## Cross-lags #the diagonal AR arrow
wy2 ~ c*wx1
wy3 ~ c*wx2
wy4 ~ c*wx3
wy5 ~ c*wx4
wy6 ~ c*wx5
wy7 ~ c*wx6
wy8 ~ c*wx7
wy9 ~ c*wx8
wy10 ~ c*wx9
wy11 ~ c*wx10
wx2 ~ d*wy1
wx3 ~ d*wy2
wx4 ~ d*wy3
wx5 ~ d*wy4
wx6 ~ d*wy5
wx7 ~ d*wy6
wx8 ~ d*wy7
wx9 ~ d*wy8
wx10 ~ d*wy9
wx11 ~ d*wy10
##
## Variances
ri_x ~~ ri_x
ri_y ~~ ri_y
wx1 ~~ wx1
wy1 ~~ wy1
wx2 ~~ wx2
wy2 ~~ wy2
wx3 ~~ wx3
wy3 ~~ wy3
wx4 ~~ wx4
wy4 ~~ wy4
wx5 ~~ wx5
wy5 ~~ wy5
wx6 ~~ wx6
wy6 ~~ wy6
wx7 ~~ wx7
wy7 ~~ wy7
wx8 ~~ wx8
wy8 ~~ wy8
wx9 ~~ wx9
wy9 ~~ wy9
wx10 ~~ wx10
wy10 ~~ wy10
wx11 ~~ wx11
wy11 ~~ wy11
##
## Covariances
ri_x ~~ ri_y
wx1 ~~ wy1
wx2 ~~ wy2
wx3 ~~ wy3
wx4 ~~ wy4
wx5 ~~ wy5
wx6 ~~ wy6
wx7 ~~ wy7
wx8 ~~ wy8
wx9 ~~ wy9
wx10 ~~ wy10
wx11 ~~ wy11
'

riclpmFit <- sem(ri_clpm11_c, data=dataSelect, estimator = "MLR", missing = "fiml")

summary(riclpmFit)

standardizedSolution(clpm_fit)
fitMeasures(clpm_fit)


################################################################################
## Test Univariate STARTS
################################################################################

starts <- buildStarts1(11)

starts1Fit <- lavaan(starts, dataSelect, estimator="MLR", missing="fiml")
summary(starts1Fit)
