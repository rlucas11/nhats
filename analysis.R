################################################################################
## Load Packages
################################################################################
library(tidyverse)
library(lavaan)

################################################################################
## Read Data
################################################################################
data <- read_csv("data/nhatsCleaned.csv")

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
