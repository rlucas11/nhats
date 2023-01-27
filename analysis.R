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
## Read script for generating model
################################################################################

source("~/Projects/code-generator/buildModel.R")
source("~/Projects/starts/scripts/usefulFunctions.R")

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

test <- buildModel(11, trait=FALSE)
testFit <- lavaan(test, data=dataSelect, estimator = "MLR", missing="fiml")
summary(testFit)

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

ri_clpm11_u <- '
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
wx2 ~ wx1
wx3 ~ wx2
wx4 ~ wx3
wx5 ~ wx4
wx6 ~ wx5
wx7 ~ wx6
wx8 ~ wx7
wx9 ~ wx8
wx10 ~ wx9
wx11 ~ wx10
wy2 ~ wy1
wy3 ~ wy2
wy4 ~ wy3
wy5 ~ wy4
wy6 ~ wy5
wy7 ~ wy6
wy8 ~ wy7
wy9 ~ wy8
wy10 ~ wy9
wy11 ~ wy10
##
##
## Cross-lags #the diagonal AR arrow
wy2 ~ wx1
wy3 ~ wx2
wy4 ~ wx3
wy5 ~ wx4
wy6 ~ wx5
wy7 ~ wx6
wy8 ~ wx7
wy9 ~ wx8
wy10 ~ wx9
wy11 ~ wx10
wx2 ~ wy1
wx3 ~ wy2
wx4 ~ wy3
wx5 ~ wy4
wx6 ~ wy5
wx7 ~ wy6
wx8 ~ wy7
wx9 ~ wy8
wx10 ~ wy9
wx11 ~ wy10
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
riclpm_u_Fit <- sem(ri_clpm11_u, data=dataSelect, estimator = "MLR", missing = "fiml")

summary(riclpm_u_Fit)

standardizedSolution(clpm_fit)
fitMeasures(clpm_fit)


################################################################################
## Test Univariate STARTS
################################################################################

## Sometimes it helps to use a generic model and then rename vars each time
dataSelect <- data %>%
    select(starts_with("Purpose"),
           starts_with("recall"))

names(dataSelect) <- c(paste0("x", 1:11),
                       paste0("y", 1:11))


startsX <- buildStartsX(11)
startsY <- buildStartsY(11)

startsFitX <- lavaan(startsX, dataSelect, estimator="MLR", missing="fiml")
summary(startsFit)

startsFitY <- lavaan(startsY, dataSelect, estimator="MLR", missing="fiml")
summary(startsFit)

library(STARTS)
test <- starts_uni_estimate(data=dataSelect[,1:11])

clpmFit <- lavaan(clpm_c, data)


lag2clpm <- "
### Model Code Generated by buildModel.R 
### Observed Variables for X 

## Residual Variance Constrained to 0
x1 ~~ 0*x1
x2 ~~ 0*x2
x3 ~~ 0*x3
x4 ~~ 0*x4
x5 ~~ 0*x5
x6 ~~ 0*x6
x7 ~~ 0*x7
x8 ~~ 0*x8
x9 ~~ 0*x9
x10 ~~ 0*x10
x11 ~~ 0*x11
 
### Observed Variables for Y 

## Residual Variance Constrained to 0
y1 ~~ 0*y1
y2 ~~ 0*y2
y3 ~~ 0*y3
y4 ~~ 0*y4
y5 ~~ 0*y5
y6 ~~ 0*y6
y7 ~~ 0*y7
y8 ~~ 0*y8
y9 ~~ 0*y9
y10 ~~ 0*y10
y11 ~~ 0*y11
 
 
### Autoregressive Part for X;
## Indicator Statements;
arx1 =~ 1*x1 
arx2 =~ 1*x2 
arx3 =~ 1*x3 
arx4 =~ 1*x4 
arx5 =~ 1*x5 
arx6 =~ 1*x6 
arx7 =~ 1*x7 
arx8 =~ 1*x8 
arx9 =~ 1*x9 
arx10 =~ 1*x10 
arx11 =~ 1*x11 

## Regression Statements
arx2 ~ arx1 
arx3 ~ a*arx2 
arx4 ~ a*arx3 
arx5 ~ a*arx4 
arx6 ~ a*arx5 
arx7 ~ a*arx6 
arx8 ~ a*arx7 
arx9 ~ a*arx8 
arx10 ~ a*arx9 
arx11 ~ a*arx10 

## Lag-2 Regression Statements
arx3 ~ aa*arx1 
arx4 ~ aa*arx2 
arx5 ~ aa*arx3 
arx6 ~ aa*arx4 
arx7 ~ aa*arx5 
arx8 ~ aa*arx6 
arx9 ~ aa*arx7 
arx10 ~ aa*arx8 
arx11 ~ aa*arx9 


## Autoregressive Component Variance 
arx1 ~~ arvx*arx1 
arx2 ~~ arv2x*arx2 
arx3 ~~ arv2x*arx3 
arx4 ~~ arv2x*arx4 
arx5 ~~ arv2x*arx5 
arx6 ~~ arv2x*arx6 
arx7 ~~ arv2x*arx7 
arx8 ~~ arv2x*arx8 
arx9 ~~ arv2x*arx9 
arx10 ~~ arv2x*arx10 
arx11 ~~ arv2x*arx11 
 
### Autoregressive Part for Y;
## Indicator Statements;
ary1 =~ 1*y1 
ary2 =~ 1*y2 
ary3 =~ 1*y3 
ary4 =~ 1*y4 
ary5 =~ 1*y5 
ary6 =~ 1*y6 
ary7 =~ 1*y7 
ary8 =~ 1*y8 
ary9 =~ 1*y9 
ary10 =~ 1*y10 
ary11 =~ 1*y11 

## Regression Statements
ary2 ~ ary1 
ary3 ~ b*ary2 
ary4 ~ b*ary3 
ary5 ~ b*ary4 
ary6 ~ b*ary5 
ary7 ~ b*ary6 
ary8 ~ b*ary7 
ary9 ~ b*ary8 
ary10 ~ b*ary9 
ary11 ~ b*ary10 

## Lag-2 Regression Statements
ary3 ~ bb*ary1
ary4 ~ bb*ary2 
ary5 ~ bb*ary3 
ary6 ~ bb*ary4 
ary7 ~ bb*ary5 
ary8 ~ bb*ary6 
ary9 ~ bb*ary7 
ary10 ~ bb*arx8 
ary11 ~ bb*ary9 


## Autoregressive Component Variance 
ary1 ~~ arvy*ary1 
ary2 ~~ arv2y*ary2 
ary3 ~~ arv2y*ary3 
ary4 ~~ arv2y*ary4 
ary5 ~~ arv2y*ary5 
ary6 ~~ arv2y*ary6 
ary7 ~~ arv2y*ary7 
ary8 ~~ arv2y*ary8 
ary9 ~~ arv2y*ary9 
ary10 ~~ arv2y*ary10 
ary11 ~~ arv2y*ary11 
 
### Cross-Lagged Path 
## Y predicted from X 
ary2 ~ c*arx1 
ary3 ~ c*arx2 
ary4 ~ c*arx3 
ary5 ~ c*arx4 
ary6 ~ c*arx5 
ary7 ~ c*arx6 
ary8 ~ c*arx7 
ary9 ~ c*arx8 
ary10 ~ c*arx9 
ary11 ~ c*arx10 
 
### Cross-Lagged Path 
## X predicted from Y 
arx2 ~ d*ary1 
arx3 ~ d*ary2 
arx4 ~ d*ary3 
arx5 ~ d*ary4 
arx6 ~ d*ary5 
arx7 ~ d*ary6 
arx8 ~ d*ary7 
arx9 ~ d*ary8 
arx10 ~ d*ary9 
arx11 ~ d*ary10 
 
## AR Correlations 
arx1 ~~ cor_xy*ary1 
arx2 ~~ cor_xyr*ary2 
arx3 ~~ cor_xyr*ary3 
arx4 ~~ cor_xyr*ary4 
arx5 ~~ cor_xyr*ary5 
arx6 ~~ cor_xyr*ary6 
arx7 ~~ cor_xyr*ary7 
arx8 ~~ cor_xyr*ary8 
arx9 ~~ cor_xyr*ary9 
arx10 ~~ cor_xyr*ary10 
arx11 ~~ cor_xyr*ary11
"

lag2 <- "
### Model Code Generated by buildModel.R 
### Observed Variables for X 

## Residual Variance Constrained to 0
x1 ~~ 0*x1
x2 ~~ 0*x2
x3 ~~ 0*x3
x4 ~~ 0*x4
x5 ~~ 0*x5
x6 ~~ 0*x6
x7 ~~ 0*x7
x8 ~~ 0*x8
x9 ~~ 0*x9
x10 ~~ 0*x10
x11 ~~ 0*x11
 
### Observed Variables for Y 

## Residual Variance Constrained to 0
y1 ~~ 0*y1
y2 ~~ 0*y2
y3 ~~ 0*y3
y4 ~~ 0*y4
y5 ~~ 0*y5
y6 ~~ 0*y6
y7 ~~ 0*y7
y8 ~~ 0*y8
y9 ~~ 0*y9
y10 ~~ 0*y10
y11 ~~ 0*y11
 

### X States;
sx1 =~ 1*x1 
sx2 =~ 1*x2 
sx3 =~ 1*x3 
sx4 =~ 1*x4 
sx5 =~ 1*x5 
sx6 =~ 1*x6 
sx7 =~ 1*x7 
sx8 =~ 1*x8 
sx9 =~ 1*x9 
sx10 =~ 1*x10 
sx11 =~ 1*x11 

## State Variance;
sx1 ~~ sx*sx1 
sx2 ~~ sx*sx2 
sx3 ~~ sx*sx3 
sx4 ~~ sx*sx4 
sx5 ~~ sx*sx5 
sx6 ~~ sx*sx6 
sx7 ~~ sx*sx7 
sx8 ~~ sx*sx8 
sx9 ~~ sx*sx9 
sx10 ~~ sx*sx10 
sx11 ~~ sx*sx11 
 

### Y States;
sy1 =~ 1*y1 
sy2 =~ 1*y2 
sy3 =~ 1*y3 
sy4 =~ 1*y4 
sy5 =~ 1*y5 
sy6 =~ 1*y6 
sy7 =~ 1*y7 
sy8 =~ 1*y8 
sy9 =~ 1*y9 
sy10 =~ 1*y10 
sy11 =~ 1*y11 

## State Variance;
sy1 ~~ sy*sy1 
sy2 ~~ sy*sy2 
sy3 ~~ sy*sy3 
sy4 ~~ sy*sy4 
sy5 ~~ sy*sy5 
sy6 ~~ sy*sy6 
sy7 ~~ sy*sy7 
sy8 ~~ sy*sy8 
sy9 ~~ sy*sy9 
sy10 ~~ sy*sy10 
sy11 ~~ sy*sy11 
 
### Autoregressive Part for X;
## Indicator Statements;
arx1 =~ 1*x1 
arx2 =~ 1*x2 
arx3 =~ 1*x3 
arx4 =~ 1*x4 
arx5 =~ 1*x5 
arx6 =~ 1*x6 
arx7 =~ 1*x7 
arx8 =~ 1*x8 
arx9 =~ 1*x9 
arx10 =~ 1*x10 
arx11 =~ 1*x11 

## Regression Statements
arx2 ~ arx1 
arx3 ~ a*arx2 
arx4 ~ a*arx3 
arx5 ~ a*arx4 
arx6 ~ a*arx5 
arx7 ~ a*arx6 
arx8 ~ a*arx7 
arx9 ~ a*arx8 
arx10 ~ a*arx9 
arx11 ~ a*arx10 

## Lag-2 Regression Statements
arx3 ~ aa*arx1 
arx4 ~ aa*arx2 
arx5 ~ aa*arx3 
arx6 ~ aa*arx4 
arx7 ~ aa*arx5 
arx8 ~ aa*arx6 
arx9 ~ aa*arx7 
arx10 ~ aa*arx8 
arx11 ~ aa*arx9 


## Autoregressive Component Variance 
arx1 ~~ arvx*arx1 
arx2 ~~ arv2x*arx2 
arx3 ~~ arv2x*arx3 
arx4 ~~ arv2x*arx4 
arx5 ~~ arv2x*arx5 
arx6 ~~ arv2x*arx6 
arx7 ~~ arv2x*arx7 
arx8 ~~ arv2x*arx8 
arx9 ~~ arv2x*arx9 
arx10 ~~ arv2x*arx10 
arx11 ~~ arv2x*arx11 
 
### Autoregressive Part for Y;
## Indicator Statements;
ary1 =~ 1*y1 
ary2 =~ 1*y2 
ary3 =~ 1*y3 
ary4 =~ 1*y4 
ary5 =~ 1*y5 
ary6 =~ 1*y6 
ary7 =~ 1*y7 
ary8 =~ 1*y8 
ary9 =~ 1*y9 
ary10 =~ 1*y10 
ary11 =~ 1*y11 

## Regression Statements
ary2 ~ ary1 
ary3 ~ b*ary2 
ary4 ~ b*ary3 
ary5 ~ b*ary4 
ary6 ~ b*ary5 
ary7 ~ b*ary6 
ary8 ~ b*ary7 
ary9 ~ b*ary8 
ary10 ~ b*ary9 
ary11 ~ b*ary10 

## Lag-2 Regression Statements
ary3 ~ bb*ary1
ary4 ~ bb*ary2 
ary5 ~ bb*ary3 
ary6 ~ bb*ary4 
ary7 ~ bb*ary5 
ary8 ~ bb*ary6 
ary9 ~ bb*ary7 
ary10 ~ bb*arx8 
ary11 ~ bb*ary9 


## Autoregressive Component Variance 
ary1 ~~ arvy*ary1 
ary2 ~~ arv2y*ary2 
ary3 ~~ arv2y*ary3 
ary4 ~~ arv2y*ary4 
ary5 ~~ arv2y*ary5 
ary6 ~~ arv2y*ary6 
ary7 ~~ arv2y*ary7 
ary8 ~~ arv2y*ary8 
ary9 ~~ arv2y*ary9 
ary10 ~~ arv2y*ary10 
ary11 ~~ arv2y*ary11 
 
### Cross-Lagged Path 
## Y predicted from X 
ary2 ~ c*arx1 
ary3 ~ c*arx2 
ary4 ~ c*arx3 
ary5 ~ c*arx4 
ary6 ~ c*arx5 
ary7 ~ c*arx6 
ary8 ~ c*arx7 
ary9 ~ c*arx8 
ary10 ~ c*arx9 
ary11 ~ c*arx10 
 
### Cross-Lagged Path 
## X predicted from Y 
arx2 ~ d*ary1 
arx3 ~ d*ary2 
arx4 ~ d*ary3 
arx5 ~ d*ary4 
arx6 ~ d*ary5 
arx7 ~ d*ary6 
arx8 ~ d*ary7 
arx9 ~ d*ary8 
arx10 ~ d*ary9 
arx11 ~ d*ary10 
 
## AR Correlations 
arx1 ~~ cor_xy*ary1 
arx2 ~~ cor_xyr*ary2 
arx3 ~~ cor_xyr*ary3 
arx4 ~~ cor_xyr*ary4 
arx5 ~~ cor_xyr*ary5 
arx6 ~~ cor_xyr*ary6 
arx7 ~~ cor_xyr*ary7 
arx8 ~~ cor_xyr*ary8 
arx9 ~~ cor_xyr*ary9 
arx10 ~~ cor_xyr*ary10 
arx11 ~~ cor_xyr*ary11
"


lag2Fit <- lavaan(lag2clpm, dataSelect, estimator="MLR", missing="fiml")
summary(lag2Fit)
