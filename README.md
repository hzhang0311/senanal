# senanal

## Installation

```r
# install.packages("devtools")
devtools::install_github("hzhang0311/senanal")
```

## Usage

### I. Senanal

The funtion `senanal` requires two inputs:

1) *data*: A data frame that contains 4 columns each contains parameters, base values, lower bound and upper bound (must follow this order).
2) *unit*: A string describing the unit that this analysis is using, for example, "2022 USD".

This function outputs a list including a data frame of analysis results ($outcome) and a tornado disgram ($tornado). It is recommendded for users to first store the outputs in a variable and then call the specific result they need.

```r
mydata = data.frame(cbind("Parameter" = c("Faculty", "Student", "Doctor"),
                          "Base" = c(2000, 7200, 3000),
                          "Low" = c(1000,6000,2000),
                          "High" = c(3000, 9000, 5000)))
res = senanal(data = mydata, unit = "USD")
sen_output = res$outcome
res$tornado
```
