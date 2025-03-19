# Summary

An R package for estimating excess mortality, using seasonal ARIMA models. 

# Installation

Install *devtools* if you don't already have it:

```r
install.packages('devtools')
```

Install the *EstimateExcessMortality* package:

```r
devtools::install_github('yea-hung/EstimateExcessMortality')
```

# Use

Load the package:

```r
library('EstimateExcessMortality')
```

Import your data:

```r
weekly_data<-readRDS('weekly data.rds')
```

To estimate excess mortality for the variable `Inland Empire`:

```r
rr<-estimate_weekly_excess('Inland Empire',weekly_data)
```

To view the plot:

```r
rr$plot
```

A possible framework for analyzing multiple variables is as follows:

```r
r1<-estimate_weekly_excess('Los Angeles County',weekly_data)
r2<-estimate_weekly_excess('San Francisco Bay Area',weekly_data)
r3<-estimate_weekly_excess('Inland Empire',weekly_data)
results<-rbind(r1$results,r2$results,r3$results)
```

This stacks all of the `results` from each analysis into a single data frame, named `results`.

# Methodological details

The weekly models use an approach described [here](https://otexts.com/fpp2/complexseasonality.html) and [here](https://robjhyndman.com/hyndsight/forecasting-weekly-data/).
