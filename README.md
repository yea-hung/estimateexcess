# Description

R functions for estimating excess mortality, using ARIMA models. 

# Dependencies

- *forecast*: Used for the ARIMA models.
- *ggplot2*: Used to plot the results.

# Functions

- `estimate_monthly_excess()`: Function for estimating monthly excess.
- `estimate_weekly_excess()`: Function for estimating weekly excess.

# Arguments

- `yy`: The name of the variable of interest, in `data`.
- `forecast.window`: The length of the forecast window. This is the total length of the time period of interest. In other words, if performing analysis for pandemic-related excess mortality, the `forecast.window` is the duration of pandemic time of interest.
- `forecast.start`: The start date of the forecast.
- `forecast.periods`: Optional. A vector that has unique values for each time period of interest. The length of the vector should be equal to `forecast.window`.
- `data.start`: The start date of the data, in a vector form. Consult the default value and the documentation for `ts()` for further information.
- `data`: The `data` object should be a `data.frame`, with a `Date` variable named `month` (if using monthly data) or `week` (if using weekly data). In the latter scenario, the expectation is that `week` is the date of the last day of the week (a Saturday, using US convention). The other variables in `data` should each represent the total number of deaths within some group of interest. 

# Value

- `results.by.month` or `results.by.week`: The date-specific results.
- `results`: The overall results, with summation over the entire time period of interest.
- `simulations`: Simulated sums. These can be used to obtain a prediction interval for the sum over the time period of interest. The bounds in `results` (above) are derived from these simulations. However, the entirety of the simulations may be useful in some cases.
- `plot`: A visualization of the results. 

# Use

First, load your `data` object:

```r
dd<-readRDS('weekly data.rds')
```

We have named the object `dd` since this is the default value of `data` in the estimation functions.

To estimate excess mortality for the variable `dpw.Inland Empire`:

```r
rr<-estimate_weekly_excess('dpw.Inland Empire')
```

To view the plot:

```r
rr$plot
```

A possible framework for analyzing multiple variables is as follows:

```r
r1<-estimate_weekly_excess('dpw.Los Angeles County')
r2<-estimate_weekly_excess('dpw.San Francisco Bay Area')
r3<-estimate_weekly_excess('dpw.Inland Empire')
RR<-rbind(r1$results,r2$results,r3$results)
```

This stacks all of the `results` from each analysis into a single `data.frame`, named `RR`.

# Models

The weekly models use an approach described [here](https://otexts.com/fpp2/complexseasonality.html) and [here](https://robjhyndman.com/hyndsight/forecasting-weekly-data/).
