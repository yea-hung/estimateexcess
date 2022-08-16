# Description

R code for estimating excess mortality, using ARIMA models. 

# Functions

- `estimate_weekly_excess()`: Function for estimating weekly excess.
- `estimate_monthly_excess()`: Function for estimating monthly excess.

# Arguments

- `yy`: The name of the variable of interest, in `data`.
- `forecast.window`: The length of the forecast window. This is the total length of the time period of interest. In other words, if performing analysis for pandemic-related excess mortality, the `forecast.window` is the duration of pandemic time of interest.
- `forecast.start`: The start date of the forecast.
- `data`: The `data` object should be a `data frame`, with a variable named `date` that is in `Date` format. If the data are aggregated by week, the expectation is that `date` is the date of the last day of the week (a Saturday, using American convention). The other variables in `data` should each represent the total number of deaths within some group of interest. These might optionally share a common name prefix, referred to as a `stub` in the code (see below). For example, the variables might be named: `dpw.California`, `dpw.Oregon`, and `dpw.Washington`.
- `stub`: This argument is optional. The only purpose for specifying the `stub` is to remove it from the plot's title.

# Value

- `results.by.date`: The date-specific results.
- `results`: The overall results, with summation over the entire time period of interest.
- `plot`: A visualization of the results.

# Use

First, load your `data` object:

```r
DD<-readRDS('weekly data.rds')
```

We have named the object `DD` since this is the default value of `data` in the estimation functions.

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
RR<-rbind(
  estimate_weekly_excess('dpw.Los Angeles County')$results
  estimate_weekly_excess('dpw.San Francisco Bay Area')$results,
  estimate_weekly_excess('dpw.Inland Empire')$results
)
```

This stacks all of the `results` from each analysis into a single `data frame`, named `RR`.