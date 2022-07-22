# Description

This code estimates excess mortality, using ARIMA models.

# `data`

The `data` object should have a variable named `date` that is in `Date` format. If the data are aggregated by week, the expectation is that `date` is the date of the last day of the week (a Saturday, using American convention).

The `data` object should also include other variables that represent the total number of deaths. These non-date variables may optionally have names that begin with a shared prefix; this prefix is known as the `stub` in the code. By default, the `stub` is `dpm.` for monthly data and `dpw.` for weekly data. 

# `mapping`

The `mapping` object should store variable-group mappings. It should have a variable named `variable` and a variable named `group`. The primary purposes of the `mapping` object are to (1) facilitate looping of analysis, and (2) add in a variable indicating variable in the summary data frame (see below). If that variable is not desired, you may comment out the line beginning `variable=` inside the definition of the `delta` data frame.

# Use

To call the function:

```r
estimate_weekly_excess('dpw.Northern California')
```

The function will create a folder named `generated data` within the working directory. Each call of the function will add a group-specific results file here containing estimates for each time unit (ie, if conducting weekly analysis, the results file will include estimates for each week).

Estimates for the entire time period are stored in `RR`. A possible framework is thus as follows:

```r
estimate_weekly_excess('dpw.Northern California')
estimate_weekly_excess('dpw.Southern California')
estimate_weekly_excess('dpw.Inland Empire')
saveRDS(RR,'results.rds')
```
