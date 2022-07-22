# Description

This code estimates excess mortality, using ARIMA models.

# `data`

The `data` object should have a variable named `date` that is in `Date` format. If the data are aggregated by week, the expectation is that `date` is the date of the last day of the week (a Saturday, using American convention).

The `data` object should also include other variables that represent the total number of deaths. These non-date variables may optionally have names that begin with a shared prefix; this prefix is known as the `stub` in the code. By default, the `stub` is `dpm.` for monthly data and `dpw.` for weekly data. 

# `mapping`

The `mapping` object should store variable-group mappings. It should have a variable named `variable` and a variable named `group`. The primary purposes of the `mapping` object are to (1) facilitate looping of analysis, and (2) add in a variable indicating variable in the summary data frame (see below). If that variable is not desired, you may comment out the line beginning `variable=` inside the definition of the `delta` data frame.

# Use


First, load your `data` object and `mapping` object:

```r
DD<-readRDS('weekly data.rds')
MM<-readRDS('mapping for weekly data.rds)
```

To call the function:

```r
estimate_weekly_excess('dpw.Northern California')
```

Estimates for the entire time period are stored in `RR`. A possible framework is thus as follows:

```r
estimate_weekly_excess('dpw.Los Angeles County')
estimate_weekly_excess('dpw.San Francisco Bay Area')
estimate_weekly_excess('dpw.Inland Empire')
saveRDS(RR,'results.rds')
```

The function will also store time-specific results, containing estimates for excess mortality for each time unit. These will be added as `.rds` files to a folder named `time-specific results` within the working directory. Each call of the function will result in a file being added to this folder. For example, the first call above will result in a file being added called `weekly Los Angeles County.rds` containing estimates for excess mortality for each week of analysis.