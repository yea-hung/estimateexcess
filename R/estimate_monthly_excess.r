#' Estimate excess mortality using monthly data
#'
#' @param outcome_variable The name of the outcome variable, in quotes.
#' @param data A data frame of the aggregated data. It should have a `Date` variable named `month` and at least one variable that represents the number of deaths per month within some population (see `outcome_variable` above).
#' @param forecast_window The number of months in the forecast window.
#' @param forecast_start The start date of the forecast window, in Date format.
#' @param forecast_periods Time periods (optional) within the forecast period. This should be specified as a numeric vector that has a length matching the forecast period. The values of the period should identify the periods. For example: `c(rep(1,5),rep(2,5),rep(3,6))`. If validly specified, the function will return results for each time period, in addition to results for the entire forecast window.
#' @param data_start The start date of the data, specified as a two-element numeric vector. See `ts()`.
#'
#' @returns A list with five elements:
#' - `results_by_month`: The monthly results.
#' - `results`: The overall results (that is, over the forecast window).
#' - `simulations`: Simulated sums, used to obtain the prediction intervals in `results` (above).
#' - `plot`: A visualization of the results.
#' - `plot_data`: The data underlying the plot.
#' @export
#'
#' @examples
#' rr<-estimate_monthly_excess('Inland Empire',monthly_data)
#' rr$results
estimate_monthly_excess<-function(outcome_variable,
                                  data,
                                  forecast_window=14,
                                  forecast_start=as.Date('2020-03-01'),
                                  forecast_periods=NULL,
                                  data_start=c(2016,1)
){

  # sort data
  data<-data[order(data$month),]

  # define data
  tt<-ts(data[data$month<forecast_start,outcome_variable],
         frequency=12,start=data_start)

  # fit model
  mm<-forecast::auto.arima(tt)

  # obtain forecasts
  ff<-forecast::forecast(mm,h=forecast_window)

  # extract observed values
  rr<-data$month[(data$month>=forecast_start)]
  rr<-rr[1:forecast_window]
  oo<-data[is.element(data$month,rr),c('month',outcome_variable)]
  names(oo)[2]<-'observed'

  # extract expected values
  ee<-data.frame(
    month=rr,
    expected=as.numeric(ff$mean),
    expected_lower=as.numeric(ff$lower[,'95%']),
    expected_upper=as.numeric(ff$upper[,'95%'])
  )

  # define month-specific results
  MM<-merge(oo,ee,by='month',all.x=FALSE,all.y=FALSE)
  MM$excess<-MM$observed-MM$expected
  MM$excess_lower<-MM$observed-MM$expected_upper
  MM$excess_upper<-MM$observed-MM$expected_lower

  # obtain prediction intervals for totals
  set.seed(94158)
  NN<-10000
  SS<-NULL
  for(ii in 1:NN){
    sim_i<-simulate(mm,future=TRUE,nsim=forecast_window)
    SS_i<-data.frame(pt=sum(sim_i))
    if(!is.null(forecast_periods)){
      for(pp in unique(forecast_periods)){
        SS_i[,paste('p',pp,sep='')]<-sum(sim_i[which(forecast_periods==pp)])
      }
    }
    SS<-rbind(SS,SS_i)
  }

  # define overall results
  RR<-data.frame(
    group=outcome_variable,
    observed=sum(MM$observed),
    expected=sum(MM$expected),
    expected_mean=mean(SS$pt),
    expected_lower=as.numeric(quantile(SS$pt,c(0.025))),
    expected_upper=as.numeric(quantile(SS$pt,c(0.975))),
    expected_se=sd(SS$pt)*sqrt(NN-1)/sqrt(NN)
  )
  RR$excess<-RR$observed-RR$expected
  RR$excess_mean<-RR$observed-RR$expected_mean
  RR$excess_lower<-RR$observed-RR$expected_upper
  RR$excess_upper<-RR$observed-RR$expected_lower
  RR$excess_se<-sd(RR$observed-SS$pt)*sqrt(NN-1)/sqrt(NN)

  # define period-specific results
  if(!is.null(forecast_periods)){
    for(period in unique(forecast_periods)){
      # define indices
      MM_p<-which(forecast_periods==period)
      SS_p<-paste('p',period,sep='')
      # define values
      observed<-sum(MM$observed[MM_p])
      expected<-sum(MM$expected[MM_p])
      expected_mean<-mean(SS[,SS_p])
      expected_lower<-quantile(SS[,SS_p],0.025)
      expected_upper<-quantile(SS[,SS_p],0.975)
      excess<-observed-expected
      excess_mean<-observed-expected_mean
      excess_lower<-observed-expected_upper
      excess_upper<-observed-expected_lower
      # add values to data frame
      RR[,paste('observed',period,sep='_')]<-observed
      RR[,paste('expected',period,sep='_')]<-expected
      RR[,paste('expected_mean',period,sep='_')]<-expected_mean
      RR[,paste('expected_lower',period,sep='_')]<-expected_lower
      RR[,paste('expected_upper',period,sep='_')]<-expected_upper
      RR[,paste('excess',period,sep='_')]<-excess
      RR[,paste('excess_mean',period,sep='_')]<-excess_mean
      RR[,paste('excess_lower',period,sep='_')]<-excess_lower
      RR[,paste('excess_upper',period,sep='_')]<-excess_upper
    }
  }

  # define x-axis breaks
  x_minor<-unique(substr(data$month,1,7))
  x_minor<-paste(x_minor,'01',sep='-')
  x_minor<-as.Date(x_minor,'%Y-%m-%d')
  x_major<-x_minor[seq(1,length(x_minor),12)]

  # define data for plot
  pandemic<-MM[,c('month','observed','expected',
                  'expected_lower','expected_upper')]
  prior<-data[data$month<forecast_start,c('month',outcome_variable)]
  names(prior)[2]<-'observed'
  prior$expected<-as.numeric(ff$fitted)
  prior$expected_lower<-NA
  prior$expected_upper<-NA
  PP_DD<-rbind(prior,pandemic)

  # define plot
  PP<-ggplot2::ggplot(ggplot2::aes(x=month,y=observed),data=PP_DD)+
    ggplot2::geom_ribbon(
      ggplot2::aes(x=month,y=expected,ymin=expected_lower,
          ymax=expected_upper),
      data=subset(PP_DD,month>=forecast_start),
      alpha=0.2,fill='#00BFC4'
    )+
    ggplot2::geom_line(ggplot2::aes(x=month,y=observed),color='#F8766D')+
    ggplot2::geom_line(ggplot2::aes(x=month,y=expected),color='#00BFC4')+
    ggplot2::scale_x_date(date_labels='%Y-%m',
                          breaks=x_major,minor_breaks=x_minor)+
    ggplot2::scale_y_continuous(labels=scales::comma)+
    ggplot2::labs(x='',y='Deaths per month')+
    ggplot2::theme_bw()

  # return results
  list(
    results_by_month=MM,
    results=RR,
    simulations=SS,
    plot=PP,
    plot_data=PP_DD
  )

}
