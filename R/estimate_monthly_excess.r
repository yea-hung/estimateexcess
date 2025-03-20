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

  # store results
  RR<-data.frame(
    group=outcome_variable,
    observed=sum(MM$observed),
    expected=mean(SS$pt),
    expected_alternate=sum(MM$expected),
    expected_lower=as.numeric(quantile(SS$pt,0.025)),
    expected_upper=as.numeric(quantile(SS$pt,0.975)),
    excess=sum(MM$observed)-mean(SS$pt),
    excess_alternate=sum(MM$observed-MM$expected),
    excess_lower=sum(MM$observed)-as.numeric(quantile(SS$pt,0.975)),
    excess_upper=sum(MM$observed)-as.numeric(quantile(SS$pt,0.025))
  )
  if(!is.null(forecast_periods)){
    for(period in unique(forecast_periods)){
      MM_i<-which(forecast_periods==period)
      SS_i<-paste('p',period,sep='')
      ss<-period
      RR[,paste('observed',ss,sep='_')]<-sum(MM$observed[MM_i])
      RR[,paste('expected',ss,sep='_')]<-mean(SS[,SS_i])
      RR[,paste('expected_alternate',ss,sep='_')]<-sum(MM$expected[MM_i])
      RR[,paste('expected_lower',ss,sep='_')]<-quantile(SS[,SS_i],0.025)
      RR[,paste('expected_upper',ss,sep='_')]<-quantile(SS[,SS_i],0.975)
      RR[,paste('excess',ss,sep='_')]<-sum(MM$observed[MM_i])-mean(SS[,SS_i])
      RR[,paste('excess_alternate',ss,sep='_')]<-sum(
        MM$observed[MM_i]-MM$expected[MM_i]
      )
      RR[,paste('excess_lower',ss,sep='_')]<-sum(MM$observed[MM_i])-
        quantile(SS[,SS_i],0.975)
      RR[,paste('excess_upper',ss,sep='_')]<-sum(MM$observed[MM_i])-
        quantile(SS[,SS_i],0.025)
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
