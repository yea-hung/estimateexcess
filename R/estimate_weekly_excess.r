# define function
#' Estimate excess mortality using weekly data
#'
#' @param outcome_variable The name of the outcome variable, in quotes.
#' @param data A data frame of the aggregated data. It should have a `Date` variable named `week` and at least one variable that represents the number of deaths per week within some population (see `outcome_variable` above). `week` should represent the date of the last day of each week (for example, if you are using US convention, the last day of each week should be a Saturday).
#' @param forecast_window The number of weeks in the forecast window.
#' @param forecast_start The start date of the forecast window, in Date format.
#' @param forecast_periods Time periods (optional) within the forecast period. This should be specified as a numeric vector that has a length matching the forecast period. The values of the period should identify the periods. For example: `c(rep(1,12),rep(2,8),rep(3,15))`. If validly specified, the function will return results for each time period, in addition to results for the entire forecast window.
#' @param data_start The start date of the data, specified as a two-element numeric vector. See `ts()`. For example, `c(2016,15/7)` is the week ending 2016-01-09. That's because 2016-01-01 would be `c(2016,7/7)` and 2016-01-08 would be `c(2016,14/7)`..
#'
#' @returns A list with five elements:
#' - `results_by_week`: The weekly results.
#' - `results`: The overall results (that is, over the forecast window).
#' - `simulations`: Simulated sums, used to obtain the prediction intervals in `results` (above).
#' - `plot`: A visualization of the results.
#' - `plot_data`: The data underlying the plot.
#' @export
#'
#' @examples
#' rr<-estimate_weekly_excess('Inland Empire',weekly_data)
#' rr$results
estimate_weekly_excess<-function(
    outcome_variable,
    data,
    forecast_window=143,
    forecast_start=as.Date('2020-03-07'),
    forecast_periods=NULL,
    data_start=c(2016,15/7)
){

  # sort data
  data<-data[order(data$week),]

  # define data
  tt<-ts(data[data$week<forecast_start,outcome_variable],frequency=365.25/7,
         start=data_start)

  # fit model
  mm<-list(aicc=Inf)
  for(i in 1:26){ # should not exceed 52/2
    mm_i<-forecast::auto.arima(tt,xreg=fourier(tt,K=i),seasonal=TRUE)
    if(mm_i$aicc<mm$aicc){
      mm<-mm_i
      k.best<-i
    }
  }

  # obtain forecasts
  ff<-forecast::forecast(mm,xreg=fourier(tt,K=k.best,h=forecast_window))

  # extract observed values
  rr<-data$week[data$week>=forecast_start]
  rr<-rr[1:forecast_window]
  oo<-data[is.element(data$week,rr),c('week',outcome_variable)]
  names(oo)[2]<-'observed'

  # extract expected values
  ee<-data.frame(
    week=rr,
    expected=as.numeric(ff$mean),
    expected_lower=as.numeric(ff$lower[,'95%']),
    expected_upper=as.numeric(ff$upper[,'95%'])
  )

  # define week-specific results
  WW<-merge(oo,ee,by='week',all.x=FALSE,all.y=FALSE)
  WW$excess<-WW$observed-WW$expected
  WW$excess_lower<-WW$observed-WW$expected_upper
  WW$excess_upper<-WW$observed-WW$expected_lower

  # obtain prediction intervals for totals
  set.seed(94158)
  NN<-10000
  SS<-NULL
  for(ii in 1:NN){
    sim_i<-forecast::simulate(mm,future=TRUE,nsim=forecast_window,
                              xreg=fourier(tt,K=k.best,h=forecast_window))
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
    observed=sum(WW$observed),
    expected=mean(SS$pt),
    expected_alternate=sum(WW$expected),
    expected_lower=as.numeric(quantile(SS$pt,c(0.025))),
    expected_upper=as.numeric(quantile(SS$pt,c(0.975))),
    excess=sum(WW$observed)-mean(SS$pt),
    excess_alternate=sum(WW$observed-WW$expected),
    excess_lower=sum(WW$observed)-as.numeric(quantile(SS$pt,0.975)),
    excess_upper=sum(WW$observed)-as.numeric(quantile(SS$pt,0.025))
  )
  if(!is.null(forecast_periods)){
    for(period in unique(forecast_periods)){
      WW_i<-which(forecast_periods==period)
      SS_i<-paste('p',period,sep='')
      ss<-period
      RR[,paste('observed',ss,sep='_')]<-sum(WW$observed[WW_i])
      RR[,paste('expected',ss,sep='_')]<-mean(SS[,SS_i])
      RR[,paste('expected_alternate',ss,sep='_')]<-sum(WW$expected[WW_i])
      RR[,paste('expected_lower',ss,sep='_')]<-quantile(SS[,SS_i],c(0.025))
      RR[,paste('expected_upper',ss,sep='_')]<-quantile(SS[,SS_i],c(0.975))
      RR[,paste('excess',ss,sep='_')]<-sum(WW$observed[WW_i])-mean(SS[,SS_i])
      RR[,paste('excess_alternate',ss,sep='_')]<-sum(
        WW$observed[WW_i]-WW$expected[WW_i]
      )
      RR[,paste('excess_lower',ss,sep='_')]<-sum(WW$observed[WW_i])-
        quantile(SS[,SS_i],0.975)
      RR[,paste('excess_upper',ss,sep='_')]<-sum(WW$observed[WW_i])-
        quantile(SS[,SS_i],0.025)
    }
  }

  # define x-axis breaks
  x_minor<-unique(substr(data$week,1,7))
  x_minor<-paste(x_minor,'01',sep='-')
  x_minor<-as.Date(x_minor,'%Y-%m-%d')
  x_major<-x_minor[seq(1,length(x_minor),12)]

  # define data for plot
  pandemic<-WW[,c('week','observed','expected',
                  'expected_lower','expected_upper')]
  prior<-data[data$week<forecast_start,c('week',outcome_variable)]
  names(prior)[2]<-'observed'
  prior$expected<-as.numeric(ff$fitted)
  prior$expected_lower<-NA
  prior$expected_upper<-NA
  PP_DD<-rbind(prior,pandemic)

  # define plot
  PP<-ggplot2::ggplot(aes(x=week,y=observed),data=PP_DD)+
    ggplot2::geom_ribbon(
      ggplot2::aes(x=week,y=expected,ymin=expected_lower,
                   ymax=expected_upper),
      data=subset(PP_DD,week>=forecast_start),
      alpha=0.2,fill='#00BFC4'
    )+
    ggplot2::geom_line(aes(x=week,y=observed),color='#F8766D')+
    ggplot2::geom_line(aes(x=week,y=expected),color='#00BFC4')+
    ggplot2::scale_x_date(date_labels='%Y-%m',breaks=x_major,
                          minor_breaks=x_minor)+
    ggplot2::scale_y_continuous(labels=scales::comma)+
    ggplot2::labs(x='',y='Deaths per week')+
    ggplot2::theme_bw()

  # return results
  list(
    results_by_week=WW,
    results=RR,
    simulations=SS,
    plot=PP,
    plot_data=PP_DD
  )

}
