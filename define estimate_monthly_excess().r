# load packages
library(forecast)
library(ggplot2)

# define function
estimate_monthly_excess<-function(yy,forecast_window=14,
                                  forecast_start=as.Date('2020-03-01'),
                                  forecast_periods=NULL,
                                  data_start=c(2016,1),
                                  data=dd){ 
  
  # sort data
  data<-data[order(data$month),]
  
  # define data
  tt<-ts(data[data$month<forecast_start,yy],frequency=12,start=data_start)
  
  # fit model 
  mm<-auto.arima(tt)

  # obtain forecasts
  ff<-forecast(mm,h=forecast_window)
  
  # extract observed values
  rr<-data$month[(data$month>=forecast_start)]
  rr<-rr[1:forecast_window]
  oo<-data[is.element(data$month,rr),c('month',yy)]
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
    sim.i<-simulate(mm,future=TRUE,nsim=forecast_window)
    SS.i<-data.frame(pt=sum(sim.i))
    if(!is.null(forecast_periods)){
      for(pp in unique(forecast_periods)){
        SS.i[,paste('p',pp,sep='')]<-sum(sim.i[which(forecast_periods==pp)])
      }
    }
    SS<-rbind(SS,SS.i)
  }
  
  # store results
  RR<-data.frame(
    group=yy,
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
      MM.i<-which(forecast_periods==period) 
      SS.i<-paste('p',period,sep='') 
      ss<-period 
      RR[,paste('observed',ss,sep='_')]<-sum(MM$observed[MM.i])
      RR[,paste('expected',ss,sep='_')]<-mean(SS[,SS.i])
      RR[,paste('expected_alternate',ss,sep='_')]<-sum(MM$expected[MM.i])
      RR[,paste('expected_lower',ss,sep='_')]<-quantile(SS[,SS.i],0.025)
      RR[,paste('expected_upper',ss,sep='_')]<-quantile(SS[,SS.i],0.975)
      RR[,paste('excess',ss,sep='_')]<-sum(MM$observed[MM.i])-mean(SS[,SS.i])
      RR[,paste('excess_alternate',ss,sep='_')]<-sum(
        MM$observed[MM.i]-MM$expected[MM.i]
      )
      RR[,paste('excess_lower',ss,sep='_')]<-sum(MM$observed[MM.i])-
        quantile(SS[,SS.i],0.975)
      RR[,paste('excess_upper',ss,sep='_')]<-sum(MM$observed[MM.i])-
        quantile(SS[,SS.i],0.025)
    }
  }
  
  # define x-axis breaks 
  x.minor<-unique(substr(data$month,1,7))
  x.minor<-paste(x.minor,'01',sep='-')
  x.minor<-as.Date(x.minor,'%Y-%m-%d')
  x.major<-x.minor[seq(1,length(x.minor),12)]
  
  # define data for plot
  pandemic<-MM[,c('month','observed','expected',
                  'expected_lower','expected_upper')]
  prior<-data[data$month<forecast_start,c('month',yy)]
  names(prior)[2]<-'observed'
  prior$expected<-as.numeric(ff$fitted)
  prior$expected_lower<-NA
  prior$expected_upper<-NA
  plot_data<-rbind(prior,pandemic)
  
  # define plot
  PP<-ggplot(aes(x=month,y=observed),data=plot_data)+
    geom_ribbon(aes(x=month,y=expected,ymin=expected_lower,
                    ymax=expected_upper),
                data=subset(plot_data,month>=forecast_start),
                alpha=0.2,fill='#00BFC4')+
    geom_line(aes(x=month,y=observed),color='#F8766D')+
    geom_line(aes(x=month,y=expected),color='#00BFC4')+   
    scale_x_date(date_labels='%Y-%m',breaks=x.major,minor_breaks=x.minor)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='',y='Deaths per month')+
    theme_bw()
  
  # return results
  list(results_by_month=MM,results=RR,simulations=SS,plot=PP,
       plot_data=plot_data)
  
}