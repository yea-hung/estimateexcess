# load packages
library(forecast)
library(ggplot2)

# define function
estimate_weekly_excess<-function(yy,forecast_window=143,
                                 forecast_start=as.Date('2020-03-07'),
                                 forecast_periods=NULL,
                                 data_start=c(2016,15/7), # the week ending 2016-01-09
                                 data=dd){ 
  
  # sort data
  data<-data[order(data$week),]
  
  # define data
  tt<-ts(data[data$week<forecast_start,yy],frequency=365.25/7,
         start=data_start) 
  
  # fit model 
  mm<-list(aicc=Inf)
  for(i in 1:25){ # should not exceed 52/2
    mm.i<-auto.arima(tt,xreg=fourier(tt,K=i),seasonal=FALSE)
    if(mm.i$aicc<mm$aicc){
      mm<-mm.i
      k.best<-i
    } 
  }
  
  # obtain forecasts
  ff<-forecast(mm,xreg=fourier(tt,K=k.best,h=forecast_window))
  
  # extract observed values
  rr<-data$week[data$week>=forecast_start]
  rr<-rr[1:forecast_window]
  oo<-data[is.element(data$week,rr),c('week',yy)]
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
    sim.i<-simulate(mm,future=TRUE,nsim=forecast_window,
                    xreg=fourier(tt,K=k.best,h=forecast_window))
    SS.i<-data.frame(pt=sum(sim.i))
    if(!is.null(forecast_periods)){
      for(pp in unique(forecast_periods)){
        SS.i[,paste('p',pp,sep='')]<-sum(sim.i[which(forecast_periods==pp)])
      }
    }
    SS<-rbind(SS,SS.i)
  }
  
  # define overall results
  RR<-data.frame(
    group=yy,
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
      WW.i<-which(forecast_periods==period) 
      SS.i<-paste('p',period,sep='') 
      ss<-period 
      RR[,paste('observed',ss,sep='.')]<-sum(WW$observed[WW.i])
      RR[,paste('expected',ss,sep='.')]<-mean(SS[,SS.i])
      RR[,paste('expected_alternate',ss,sep='.')]<-sum(WW$expected[WW.i])
      RR[,paste('expected_lower',ss,sep='.')]<-quantile(SS[,SS.i],c(0.025))
      RR[,paste('expected_upper',ss,sep='.')]<-quantile(SS[,SS.i],c(0.975))
      RR[,paste('excess',ss,sep='.')]<-sum(WW$observed[WW.i])-mean(SS[,SS.i])
      RR[,paste('excess_alternate',ss,sep='.')]<-sum(
        WW$observed[WW.i]-WW$expected[WW.i]
      )
      RR[,paste('excess_lower',ss,sep='.')]<-sum(WW$observed[WW.i])-
        quantile(SS[,SS.i],0.975)
      RR[,paste('excess_upper',ss,sep='.')]<-sum(WW$observed[WW.i])-
        quantile(SS[,SS.i],0.025)
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
  prior<-data[data$week<forecast_start,c('week',yy)]
  names(prior)[2]<-'observed'
  prior$expected<-as.numeric(ff$fitted)
  prior$expected_lower<-NA
  prior$expected_upper<-NA
  plot_data<-rbind(prior,pandemic)
  
  # define plot
  PP<-ggplot(aes(x=week,y=observed),data=plot_data)+
    geom_ribbon(aes(x=week,y=expected,ymin=expected_lower,
                    ymax=expected_upper),
                data=subset(plot_data,week>=forecast_start),
                alpha=0.2,fill='#00BFC4')+
    geom_line(aes(x=week,y=observed),color='#F8766D')+
    geom_line(aes(x=week,y=expected),color='#00BFC4')+   
    scale_x_date(date_labels='%Y-%m',breaks=x_major,minor_breaks=x_minor)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='',y='Deaths per week')+
    theme_bw()
  
  # return results
  list(results_by_week=WW,results=RR,simulations=SS,plot=PP)
  
}