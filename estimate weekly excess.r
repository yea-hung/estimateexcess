# load packages
library(forecast)
library(ggplot2)

# define function
estimate_weekly_excess<-function(yy,forecast.window=143,
                                 forecast.start=as.Date('2020-03-07'),
                                 data.start=c(2016,15/7), # the week ending 2016-01-09
                                 data=dd){ 
  # sort data
  data<-data[order(data$week),]
  # define data
  tt<-ts(data[data$week<forecast.start,yy],frequency=365.25/7,
         start=data.start) 
  # fit model 
  # - https://otexts.com/fpp2/complexseasonality.html
  # - https://robjhyndman.com/hyndsight/forecasting-weekly-data/
  mm<-list(aicc=Inf)
  for(i in 1:25){ # should not exceed 52/2
    mm.i<-auto.arima(tt,xreg=fourier(tt,K=i),seasonal=FALSE)
    if(mm.i$aicc<mm$aicc){
      mm<-mm.i
      k.best<-i
    } 
  }
  # obtain forecasts
  ff<-forecast(mm,xreg=fourier(tt,K=k.best,h=forecast.window))
  # extract observed values
  rr<-data$week[data$week>=forecast.start]
  rr<-rr[1:forecast.window]
  oo<-data[is.element(data$week,rr),c('week',yy)]
  names(oo)[2]<-'observed'
  # extract expected values
  ee<-data.frame(
    week=rr,
    expected=as.numeric(ff$mean),
    expected.lower=as.numeric(ff$lower[,'95%']),
    expected.upper=as.numeric(ff$upper[,'95%'])
  )
  # define week-specific results
  WW<-merge(oo,ee,by='week',all.x=FALSE,all.y=FALSE)
  WW$excess<-WW$observed-WW$expected
  WW$excess.lower<-WW$observed-WW$expected.upper
  WW$excess.upper<-WW$observed-WW$expected.lower
  # obtain prediction intervals for totals
  set.seed(94158)
  NN<-10000
  SS<-NULL
  for(ii in 1:NN){
    sim.i<-simulate(mm,future=TRUE,nsim=forecast.window,
                    xreg=fourier(tt,K=k.best,h=forecast.window))
    SS.i<-data.frame(pt=sum(sim.i))
    SS<-rbind(SS,SS.i)
  }
  # define overall results
  RR<-data.frame(
    group=yy,
    observed=sum(WW$observed),
    expected=sum(WW$expected),
    expected.alternate=mean(SS$pt),
    expected.lower=as.numeric(quantile(SS$pt,c(0.025))),
    expected.upper=as.numeric(quantile(SS$pt,c(0.975))),
    excess=sum(WW$observed-WW$expected),
    excess.alternate=sum(WW$observed)-mean(SS$pt),
    excess.lower=sum(WW$observed)-as.numeric(quantile(SS$pt,0.975)),
    excess.upper=sum(WW$observed)-as.numeric(quantile(SS$pt,0.025))
  )
  # define x-axis breaks 
  x.minor<-unique(substr(data$week,1,7))
  x.minor<-paste(x.minor,'01',sep='-')
  x.minor<-as.Date(x.minor,'%Y-%m-%d')
  x.major<-x.minor[seq(1,length(x.minor),12)]
  # define data for plot
  pandemic<-WW[,c('week','observed','expected',
                  'expected.lower','expected.upper')]
  prior<-data[data$week<forecast.start,]
  names(prior)[2]<-'observed'
  prior$expected<-as.numeric(ff$fitted)
  prior$expected.lower<-NA
  prior$expected.upper<-NA
  plot.data<-rbind(prior,pandemic)
  # define plot
  PP<-ggplot(aes(x=week,y=observed),data=plot.data)+
    geom_ribbon(aes(x=week,y=expected,ymin=expected.lower,
                    ymax=expected.upper),
                data=subset(plot.data,week>=forecast.start),
                alpha=0.2,fill='#00BFC4')+
    geom_line(aes(x=week,y=observed),color='#F8766D')+
    geom_line(aes(x=week,y=expected),color='#00BFC4')+   
    scale_x_date(date_labels='%Y-%m',breaks=x.major,minor_breaks=x.minor)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='',y='Deaths per week')+
    theme_bw()
  # return results
  list(results.by.week=WW,results=RR,simulations=SS,plot=PP)
}