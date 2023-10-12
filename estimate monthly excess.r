# load packages
library(forecast)
library(ggplot2)

# define function
estimate_monthly_excess<-function(yy,forecast.window=14,
                                  forecast.start=as.Date('2020-03-01'),
                                  data.start=c(2016,1),
                                  data=dd){ 
  # sort data
  data<-data[order(data$month),]
  # define data
  tt<-ts(data[data$month<forecast.start,yy],frequency=12,start=data.start) 
  # fit model 
  mm<-auto.arima(tt)
  # obtain forecasts
  ff<-forecast(mm,h=forecast.window)
  # extract observed values
  rr<-data$month[(data$month>=forecast.start)]
  rr<-rr[1:forecast.window]
  oo<-data[is.element(data$month,rr),c('month',yy)]
  names(oo)[2]<-'observed'
  # extract expected values
  ee<-data.frame(
    month=rr,
    expected=as.numeric(ff$mean),
    expected.lower=as.numeric(ff$lower[,'95%']),
    expected.upper=as.numeric(ff$upper[,'95%'])
  )
  # define month-specific results
  MM<-merge(oo,ee,by='month',all.x=FALSE,all.y=FALSE)
  MM$excess<-MM$observed-MM$expected
  MM$excess.lower<-MM$observed-MM$expected.upper
  MM$excess.upper<-MM$observed-MM$expected.lower
  # obtain prediction intervals for totals
  set.seed(94158)
  NN<-10000
  SS<-NULL
  for(ii in 1:NN){
    sim.i<-simulate(mm,future=TRUE,nsim=forecast.window)
    SS.i<-data.frame(pt=sum(sim.i))
    SS<-rbind(SS,SS.i)
  }
  # store results
  RR<-data.frame(
    group=yy,
    observed=sum(MM$observed),
    expected=sum(MM$expected),
    expected.alternate=mean(SS$pt),
    expected.lower=as.numeric(quantile(SS$pt,c(0.025))),
    expected.upper=as.numeric(quantile(SS$pt,c(0.975))),
    excess=sum(MM$observed-MM$expected),
    excess.alternate=sum(MM$observed)-mean(SS$pt),
    excess.lower=sum(MM$observed)-as.numeric(quantile(SS$pt,0.975)),
    excess.upper=sum(MM$observed)-as.numeric(quantile(SS$pt,0.025))
  )
  # define x-axis breaks 
  x.minor<-unique(substr(data$month,1,7))
  x.minor<-paste(x.minor,'01',sep='-')
  x.minor<-as.Date(x.minor,'%Y-%m-%d')
  x.major<-x.minor[seq(1,length(x.minor),12)]
  # define data for plot
  pandemic<-MM[,c('month','observed','expected',
                  'expected.lower','expected.upper')]
  prior<-data[data$month<forecast.start,c('month',yy)]
  names(prior)[2]<-'observed'
  prior$expected<-as.numeric(ff$fitted)
  prior$expected.lower<-NA
  prior$expected.upper<-NA
  plot.data<-rbind(prior,pandemic)
  # define plot
  PP<-ggplot(aes(x=month,y=observed),data=plot.data)+
    geom_ribbon(aes(x=month,y=expected,ymin=expected.lower,
                    ymax=expected.upper),
                data=subset(plot.data,month>=forecast.start),
                alpha=0.2,fill='#00BFC4')+
    geom_line(aes(x=month,y=observed),color='#F8766D')+
    geom_line(aes(x=month,y=expected),color='#00BFC4')+   
    scale_x_date(date_labels='%Y-%m',breaks=x.major,minor_breaks=x.minor)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='',y='Deaths per month')+
    theme_bw()
  # return results
  list(results.by.month=MM,results=RR,simulations=SS,plot=PP)
}