# load packages
library(forecast)
library(ggplot2)

# define function
estimate_monthly_excess<-function(yy,forecast.window=14,
                                  forecast.start=as.Date('2020-03-01'),
                                  data.start=c(2016,1),
                                  data=DD,stub='^dpm\\.'){ 
  # define data
  tt<-ts(data[data$date<forecast.start,yy],freq=12,start=data.start) 
  # fit model 
  mm<-auto.arima(tt)
  # obtain forecasts
  ff<-forecast(mm,h=forecast.window,biasadj = TRUE)
  # extract observed values
  rr<-data$date[(data$date>=forecast.start)]
  rr<-rr[1:forecast.window]
  oo<-data[is.element(data$date,rr),c('date',yy)]
  names(oo)[2]<-'observed'
  # extract expected values
  ee<-data.frame(
    date=rr,
    expected=as.numeric(ff$mean),
    expected.lower=as.numeric(ff$lower[,'95%']),
    expected.upper=as.numeric(ff$upper[,'95%'])
  )
  # define date-specific results
  DD<-merge(oo,ee,by='date',all.x=FALSE,all.y=FALSE)
  DD$excess<-DD$observed-DD$expected
  DD$excess.lower<-DD$observed-DD$expected.upper
  DD$excess.upper<-DD$observed-DD$expected.lower
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
    observed=sum(DD$observed),
    expected=sum(DD$expected),
    expected.lower=as.numeric(quantile(SS$pt,c(0.025))),
    expected.upper=as.numeric(quantile(SS$pt,c(0.975))),
    excess=sum(DD$observed-DD$expected),
    excess.alternate=sum(DD$observed)-mean(SS$pt),
    excess.lower=sum(DD$observed)-as.numeric(quantile(SS$pt,0.975)),
    excess.upper=sum(DD$observed)-as.numeric(quantile(SS$pt,0.025))
  )
  # define prior deaths
  pp<-data
  pp<-lapply(2016:2019,function(year){
    prior.start<-as.Date(paste(year,'03-01',sep='-'),'%Y-%m-%d')
    prior<-subset(pp,date>=prior.start)
    prior<-prior[1:forecast.window,c('date',yy)]
    prior<-subset(prior,date<as.Date('2020-03-01','%Y-%m-%d'))
    names(prior)[2]<-'prior'
    prior$year<-year
    prior$date<-as.Date(paste('2020',substr(prior$date,6,10),sep='-'),'%Y-%m-%d')
    prior
  })
  pp<-do.call(rbind,pp)
  # define title for plot
  tt<-gsub(stub,'',yy)
  tt<-paste(tt,':',sep='')
  excess.point<-RR$excess
  excess.point<-format(round(excess.point,0),trim=TRUE,big.mark=',')
  excess.interval<-c(RR$excess.lower,RR$excess.upper)
  excess.interval<-format(round(excess.interval,0),trim=TRUE,big.mark=',')
  excess.interval<-paste(excess.interval,collapse=' to ')
  excess.interval<-paste('(',excess.interval,')',sep='')
  tt<-paste(tt,excess.point,excess.interval,'excess deaths')
  # define x-axis breaks 
  x.major<-rr[seq(1,length(rr),3)]
  x.minor<-rr
  # define plot
  PP<-ggplot(aes(x=date,y=observed),data=oo)+
    geom_line(aes(x=date,y=prior,group=year),data=pp,color='gray85')+
    geom_ribbon(aes(x=date,y=expected,ymin=expected.lower,
                    ymax=expected.upper),data=ee,
                alpha=0.2,fill='#00BFC4')+
    geom_line(aes(x=date,y=expected),data=ee,color='#00BFC4')+
    geom_line(color='#F8766D')+
    scale_x_date(date_labels='%Y-%m',breaks=x.major,minor_breaks=x.minor)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='',y='Deaths per month',title=tt)+
    theme_bw()
  # return results
  list(results.by.date=DD,results=RR,plot=PP)
}