# load packages
library(forecast)
library(ggplot2)

# define function
estimate_monthly_excess<-function(yy,forecast.window=14,
                                  forecast.start=as.Date('2020-03-01'),
                                  data.start=c(2016,1),
                                  data=dd,stub='^dpm\\.'){ 
  # sort data
  data<-data[order(data$month),]
  # define data
  tt<-ts(data[data$month<forecast.start,yy],freq=12,start=data.start) 
  # fit model 
  mm<-auto.arima(tt)
  # obtain forecasts
  ff<-forecast(mm,h=forecast.window,biasadj = TRUE)
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
  # define prior deaths
  pp<-data
  earliest.year<-min(as.numeric(substr(pp$month,1,4)))
  pp<-lapply(earliest.year:2019,function(year){
    prior.start<-as.Date(paste(year,'03-01',sep='-'),'%Y-%m-%d')
    prior<-subset(pp,month>=prior.start)
    prior<-prior[1:forecast.window,c('month',yy)]
    prior<-subset(prior,month<as.Date('2020-03-01','%Y-%m-%d'))
    names(prior)[names(prior)==yy]<-'prior'
    prior$year<-year
    prior$month.original<-prior$month
    prior$actual.year<-as.numeric(substr(prior$month,1,4))
    prior$fake.year<-2020+(prior$actual.year-min(prior$actual.year))
    prior$month<-paste(prior$fake.year,substr(prior$month,6,10),sep='-')
    prior$month<-as.Date(prior$month,'%Y-%m-%d')
    prior
  })
  pp<-do.call(rbind,pp)
  row.names(pp)<-NULL
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
  PP<-ggplot(aes(x=month,y=observed),data=oo)+
    geom_line(aes(x=month,y=prior,group=year),data=pp,color='gray85')+
    geom_ribbon(aes(x=month,y=expected,ymin=expected.lower,
                    ymax=expected.upper),data=ee,
                alpha=0.2,fill='#00BFC4')+
    geom_line(aes(x=month,y=expected),data=ee,color='#00BFC4')+
    geom_line(color='#F8766D')+
    scale_x_date(date_labels='%Y-%m',breaks=x.major,minor_breaks=x.minor)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='',y='Deaths per month',title=tt)+
    theme_bw()
  # return results
  list(results.by.month=MM,results=RR,simulations=SS,plot=PP)
}