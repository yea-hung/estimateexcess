# load packages
library(forecast)
library(ggplot2)

# define function
estimate_weekly_excess<-function(yy,forecast.window=91,
                                 forecast.start=as.Date('2020-03-07'),
                                 data=DD,mapping=MM,
                                 stub='^dpw\\.'){ 
  # initiate results object, if it doesn't already exist
  if(!exists('RR')){
    RR<-NULL
  }
  # define data
  tt<-ts(data[data$date<forecast.start,yy],freq=365.25/7,
         start=c(2016,15/7)) # the week ending 2016-01-09
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
  # define and output combined data, with observed, expected, and excess
  bb<-merge(oo,ee,by='date',all.x=FALSE,all.y=FALSE)
  if(!file.exists('generated data')){
    dir.create('generated data') # creates the 'generated data' directory
  }
  ff<-paste('generated data/weekly ',gsub(stub,'',yy),'.rds',sep='')
  saveRDS(bb,ff)
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
  # store results
  delta<-data.frame(
    # define variable and group
    variable=mapping$variable[mapping$group==gsub(stub,'',yy)],
    group=yy,
    # define results for entire period
    observed=sum(bb$observed),
    expected=sum(bb$expected),
    expected.lower=as.numeric(quantile(SS$pt,c(0.025))),
    expected.upper=as.numeric(quantile(SS$pt,c(0.975))),
    excess=sum(bb$observed-bb$expected),
    excess.alternate=sum(bb$observed)-mean(SS$pt),
    excess.lower=sum(bb$observed)-as.numeric(quantile(SS$pt,0.975)),
    excess.upper=sum(bb$observed)-as.numeric(quantile(SS$pt,0.025))
  )
  RR<<-rbind(RR,delta)
  # define prior deaths
  pp<-data
  pp$date<-as.Date(pp$date,'%Y-%m-%d')
  pp<-lapply(2016:2019,function(year){
    prior.start<-as.Date(paste(year,'03-07',sep='-'),'%Y-%m-%d')
    prior<-subset(pp,date>=prior.start)
    prior<-prior[1:forecast.window,c('date',yy)]
    prior<-subset(prior,date<as.Date('2020-03-07','%Y-%m-%d'))
    names(prior)[2]<-'prior'
    prior$year<-year
    prior$date<-prior$date+(2020-year)*365.25
    prior
  })
  pp<-do.call(rbind,pp)
  # define title for plot
  tt<-gsub(stub,'',yy)
  tt<-paste(tt,':',sep='')
  excess.point<-delta$excess
  excess.point<-format(round(excess.point,0),trim=TRUE,big.mark=',')
  excess.interval<-c(delta$excess.lower,delta$excess.upper)
  excess.interval<-format(round(excess.interval,0),trim=TRUE,big.mark=',')
  excess.interval<-paste(excess.interval,collapse=' to ')
  excess.interval<-paste('(',excess.interval,')',sep='')
  tt<-paste(tt,excess.point,excess.interval,'excess deaths')
  # define x-axis breaks 
  x.major<-rr[seq(1,length(rr),12)]
  x.minor<-rr
  # define plot
  gg<-ggplot(aes(x=date,y=observed),data=oo)+
    geom_line(aes(x=date,y=prior,group=year),data=pp,color='gray85')+
    geom_ribbon(aes(x=date,y=expected,ymin=expected.lower,
                    ymax=expected.upper),data=ee,
                alpha=0.2,fill='#00BFC4')+
    geom_line(aes(x=date,y=expected),data=ee,color='#00BFC4')+
    geom_line(color='#F8766D')+
    scale_x_date(date_labels='%Y-%m-%d',breaks=x.major,minor_breaks=x.minor)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='',y='Deaths per week',title=tt)+
    theme_bw()
  # show plot
  print(gg)
}