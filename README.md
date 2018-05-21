Volatility Analysis
================
Zhengyun Dou
5/21/2018

``` r
#list of required packages
library(devtools)
library(rvest)
library(tidyverse)
library(stringr)
library(quantmod) #financial package
library(lubridate)
#library(widyr)
library(tm)
library(Matrix)
library(rARPACK)
library(data.table)
library(tidyr)
library(aTSA)
library(TSA)
```

1.Introduction
--------------

Based on the records on [MotherJones](https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/), from 01-01-2017 until now, there are totally 14 mass shootings (with fatalities 3 or above).

Table 1. Mass Shooting in US (01-01-2017 till now)

``` r
mass=read_csv("Mother Jones' Investigation_ US Mass Shootings, 1982-2018 - US mass shootings.csv")
# from https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/
# str(mass)
# head(mass)
mass2017=mass %>% 
  mutate(Date=mdy(mass$Date)) %>% 
  filter(Date>="2017-01-01") #covert the date to be the same format as in stock[]

mass2017[,c(2,3,5,6)]
```

    ## # A tibble: 14 x 4
    ##    Location                 Date       Summary                  Fatalities
    ##    <chr>                    <date>     <chr>                         <int>
    ##  1 Yountville, CA           2018-03-09 Army veteran Albert Che…          3
    ##  2 Parkland, Florida        2018-02-14 Nikolas J. Cruz, 19, he…         17
    ##  3 Melcroft, PA             2018-01-28 Timothy O'Brien Smith, …          4
    ##  4 Rancho Tehama, CA        2017-11-14 Kevin Janson Neal, 44, …          5
    ##  5 Sutherland Springs, TX   2017-11-05 Devin Patrick Kelley, a…         26
    ##  6 Thornton, CO             2017-11-01 Scott Allen Ostrem, 47,…          3
    ##  7 Edgewood, MD             2017-10-18 Radee Labeeb Prince, 37…          3
    ##  8 Las Vegas, NV            2017-10-01 Stephen Craig Paddock, …         58
    ##  9 San Francisco, CA        2017-06-14 Jimmy Lam, 38, fatally …          3
    ## 10 Tunkhannock, PA          2017-06-07 Randy Stair, a 24-year-…          3
    ## 11 Orlando, Florida         2017-06-05 John Robert Neumann, Jr…          5
    ## 12 Kirkersville, Ohio       2017-05-12 Thomas Hartless, 43, sh…          3
    ## 13 Fresno, California       2017-04-18 Kori Ali Muhammad, 39, …          3
    ## 14 Fort Lauderdale, Florida 2017-01-06 Esteban Santiago, 26, f…          5

Our analysis is to explore if the stock prices of the gun manufactuers were affected after mass shooting,

There are four major gun manufacturers in the US that go public, namely,

Table 2. Gun Manufacturers

| Company Name            | Stock Symbol |
|-------------------------|--------------|
| American Outdoor Brands | AOBC         |
| Sturm, Ruger            | RGR          |
| Vista Outdoor           | VSTO         |
| Olin                    | OLN          |

Below is a general overview of the stocks of these four companies from 2017-01-01 to 2018-04-25.

``` r
data=tibble(company=c("American Outdoor Brands","Sturm, Ruger","Vista Outdoor","Olin"),
            symbol=c("AOBC","RGR","VSTO","OLN"),
            agency=c("NASDAQ","NYSE","NYSE","NYSE"))

getSymbols(data$symbol,from="2017-01-01",adjust=TRUE) #all the four companies  
```

    ## [1] "AOBC" "RGR"  "VSTO" "OLN"

``` r
stock=Cl(get(data$symbol[1])) #close column of AOBC
for(i in 2:length(data$symbol))
  stock=merge(stock,Cl(get(data$symbol[i]))) #closure stock price for all four companies     
colnames(stock)=data$symbol
# tail(stock)
stockchange=stock%>% diff
#head(stockchange)

stock%>% as.tibble() %>% 
  mutate(date=index(stock))  %>%  
  gather('AOBC','RGR','VSTO','OLN',key="Co",value="price") %>% 
  ggplot(aes(y=price,x=date))+geom_line(aes(color=Co))
```

![](Volatility_Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

The 16 months of trends of gun stock price indicate that four companies show four distinct trends. The gun stock price of RGR (blue) shows the largest variability, yet the price keep comes back to the previous level of the trend. The OLN (green) suggests the most stable response line with the least variability. The gun stock price of AOBC (red) and VSTO (purple) both have decreasing pattern of trends, however, VSTO (purple) experiences the greatest loss in price compared to the change of the price of AOBC (red). The most noticeable and important observation in this graph is that the gun stock price of VSTO (purple) has fallen the most and the RGR (blue) has the greatest variability.

2. Stock Changes after Mass Shooting
------------------------------------

### 2.1 Stock Price Change Analysis

We selected stocks of the four companies on the massing shooting date and the following one week of each mass shooting case. Intuitively, We plot the stock prices for each case in that one week to see if there is a big difference.

``` r
mdays=function(a,x){
  day=matrix(nrow = x,ncol=length(a))
  for (i in 1:x){
    day[i,]=a+i}
  c(a,day)}
#create a function to extend the mass shooting date to the following x days

daterange1=mdays(mass2017$Date,7)
case=rep(1:14,c(6,6,6,2,5,6,5,6,3,6,5,5,5,6)) %>% as.tibble()

for_plot=stock[daterange1] %>% as.tibble() %>% 
  mutate(date=index(stock[daterange1])) %>%cbind(.,case) %>%  
  gather('AOBC','RGR','VSTO','OLN',key="Co",value="price") %>%
  `colnames<-`(c("date", "mass_shooting_case","company", "price"))

for_plot%>% ggplot(aes(y=price,x=date, color=company))+geom_point() +facet_wrap(~ mass_shooting_case, ncol = 4, scales = "free", labeller=label_both)+geom_line()+labs(title = "7 days of stock price change on the mass shooting events ")
```

![](Volatility_Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

As the mass shooting events occur, the plots indicate that changes on the stock price of AOBC (Red), OLN (green), and RGR (blue) is negligible for all 14 cases whereas VSTO (purple) shows decreasing pattern on the first case and the 10th case. One thing to note is that cases 4 and 9 are distinct from other cases because cases 5 and 10 occurred right after the cases 4 and 9. Thus, cases 5 and 10 were unable to observed for 7 days. For instance, case 4 only has 2 plot points, and this means case 5 happened 2 days after the event of case 4, therefore case 4 only has two days of observations.

Alternatively, we examine the price variances for each case in the same period as in above plot, to see if there was a big variation on the stock price.

Table 3. Stock Price Variance

``` r
variance=stock[daterange1] %>% as.tibble() %>% 
  mutate(date=index(stock[daterange1])) %>%
  cbind(.,case) %>% rename(shootingcase=value) %>% 
  group_by(shootingcase) %>% 
  summarise(varianceAOBC=var(AOBC),varianceRGR=var(RGR),
            varianceVSTO=var(VSTO),varianceOLN=var(OLN)) %>% 
  mutate(date=sort(mass2017$Date)) %>% select(shootingcase,date,varianceAOBC:varianceOLN)

variance[,c(1,3,4,5,6)]
```

    ## # A tibble: 14 x 5
    ##    shootingcase varianceAOBC varianceRGR varianceVSTO varianceOLN
    ##           <int>        <dbl>       <dbl>        <dbl>       <dbl>
    ##  1            1      0.0723       0.931      20.1         0.190  
    ##  2            2      0.00619      0.458       0.00722     1.10   
    ##  3            3      0.0706       0.259       0.209       0.380  
    ##  4            4      0.00845      0.0986      0.00320     0.00805
    ##  5            5      0.0322       1.13        1.57        0.493  
    ##  6            6      0.242        0.286       0.629       0.250  
    ##  7            7      0.0418       0.599       0.452       0.0109 
    ##  8            8      0.0235       0.168       0.128       0.204  
    ##  9            9      0.00823      0.416       0.316       0.130  
    ## 10           10      0.0996       0.496       8.73        0.190  
    ## 11           11      0.0100       0.361       0.0845      0.397  
    ## 12           12      0.198        1.18        0.0104      0.343  
    ## 13           13      0.142        1.73        0.187       0.0713 
    ## 14           14      0.0111       0.164       0.0160      0.836

From this table, we are able to draw the same conclusion that, apart from the variances of VSTO in case 1 and case 10, the other variances are comparably very small, which means there are no big price changes in those cases. When we take a closer look at these two cases only, we find the price change happened three days after the mass shootings.

Table 4 and Table 5. Detailed Stock Prices in Cases 1 and 10

``` r
stock$VSTO[mdays(ymd("2017-01-06"),7)]
```

    ##             VSTO
    ## 2017-01-06 38.08
    ## 2017-01-09 37.59
    ## 2017-01-10 37.77
    ## 2017-01-11 37.79
    ## 2017-01-12 29.58
    ## 2017-01-13 28.70

``` r
stock$VSTO[mdays(ymd("2017-11-05"),7)]
```

    ##             VSTO
    ## 2017-11-06 19.24
    ## 2017-11-07 18.64
    ## 2017-11-08 18.43
    ## 2017-11-09 13.25
    ## 2017-11-10 13.28

Therefore, we don’t see this change is directly related to the mass shooting.

### 2.2 Volatility Analysis

In order to further analyze this problem, we use volatility analysis to see if there is any impact. We build ARIMA model and extract the residuals from it. Then we regard the residual2 as the volatility. We choose stock adjusted prices of AOBC, RGR, VSTO, OLN as our data. The data are all from January 03, 2017 to April 18, 2018. After applied unit root test in r, it seems they all have unit root. Therefore, we take the differences for all of them. After differentiation, RGR and VSTO are convert to stationary with no ACF and PACF in series. Thus we treat the diff.adjusted.price^2 as volatility. As for the rest two series which is AOBC, OLN. After differentiation, we fit MA (6) and ARMA (4,4) model respectively to get the residuals and treated residuals2 as volatility. Below four plots shows the volatility of each company with the mass shooting case marked on the date it happened.

``` r
#AOBC
getSymbols("AOBC") 
#acf(AOBC$AOBC.Adjusted,lag.max = 1000)
#pacf(AOBC$AOBC.Adjusted,lag.max = 1000)
#plot(AOBC$AOBC.Adjusted)
#unit root test
aTSA::adf.test(AOBC$AOBC.Adjusted); tseries::adf.test(AOBC$AOBC.Adjusted)
AOBC.Adjusted.diff<-diff(AOBC$AOBC.Adjusted)[-1]
par(mfrow=c(1,2))
#acf(AOBC.Adjusted.diff)
#pacf(AOBC.Adjusted.diff)
#Thus the original data in random walk
##
#eacf(AOBC.Adjusted.diff,ar.max=10,ma.max=10)
Box.test(AOBC.Adjusted.diff,type="Ljung-Box",lag=12)
#ARIMA(3,0,3)
#ARIMA(3,0,6)
#ARIMA(0,0,4)
m1<-arima(AOBC.Adjusted.diff,order=c(3,0,3),include.mean = TRUE)
m2<-arima(AOBC.Adjusted.diff,order=c(3,0,6),include.mean = TRUE)
m3<-arima(AOBC.Adjusted.diff,order=c(0,0,6),include.mean = TRUE)
#confint(m1);tsdiag(m1)
#confint(m2);tsdiag(m2)
#confint(m3);tsdiag(m3)
Box.test(m1$residuals,lag=12,fitdf = 1,type="Ljung-Box")
Box.test(m2$residuals,lag=12,fitdf = 1,type="Ljung-Box")
Box.test(m3$residuals,lag=12,fitdf = 1,type="Ljung-Box")
m4<-arima(AOBC.Adjusted.diff,order=c(0,0,6),include.mean = TRUE,fixed=c(0,0,0,NA,0,NA,0))
#confint(m4);tsdiag(m4)
Box.test(m4$residuals,lag=12,fitdf = 1,type="Ljung-Box")
resi_square<-m4$residuals^2
volatility1<-xts(x = resi_square,order.by = index(AOBC.Adjusted.diff))
#plot(volatility1)
#plot(AOBC.Adjusted.diff)
```

``` r
getSymbols("RGR") 
aTSA::adf.test(RGR$RGR.Adjusted); tseries::adf.test(RGR$RGR.Adjusted)
Box.test(RGR$RGR.Adjusted,type="Ljung-Box",lag=12)
RGR.Adjusted.diff<-diff(RGR$RGR.Adjusted)[-1]
par(mfrow=c(1,2))
Box.test(RGR.Adjusted.diff,type="Ljung-Box",lag=12)
volatility2<-xts(x=(RGR.Adjusted.diff-mean(RGR.Adjusted.diff))^2,order.by = index(RGR.Adjusted.diff))
```

``` r
getSymbols("VSTO") 
VSTO.Adjusted<-VSTO$VSTO.Adjusted[!is.na(VSTO$VSTO.Adjusted)]
aTSA::adf.test(VSTO.Adjusted); tseries::adf.test(VSTO.Adjusted)
Box.test(VSTO.Adjusted,type="Ljung-Box",lag=12)
VSTO.Adjusted.diff<-diff(VSTO.Adjusted)[-1]
par(mfrow=c(1,2))
eacf(VSTO.Adjusted.diff)
Box.test(VSTO.Adjusted.diff,type="Ljung-Box",lag=12)
volatility3<-xts(x=(VSTO.Adjusted.diff-mean(VSTO.Adjusted.diff))^2,order.by = index(VSTO.Adjusted.diff))
```

``` r
getSymbols("OLN") 
OLN.Adjusted<-OLN$OLN.Adjusted[!is.na(OLN$OLN.Adjusted)]
aTSA::adf.test(OLN.Adjusted); tseries::adf.test(OLN.Adjusted)
Box.test(OLN.Adjusted,type="Ljung-Box",lag=12)
OLN.Adjusted.diff<-diff(OLN.Adjusted)[-1]
par(mfrow=c(1,2))
eacf(OLN.Adjusted.diff)
Box.test(OLN.Adjusted.diff,type="Ljung-Box",lag=12)
m1<-arima(OLN.Adjusted.diff,order=c(4,0,4),include.mean = TRUE)#,fixed=c(0,0,0,NA,0,NA,0))
m2<-arima(OLN.Adjusted.diff,order=c(4,0,4),include.mean = TRUE,fixed=c(0,NA,NA,0,0,NA,NA,0,0))
Box.test(m1$residuals,type="Ljung-Box",lag=12)
Box.test(m2$residuals,type="Ljung-Box",lag=12)
par(mfrow=c(1,3))
volatility4<-xts(x=m2$residuals^2,order.by = index(OLN.Adjusted.diff))
```

Above four chunks are used to get the volatility of each company. Since we will visulize these volatility, we hide the output from them.

Below are the visualizations of the four volatilities.

``` r
#Plots for all 4 companies volatility from Jan 2017 to April 2018.

dataset1<-data.frame(volatility=as.vector(volatility1),idx = index(volatility1), label=index(volatility1))%>%filter(idx >="2017-01-01")
dataset1<-left_join(dataset1,mass2017,by=c("idx"="Date"))


dataset2<-data.frame(volatility=as.vector(volatility2),idx = index(volatility2), label=index(volatility2))%>%filter(idx >="2017-01-01")
dataset2<-left_join(dataset2,mass2017,by=c("idx"="Date"))


dataset3<-data.frame(volatility=as.vector(volatility3),idx = index(volatility3), label=index(volatility3))%>%filter(idx >="2017-01-01")
dataset3<-left_join(dataset3,mass2017,by=c("idx"="Date"))


dataset4<-data.frame(volatility=as.vector(volatility4),idx = index(volatility4), label=index(volatility4))%>%filter(idx >="2017-01-01")
dataset4<-left_join(dataset4,mass2017,by=c("idx"="Date"))


dataset1 %>% ggplot(mapping=aes(x= idx, y= volatility))+geom_line(colour='red')+geom_text( aes(label=Case,angle=90),size=2,hjust=0,na.rm=TRUE)+
  scale_x_date(breaks = scales::pretty_breaks(n = 9))+ggtitle("Volatility Analysis of AOBC")+xlab("Date")+ylab("Volatility")
```

![](Volatility_Analysis_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
dataset2 %>% ggplot(mapping=aes(x= idx, y= volatility))+geom_line(colour='pink')+geom_text( aes(label=Case,angle=90),size=2,hjust=0,na.rm=TRUE)+
  scale_x_date(breaks = scales::pretty_breaks(n = 9))+ggtitle("Volatility Analysis of RGR")+xlab("Date")+ylab("Volatility")
```

![](Volatility_Analysis_files/figure-markdown_github/unnamed-chunk-11-2.png)

``` r
dataset3 %>% ggplot(mapping=aes(x= idx, y= volatility))+geom_line(colour='purple')+geom_text( aes(label=Case,angle=90),size=2,hjust=0,na.rm=TRUE)+
  scale_x_date(breaks = scales::pretty_breaks(n = 9))+ggtitle("Volatility Analysis of VSTO")+xlab("Date")+ylab("Volatility")
```

![](Volatility_Analysis_files/figure-markdown_github/unnamed-chunk-11-3.png)

``` r
dataset4 %>% ggplot(mapping=aes(x= idx, y= volatility))+geom_line(colour='green')+geom_text( aes(label=Case,angle=90),size=2,hjust=0,na.rm=TRUE)+
  scale_x_date(breaks = scales::pretty_breaks(n = 9))+ggtitle("Volatility Analysis of OLN")+xlab("Date")+ylab("Volatility")
```

![](Volatility_Analysis_files/figure-markdown_github/unnamed-chunk-11-4.png)

The days with bigger volatility are all out of analysis interest, in other words, on those days with mass shootings, we don’t see big volatility. Therefore, based on the volatility analysis, we conclude mass shooting has no impact on the stock prices.

### 2.3 Summary

Therefore, according to our analysis, we conclude mass shooting has no impact on the gun manufacturers stocks.
