###################################################
## Global Variables and Function Initialisation  ##
## You have to run these functions first         ##
###################################################

options (digits =3)
options (blpAutoConnect = TRUE)

###################################################
## Description: Does a test on which packages are installed and if necessary installs them locally and makes them available locally
## Date 31 August 2016
###################################################


pkgTest <- function(x,cranMirror="http://www.stats.bris.ac.uk/R/")
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE,repos=cranMirror)
        if(!require(x,character.only = TRUE)) stop ("Package not found")
    }
  }


#########################################################################################################################################################################
##Loads the code to query bloomberg using BB Tickers##
#########################################################################################################################################################################






#########################################################################################################################################################################
## include code for parameterising the stochastic differential equation
#########################################################################################################################################################################
# The legacy commands to accomplish this are 
# install.packages("timeDate",repos="http://www.stats.bris.ac.uk/R" )
# library("timeDate")
# require(timeDate)
#pkgTest("Rblpapi",cranMirror="http://cran.ma.imperial.ac.uk/")
#pkgTest("timeDate")
#pkgTest("sde")
#pkgTest("xts")
#pkgTest("quantmod")
#pkgTest("lubridate")
#pkgTest("VarSwapPrice")
#pkgTest("plyr")
#pkgTest("PortRisk")
#pkgTest("xtable")
#pkgTest("FRAPO")
#pkgTest("fPortfolio")
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#biocLite("graph") 



#########################################################################################################################################################################
## maximimum likelihood method of parameterising the stochastic differential equation
#########################################################################################################################################################################

fit.OU_exact <- function (S,delta=1)
{
S<-as.data.frame(S)
n <-  dim(array(S))[1]-1
end<- dim(array(S))[1]
S<-array(S[,1])
  Sx  <- sum( S[1:(end-1)] )
  Sy  <- sum( S[2:end] )
  Sxx<- sum( S[1:(end-1)]^2 )
  Sxy <- sum( S[1:(end-1)]*S[2:end] )
  Syy <- sum( S[2:end]^2 )
 
  mu  <- (Sy*Sxx - Sx*Sxy) / ( n*(Sxx - Sxy) - (Sx^2 - Sx*Sy) )
  lambda <- -log( (Sxy - mu*Sx - mu*Sy + n*mu^2) / (Sxx -2*mu*Sx + n*mu^2) ) / delta
  a <- exp(-lambda*delta)
  sigmah2 <- (Syy - 2*a*Sxy + a^2*Sxx - 2*mu*(1-a)*(Sy - a*Sx) + n*mu^2*(1-a)^2)/n
  sigma <- sqrt(sigmah2*2*lambda/(1-a^2))

#return(list(mu,lambda,sigma))

theta1<-lambda*mu
half.life <- log(2) / lambda
life.to90pct<- log(10) / lambda
life.to99pct<- log(100) / lambda
results<-list(mu=mu,lambda=lambda,sigma=sigma,theta1=theta1,theta2=lambda,theta3=sigma,half.life=half.life,life.to90pct=life.to90pct,life.to99pct=life.to99pct)



#results<-data.frame(c(mu,lambda,sigma,theta1,half.life,life.to90pct))   # working code temporarily commented out
#results<-list(mu=mu,lambda=lambda,sigma=sigma,theta1=theta1,theta2=lambda,theta3=sigma,half.life=half.life,life.to90pct=life.to90pct)
#names(results)<-NULL # working code temporarily commented out
#row.names(results)<-list("mu","lambda.theta2","sigma.theta3","theta1","half.life","life.to90pct") # working code temporarily commented out

return(results)
}



#########################################################################################################################################################################
## code for defining likelihood function for ornstein uhlenbeck function as defined by "sde" class NOTE theta1=mu*lambda,theta2=lambda,theta3=sigma
#########################################################################################################################################################################

ou.lik <- function(x) {
  function(theta1,theta2,theta3) {
    n <- length(x)
    dt <- deltat(x)
    -sum(dcOU(x=x[2:n], Dt=dt, x0=x[1:(n-1)],
      theta=c(theta1,theta2,theta3), log=TRUE))
  }
}

fit.vasicek<-function(spread)
{
spread<-as.ts(spread)
ou.fit <- mle(ou.lik(spread),
  start=list(theta1=0,theta2=0.0,theta3=0.00),
  method="L-BFGS-B",lower=c(0,1e-5,1e-3), upper=c(1,1,1)
)
ou.coe <- coef(ou.fit)

lambda<-ou.coe[2]
mu<-ou.coe[1]/ou.coe[2]
sigma<-ou.coe[3]

theta1<-ou.coe[1]
theta2<-ou.coe[2]
theta3<-ou.coe[3]

half.life <- log(2) / lambda
life.to90pct<- log(10) / lambda
life.to99pct<- log(100) / lambda
results<-list(mu=mu,lambda=lambda,sigma=sigma,theta1=theta1,theta2=lambda,theta3=sigma,half.life=half.life,life.to90pct=life.to90pct,life.to99pct=life.to99pct)
return(results)

}


#########################################################################################################################################################################
## Downloads data from Bloomberg using the Bloomberg API toolcreates
## Date:  31 March 2016
#########################################################################################################################################################################

getBBGdata2 <- function(ticker_code,start_date,end_date=NULL,frequency="DAILY",required_fields="PX_LAST")
{



Sys.setenv(TZ="GMT")     # Tells Bloomberg to use Green Mean Time

option.fields <- c("periodicitySelection", "nonTradingDayFillOption",
                   "nonTradingDayFillMethod", #"periodicityAdjustment",
                   "adjustmentFollowDPDF", "pricingOption")

option.values <- c(frequency, "NON_TRADING_WEEKDAYS", 
                   "PREVIOUS_VALUE",#   "CALENDAR", 
				   "TRUE", "PRICING_OPTION_PRICE")

bbg_options <- structure(option.values, names = option.fields)

downloaded_data<-bdh(securities               =ticker_code,                fields=required_fields, 
                     start.date               = as.Date(start_date),    end.date = as.Date(end_date), 
					 options = bbg_options,            #include.non.trading.days = FALSE,                   
					 overrides                = NULL,                    verbose = FALSE, 
					 identity                 = NULL)

numvars<-dim(ticker_code)[1]
numobs<-dim(downloaded_data[[1]])[1]#/numvars


dates <- downloaded_data[[1]][,1]
tempdata = downloaded_data[[1]][,1]

for ( counter in 1:numvars)
{
tempdata <- cbind(tempdata,downloaded_data[[counter]][2])
}

dataset <- data.frame(tempdata)

new_ticker_code <- gsub(" ", "", ticker_code)
colnames(dataset)<-c("Dates",new_ticker_code)
return(dataset)

}


#########################################################################################################################################################################
## Prints out the time series of behaviour of factors
#########################################################################################################################################################################

plot.factor.time.series <- function(prcomp.obj,datelabels=NULL)
{
	number.of.factors.used<-dim(prcomp.obj$rotation)[2]
	Automatic.Eigen.Vector.Labels <- paste("Eig",1:number.of.factors.used)
	plot(prcomp.obj$x[,1]~datelabels, type = "l", col = colours.for.barcharts[1],axes=FALSE, frame.plot=TRUE, main = "First n factors")

for (i in 2:number.of.factors.used)
		{
		points( prcomp.obj$x[,i]~datelabels, type = "l",col = colours.for.barcharts[i],axes=FALSE, frame.plot=TRUE,) # adds to the previous chart
		#time <- 1:dim(rates)[1] # creates a time stamp. In the future need to 
		#change this code to create dates and use a chron datatype
		}
	legend("topleft", legend=paste("Eig",1:number.of.factors.used),fill = colours.for.barcharts)
	grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
	#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "light gray")
	
	axis(side=1, at = datelabels, labels = format(datelabels,"%m-%y"), tick = TRUE)#  line = NA, #side puts the axis on the bottom of the chart
	axis(side=2, tick = TRUE)#  line = NA, #side puts the axis on the bottom of the chart

	
#     pos = NA, outer = FALSE, font = NA, lty = "solid",
#     lwd = 1, lwd.ticks = lwd, col = NULL, col.ticks = NULL,
#     hadj = NA, padj = NA)
	
	
}


#########################################################################################################################################################################
## Plot single factor time series
#########################################################################################################################################################################

plot.variable.time.series <- function(prcomp.obj,factor =1,datelabels=NULL)
{
#	number.of.factors.used<-dim(prcomp.obj$rotation)[2]
#	par(mfrow = c(1,1))
#	dev.new()
	Main.Chart.Label <- paste("Time Series of Factor ",factor)
	plot(prcomp.obj$x[,factor]~datelabels, type = "l", col = colours.for.barcharts[factor],axes=FALSE, frame.plot=TRUE, main = Main.Chart.Label)
	grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
	#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "light gray")
	axis(side=1, at = datelabels, labels = format(datelabels,"%m-%y"), tick = TRUE)#  line = NA, #side puts the axis on the bottom of the chart

}
#########################################################################################################################################################################
## Plots the eigen vectors
#########################################################################################################################################################################

plot.eigen.values <-function(prcomp.obj)
{
number.of.factors.used<-dim(prcomp.obj$rotation)[2]
#barplot(prcomp.obj$sdev^2/sum(prcomp.obj$sdev^2),ylim=c(0,1), axis.lty=1,ylab=" % of Variance Explained", main="Scaled Eigenvalues",names=c("Eig1","Eig2","Eig3","Eig4"),axes=TRUE) # need to manually calculate
# this code recreates the Eigenvalues tab in Excel)
barplot(prcomp.obj$sdev^2/sum(prcomp.obj$sdev^2),ylim=c(0,1), col=colours.for.barcharts,axis.lty=1,ylab=" % of Variance Explained", main="Scaled Eigenvalues",names=paste("Eig",1:number.of.factors.used),axes=TRUE) # need to manually calculate
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "light gray")
}

#########################################################################################################################################################################
## Plots the factor loadings for each input variable
#########################################################################################################################################################################


plot.factor.loadings <-function(prcomp.obj,ticker.required)
{
#y<-c("red","blue","green","black","purple")[1:dim(prcomp.obj$rotation)[2]]#original working code for four colours only
#colours<-rep(y,dim(prcomp.obj$rotation)[1])#original working code for four colours only
#y<- colours.for.barcharts[1:dim(prcomp.obj$rotation)[2]]
y<- colours.for.barcharts[1:dim(prcomp.obj$rotation)[2]]
colours<-rep(y,dim(prcomp.obj$rotation)[1])


factor.loadings<-(summary(prcomp.obj)[[2]])
ticker.required<-strip.ticker.suffixes(ticker.required)
row.names(factor.loadings)<-c(ticker.required)

number.of.factors.used<-5#dim(prcomp.obj$rotation)[2]
#barplot(t(factor.loadings),ylab="Exposure of Variable to Factor", col=colours, main="Factor Exposures",beside=TRUE,ylim=c(round(min(factor.loadings)),round(max(factor.loadings))))
barplot(t(factor.loadings),ylab="Exposure of Variable to Factor", col=colours, main="Factor Exposures",beside=TRUE,ylim=c(round(min(factor.loadings)),round(max(factor.loadings))))

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
legend("topleft", legend=paste("Eig",1:number.of.factors.used),fill = colours.for.barcharts)
}

#########################################################################################################################################################################
## Plot variable time series
#########################################################################################################################################################################

DUMMY.plot.variable.time.series <- function(input.vars)
{

	number.of.factors.used<-dim(input.vars)[2]
	Automatic.Eigen.Vector.Labels <- paste("Eig",1:number.of.factors.used)
	plot(prcomp.obj$x[,1], type = "l", col = colours.for.barcharts[1], main = "First three factors")

for (i in 2:number.of.factors.used)
		{
		points( prcomp.obj$x[,i], type = "l",col = colours.for.barcharts[i]) # adds to the previous chart
		#time <- 1:dim(rates)[1] # creates a time stamp. In the future need to 
		#change this code to create dates and use a chron datatype
		}
	legend("topleft", legend=paste("Eig",1:number.of.factors.used),fill = colours.for.barcharts)
	grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

}


#help("stripplot")
#########################################################################################################################################################################
## Defines colours for Eigen Value charts
#########################################################################################################################################################################
colours.for.barcharts=c("red","blue","green","grey","pink","yellow","light blue","light green","black","dark blue","dark green")

#########################################################################################################################################################################
## strip off codes
#########################################################################################################################################################################
strip.ticker.suffixes <- function(bbg.ticker.with.suffix)
	{
	num.of.elements <-dim(matrix(bbg.ticker.with.suffix,ncol=1))[1]
	bbg.ticker.no.suffix<-data.frame(FALSE, nrow=num.of.elements)
	for (i in 1:num.of.elements)
		{
		bbg.ticker.no.suffix[i] <-unlist(strsplit(bbg.ticker.with.suffix[i], split=' ', fixed=TRUE))[1]
		}
return(c(bbg.ticker.no.suffix))
}

#########################################################################################################################################################################
## This calculates the amount of the inputs needed to recreate factor (i) exposure
#########################################################################################################################################################################
 calc.hedge.ratio <- function(prcomp.obj,factor=1)
{
#install.packages(MASS)
require(MASS)

mtrx<-prcomp.obj$rotation
num.factors<- dim(mtrx)[2]
num.vars<- dim(mtrx)[1]

allvarsover.hedge.vector<- t(array(c(rep(0,factor-1),rep(1,100)),num.factors))
point.hedge.vector<-t(array(c(rep(0,factor-1),1,rep(0,100)),num.factors))

allvarsover.hedge.ratios<-allvarsover.hedge.vector%*%ginv(mtrx)
point.hedge.ratios<-point.hedge.vector%*%ginv(mtrx)

results<-(as.matrix(rbind(allvarsover.hedge.ratios,point.hedge.ratios)))
## This returns a 2 columns vector. the first col has approx hedge (assuming all pca over tgt are negligible the 2nd has exact hedge ratio

return(results)
}



convert.to.timeseries <- function(original.data)
{
num.columns <- dim(original.data)[2]
temp.data<- original.data[,2:num.columns]
datefields.dt <- strptime(original.data[,1],"%d-%b-%y")


data.ts<<-xts(x=temp.data,order.by=datefields.dt)
return(data.ts)
}

#########################################################################################################################################################################
## This convert bloomberg data to timeseries function is redundant and bloomberg automatically creates an xts(?) object. You can see this by looking at the first
## columns returned which says something like internal date
#########################################################################################################################################################################

convert.bbg.to.timeseries <- function(original.data)
{
num.columns <- dim(original.data)[2]
temp.data<- original.data[,2:num.columns]
datefields.dt <- strptime(original.data[,1],"%d-%m-%y")

data.ts<<-xts(x=temp.data,order.by=datefields.dt)
return(data.ts)
}



###############################################################################
##         Drift Coefficient Diagnostic Chart
###############################################################################
##############################################################################
plot.drift.coef.diagnostic <- function(time.series,numItv=11)
{

minVal<-min(time.series)
maxVal<-max(time.series)
#numItv <- 11 # numItv determines how many buckets the observations should be split into 
#write.csv(x=numItv,file="C://Users//Maeve//Documents//vix futures prices//text.csv")

interval.locations<-seq(minVal, maxVal, length.out=numItv) # creates a vector defining the end points of the intervals

difference.ts <- c(diff(time.series),0) # this determine the change in the variable

interval.in.which.observation.sits<-findInterval(x=time.series, vec=interval.locations)

x<-tapply(X=time.series,INDEX=interval.in.which.observation.sits,FUN=median) # finds the middle of each range
y<-tapply(X=difference.ts,INDEX=interval.in.which.observation.sits,FUN=mean) # finds the middle of each range

fit <- lm(y~x+0) # fits a straight line to the data and makes it pass through zero
fit2 <- lm(y~x+I(x^2)+I(x^3)+0) # fits a straight line to the data and makes it pass through zero
#fit3 <- lm(y~x+I(1/x)) # fits a straight line to the data and makes it pass through zero



plot(x,y,main="Residual Diagnostic Chart",lwd=3,xlab="Time series buckets",ylab="Change in Time Series") # plots the diagnostic chart
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
abline(fit,lwd=3,col="RED")
# This code adds the second regression model to the plot. Note that the prediction function can only take a dataframe as an argument so you need to coerce it
lines(x,predict(fit2,as.data.frame(x)),lwd=3,col="BLUE")
#lines(x,predict(fit3,as.data.frame(x)),col="GREEN")


}
getYAHOOdata <- function(ticker.required.code,start.dt,end.dt,frequency="DAILY")
{
#default.data.field <-getSymbols(c('F'),from = start.dt,to =end.dt,auto.assign=FALSE)
#number.of.observations<-dim(matrix(weekdays(time(default.data.field[,4]))))[1]

#weekly.dates<- matrix(weekdays(time(default.data.field[,4]))) == matrix(rep("Friday",number.of.observations))

number.of.variables<-dim(matrix(ticker.required.code))[1]

dataset <- data.frame(matrix(NA,nrow=0,ncol=0))

	for (ticker.no in 1:number.of.variables)
	{

temp.var<-getSymbols(ticker.required.code[ticker.no],from = start.dt,to =end.dt,auto.assign=FALSE)
dataset<-cbind(dataset,temp.var[,4])

	}
colnames(dataset)<-c(ticker.required.code)
if (frequency=="WEEKLY")
{return(dataset[weekly.dates,])}
else
{return(dataset)}
}

#########################################################################################################################################################################
## Plots the factor loadings for each input variable
#########################################################################################################################################################################

plot.factor.loadings2 <-function(prcomp.obj,ticker.required,type="b")
{
#y<-c("red","blue","green","black","purple")[1:dim(prcomp.obj$rotation)[2]]#original working code for four colours only
#colours<-rep(y,dim(prcomp.obj$rotation)[1])#original working code for four colours only
	num.of.variables <- 5#dim(prcomp.obj$rotation)[2]

	y<- colours.for.barcharts[1:dim(prcomp.obj$rotation)[2]]
	colours<-rep(y,dim(prcomp.obj$rotation)[1])
	factor.loadings<-(summary(prcomp.obj)[[2]])
	ticker.required<-strip.ticker.suffixes(ticker.required)
	row.names(factor.loadings)<-c(ticker.required)

#	number.of.factors.used<-dim(prcomp.obj$rotation)[2]
	number.of.observations<-dim(prcomp.obj$rotation)[1]

if (type =="b")
{
	barplot(t(factor.loadings),ylab="Exposure of Variable to Factor", col=colours, main="Factor Exposures",beside=TRUE,ylim=c(round(min(factor.loadings),digits=2),round(max(factor.loadings),digits=2)))
}
else
{
	plot(t(factor.loadings)[1,],lwd=3,ylab="Exposure of Variable to Factor",type="l", col=colours[1], main="Factor Exposures",ylim=c(round(min(factor.loadings),digits=2),round(max(factor.loadings),digits=2)))
	for (i in 2:num.of.variables)
		{
		lines(t(factor.loadings)[i,],lwd=3,ylab="Exposure of Variable to Factor", col=colours[i], main="Factor Exposures",ylim=c(round(min(factor.loadings)),round(max(factor.loadings))))
		}
}
	grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
	legend("topright", legend=paste("Eig",1:num.of.variables),fill = colours.for.barcharts)
}

first.passage.time.density<-function(spread,tgt.level)
{
# first fit the sde - default is OU

# not use fitted function to create 1000 simulations

#for each simulation count number of time periods taken to hit target level

#take the average and return that as first passage time density

#also give me the variance of that number
}


#########################################################################################################################################################################
## Calculates forecasted future means for OU process
#########################################################################################################################################################################


ou.expected.value<-function(last.value,mu=1,lambda=1)
{
result<-last.value*exp(lambda*-1)+mu*(1-exp(lambda*-1)) #    expected value
return(result)
}


#########################################################################################################################################################################
## calculates standard deviation of variable for Orstein Uhlen beck process
#########################################################################################################################################################################


ou.std.dev<-function(steps.ahead=0,sigma,lambda)
{
sigma_SQD <- sigma^2 
result<-sigma_SQD/2/lambda*(1-exp(lambda*-2*steps.ahead))
return(result)
}


#########################################################################################################################################################################
## forecasts confidence intervals for mean reverting OU process
#########################################################################################################################################################################


forecast.rangs.OU<-function(last.value_INT=0,steps.ahead=1,mu_INT,lambda_INT,sigma_INT)
{
expected.values<-array(rep(NA,steps.ahead))
variances<-array(rep(NA,steps.ahead))

expected.values[1]<-ou.expected.value(last.value=last.value_INT,mu=mu_INT,lambda=lambda_INT)
variances[1]<-ou.std.dev(1,sigma=sigma_INT,lambda=lambda_INT)

if(steps.ahead>1)
	{
		for (i in 2:steps.ahead)
		{
		expected.values[i] <- ou.expected.value(last.value=expected.values[i-1],mu=mu_INT,lambda=lambda_INT)
		variances[i] <- ou.std.dev(i,sigma=sigma_INT,lambda=lambda_INT)
		}
	}

#	expected.values<-expected.values-last.value_INT
	standard.deviations<- sqrt(variances)
	
return(list(expected.values=expected.values,standard.deviations=standard.deviations))

}


#########################################################################################################################################################################
## create charts for forecasting confidence intervals
#########################################################################################################################################################################


plot.forecast.OU<-function(historic.values,forecasted.values,std.dev)
{

num.hist.obs = length(as.array(historic.values))
num.fcast.obs = length(as.array(forecasted.values))

#last.value<-historic.values[num.hist.obs]

hist.range=1:num.hist.obs
fcast.range<-(num.hist.obs+1):(num.hist.obs+num.fcast.obs)

hist.plot.vals<-array(NA,dim=num.hist.obs+num.fcast.obs)
hist.plot.vals[hist.range]<-historic.values

fcast.plot.vals<-array(NA,dim=num.hist.obs+num.fcast.obs)
fcast.plot.vals[fcast.range]<-forecasted.values

upper2sd.plot.vals<-array(NA,dim=num.hist.obs+num.fcast.obs)
upper1sd.plot.vals<-array(NA,dim=num.hist.obs+num.fcast.obs)
lower1sd.plot.vals<-array(NA,dim=num.hist.obs+num.fcast.obs)
lower2sd.plot.vals<-array(NA,dim=num.hist.obs+num.fcast.obs)

upper2sd.values<-forecasted.values+2*std.dev
upper1sd.values<-forecasted.values+1*std.dev
lower1sd.values<-forecasted.values-1*std.dev
lower2sd.values<-forecasted.values-2*std.dev

upper2sd.plot.vals[fcast.range]<-upper2sd.values
upper1sd.plot.vals[fcast.range]<-upper1sd.values
lower1sd.plot.vals[fcast.range]<-lower1sd.values
lower2sd.plot.vals[fcast.range]<-lower2sd.values

plot(hist.plot.vals, type = "l", col = "black", main = "Forecast Graph")
points(fcast.plot.vals, type = "l",col = "red",lwd=3) # adds to the previous chart
points(upper2sd.plot.vals, type = "l",col = "blue") # adds to the previous chart
points(upper1sd.plot.vals, type = "l",col = "light blue") # adds to the previous chart
points(lower2sd.plot.vals, type = "l",col = "blue") # adds to the previous chart
points(lower1sd.plot.vals, type = "l",col = "light blue") # adds to the previous chart
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
#legend("topleft", legend=c("Historic","Forecast","lower 2sd","lwr 1sd","upper 1sd","upper 2sd"),fill = c("black","red","dark blue","light blue","light blue","dark blue")
}

#########################################################################################################################################################################
## create charts for forecasting confidence intervals
#########################################################################################################################################################################

#GB00B3MYD345

plot.ou.expected.path<-function(timeseries)
{
ou.fit<-fit.OU_exact(timeseries)
last.pca.value<-timeseries[length(array(timeseries))]

fcast<-forecast.rangs.OU(last.value_INT=last.pca.value,steps.ahead=100,mu_INT=ou.fit$mu,lambda_INT=ou.fit$lambda,sigma_INT=ou.fit$sigma)
plot.forecast.OU(timeseries,fcast$expected.values,fcast$standard.deviations)

}

#########################################################################################################################################################################
## calculates the hitting time for an ou process
#########################################################################################################################################################################



ou.hitting.time <- function(tgt.value,stop.loss,max.sims=10,timeseries)
{
ou.fit<-fit.OU_exact(timeseries)
last.pca.value<-timeseries[length(array(timeseries))]
num.periods.fcast<-1000
#num.periods.fcast<-ou.fit$life.to99pct

theta1<-ou.fit$lambda * ou.fit$mu
theta2<-ou.fit$lambda
theta3<-ou.fit$sigma
set.seed(123)
sims<-sde.sim(X0=last.pca.value ,T=num.periods.fcast, M=max.sims, N=1000, model="OU",theta=c(theta1,theta2,theta3)) 

	series.tested<-sims[,1]
	final.results<-((unlist(first.row.that.meets.criterion(series.tested,tgt.lvl=0,stop.loss.lvl=last.pca.value*2))))
	
#	final.results
for (counter in 2:max.sims)
	{
	series.tested<-sims[,counter]
	results<-first.row.that.meets.criterion(series.tested,tgt.lvl=0, stop.loss.lvl=last.pca.value*2)
	final.results = rbind(final.results, unlist(results))
	}
#	hitting.times<-apply(X=final.results,FUN=mean,MARGIN=2)

	return(final.results)
}

#########################################################################################################################################################################
## find first row in time series that meets the criterion
#########################################################################################################################################################################


first.row.that.meets.criterion <- function (timeseries,tgt.lvl=0,stop.loss.lvl)
{


timed.out<-FALSE
stop.loss.hit<-FALSE
tgt.hit<-FALSE
multiplier<-0




	timeseries<-array(timeseries)
	num.rows<-length(array(timeseries))
	length.of.time<-1
	#ou.fit<-fit.OU_exact(timeseries)
	#mean.value<-ou.fit$mu
	mean.value <- mean(timeseries)

#if (stop.loss.lvl==NULL) stop.loss.lvl * timeseries[1]*2

	if (timeseries[1]>mean.value )
		multiplier<- (1)
	else
		multiplier<- (-1)

	while (multiplier*timeseries[length.of.time] > multiplier*tgt.lvl)
	{

		
		if (length.of.time >= num.rows) break
		
		if(multiplier*timeseries[length.of.time] > multiplier*stop.loss.lvl) break
		length.of.time <- length.of.time + 1
	}


	tgt.hit<-(multiplier*timeseries[length.of.time] <= multiplier*tgt.lvl)
	stop.loss.hit<-(multiplier*timeseries[length.of.time] > multiplier*stop.loss.lvl)
	timed.out<-(length.of.time >= num.rows)
	
	return(list(length.of.time=length.of.time, timed.out=timed.out, stop.loss.hit=stop.loss.hit, tgt.hit=tgt.hit))
	
}

#########################################################################################################################################################################
## third friday of the month
#########################################################################################################################################################################


third.friday <- function(YYmmdd){
d <- seq(as.Date(YYmmdd), by = 1, length.out = 30)
result<-sort(as.Date(tapply(d[which(format(d, '%a')=='Fri')], 
                    format(d[which(format(d, '%a')=='Fri')], '%m-%Y'),
                    function(x) x[3]), origin='1970-01-01'))
return(result)
}

#########################################################################################################################################################################
## finds the vix expiry of the month in YYmmdd 
#########################################################################################################################################################################


vix.expiry <- function(YYmmdd){
datetext <- YYmmdd
nxt.mth <- nextmonth(datetext)
next.mth.1st <- paste(substr(nxt.mth,1,8),"01",sep="")
sp500.option.expiry <- third.friday(next.mth.1st)
year.of.YYmmdd <- year(YYmmdd)
#easter<-Easter(year = year.of.YYmmdd, shift = 0)
#easter.test<-difftimeDate(time1=sp500.option.expiry, time2=easter, units = "days")
#if (easter.test==0) sp500.option.expiry<-sp500.option.expiry-1

vix.future.expiry<-sp500.option.expiry-30
#easter<-Easter(year = year.of.YYmmdd, shift = 0)
return(vix.future.expiry)
}

#########################################################################################################################################################################
## finds the vix expire of the last and next 6 contracts
#########################################################################################################################################################################


vix.expiry.chain <- function(YYmmdd){
current.YYmmdd<-YYmmdd
vix.expiry.lst <- list(rep(NULL,7))



if(vix.expiry(current.YYmmdd) <= current.YYmmdd){
vix.expiry.lst[[1]] <- vix.expiry(current.YYmmdd)
#current.YYmmdd <- nextmonth(current.YYmmdd)
}
else
{
temp.YYmmdd<-as.Date(paste(substr(YYmmdd,1,8),"01",sep=""))-1
vix.expiry.lst[[1]] <- vix.expiry(temp.YYmmdd)
}

for (counter in 1:6){
	vix.expiry.current.date <- vix.expiry(current.YYmmdd)
	days.to.next.expiry <- as.Date(vix.expiry.current.date) - as.Date(current.YYmmdd)

	if(days.to.next.expiry<=0){
		vix.expiry.current.date <- vix.expiry(nextmonth(current.YYmmdd))
		}
	else{
		vix.expiry.current.date <- vix.expiry(current.YYmmdd)
		}


	names(vix.expiry.current.date)<-paste("VixContract",counter,sep="_")
	vix.expiry.lst[[counter+1]]<-vix.expiry.current.date
	current.YYmmdd <- nextmonth(current.YYmmdd)

	}

return(vix.expiry.lst)
}

#########################################################################################################################################################################
## number of days in current roll period
#########################################################################################################################################################################




dt <- function(input.dt){

rollout.mth<- vix.expiry.chain(input.dt)[[1]]
rollin.mth <- vix.expiry.chain(input.dt)[[2]]

days.in.period <- as.Date(rollin.mth)-as.Date(rollout.mth)
#return(as.numeric(days.in.period),units="days")
days.in.period
}


nextmonth <- function(YYmmdd){
# ymd function parses dates in year-month-day format
startDate <- ymd(YYmmdd)
# The %m+% adds months to dates without exceeding the last day
myDates <- as.Date(startDate %m+% months(c(0:1)))[2]
return(myDates)

}


#########################################################################################################################################################################
## days left in current roll period
#########################################################################################################################################################################

dr <- function(input.dt){

rollin.mth <- vix.expiry.chain(input.dt)[[2]]

days.to.go <- as.Date(rollin.mth)-as.Date(input.dt)

return(days.to.go)
#return(as.numeric(days.to.go),units="days")

}




#******************************************************************************************************
#*
#*
#*                                Get Variance for Bloomberg Ticker (not working prperly yet!)
#*
#*
#*******************************************************************************************************

getBBGVariance <-function(ticker)
{
conn <- blpConnect()     # sets up the connection for bloomberg
Sys.setenv(TZ="GMT")     # Tells Bloomberg to use Green Mean Time


	fieldList <- data.frame(bds(conn,ticker,"OPT_CHAIN")) # fieldlist contains tickers for all options
	parsed.option.codes <- data.frame(do.call(rbind.data.frame, strsplit(as.character(fieldList[,1]), " ")))
	#temp1 contains the the fields downloaded from the option chain parsed into seperate columns

	colnames(parsed.option.codes)<-NULL

	number.of.columns <- dim(parsed.option.codes)[2]
	temp1 <-	cbind(fieldList,parsed.option.codes)

	if(number.of.columns==5)
	{
	type.strike.col <- 4
	temp1 <- temp1[-c(3)]
	}
else
	{
	type.strike.col <- 3
	}
	
	CallPut<- substr(parsed.option.codes[,type.strike.col],1,1)

	temp2<-cbind(temp1,CallPut)
	strike <- as.numeric(substr(parsed.option.codes[,type.strike.col],2,5))

	temp3<- cbind(temp2,strike)

	lastbidask <- bdp(conn, securities=temp3[,1], fields=c("PX_LAST","PX_BID","PX_ASK"))

	fieldtype <- bdp(conn, ticker, fields=c("SECURITY_TYP2"))
	if (fieldtype[1,1]=="Index")
	{ 
	lastbidask[,2] <- lastbidask[,1]
	lastbidask[,3] <- lastbidask[,1]
	}

	temp5<-cbind(temp3,lastbidask)

	temp6<-cbind(temp5,(temp5[,9]+temp5[,10])*0.5)
	names(temp6)<-list("Security","Ticker","Expiry","Ignore1","InstrumentType","CallPut","Strike","Last","PX_BID","PX_ASK","PX_MID")


	temp6$Expiry<-strptime(temp6$Expiry,"%m/%d/%y")
	option.chain <- temp6

	write.table(option.chain,row.names=TRUE,"c:/temp/test.txt")

	Calls <- option.chain[option.chain$CallPut=="C",]
	Puts  <- option.chain[option.chain$CallPut=="P",]
#	CombinedCallPut <- merge(x=Calls, y=Puts, by.x=c("Expiry","Strike"),by.y=c("Expiry","Strike"))
	CombinedCallPut <- merge(x=Calls, y=Puts, by.x=c("Expiry","Strike","Ticker"),by.y=c("Expiry","Strike","Ticker"))

	diffprice <- abs(CombinedCallPut$PX_MID.x - CombinedCallPut$PX_MID.y)
	CombinedCallPut <- cbind(CombinedCallPut,diffprice)
	CombinedCallPut <- CombinedCallPut[!names(CombinedCallPut) %in% c("Security.y","Security.y")]
	CombinedCallPut  <- CombinedCallPut[!is.na(CombinedCallPut$diffprice),]
	Expiry.dates <- unique(option.chain$Expiry)
	no.of.expiry.dates <- length(Expiry.dates)

	Underlying_lastbidask <-	bdp(conn, securities=ticker, fields=c("PX_LAST","PX_BID","PX_ASK"))
	bidask.not.available <- !is.numeric(Underlying_lastbidask$PX_ASK) || !is.numeric(Underlying_lastbidask$PX_BID)
	if(bidask.not.available)
	{
	spot_mid<-Underlying_lastbidask$PX_LAST
	}
	else
	{
	spot_mid<- 0.5* (Underlying_lastbidask$PX_ASK + Underlying_lastbidask$PX_BID)
	}

	write.table(CombinedCallPut,row.names=TRUE,"c:/temp/CombinedCallPut.txt")
#	for(expiry.no in 1:no.of.expiry.dates)
	for(expiry.no in 1:2)
	{
	CurrentExpiry <- Expiry.dates[expiry.no]
	option <- CombinedCallPut[CombinedCallPut$Expiry==CurrentExpiry,]
	option <- option[order(option$Expiry,option$Strike),]
	minoption<-min(option$diffprice)

	ForwardPrice <- option[option$diffprice == minoption,]$Strike
#	Underlying_lastbidask <-	bdp(conn, securities=ticker, fields=c("PX_LAST","PX_BID","PX_ASK"))


	Time.to.Expiry <- abs(as.numeric(difftime(Sys.Date(),as.Date(CurrentExpiry),units="days"),units="days")/365)
	putstrikes	<-matrix(data=option[option$Strike<ForwardPrice,]$Strike)
	callstrikes	<-matrix(data=option[option$Strike>ForwardPrice,]$Strike)
	put_prices  <-matrix(data=option[option$Strike<ForwardPrice,]$PX_MID.y)
	callprices  <-matrix(data=option[option$Strike>ForwardPrice,]$PX_MID.x)
	risk.free.rate <- 0.01
#	print(Time.to.Expiry)

	Variance <- (2/Time.to.Expiry) * exp(-risk.free.rate*Time.to.Expiry)*(sum(callprices* 1/(callstrikes^2))+sum(put_prices* 1/(putstrikes^2)))

	Volatility <- sqrt(Variance)
	NumericExpiry<-(CurrentExpiry$year+1900)*10000+(CurrentExpiry$mon+1)*100+CurrentExpiry$mday
	if (expiry.no==1)
		{
		VarianceSwaps <- c(NumericExpiry,Volatility,Variance)
		}
	else
		{
		VarianceSwaps <- rbind(VarianceSwaps, c(NumericExpiry,Volatility,Variance))
		}


} 
	return(VarianceSwaps)
}

#******************************************************************************************************
#*
#*
#*                                Get Variance for Bloomberg Ticker (not working prperly yet!)
#*
#*
#*******************************************************************************************************

getBBGVarianceFast <-function(ticker, TICKERS_ALREADY_DOWNLOADED=TRUE, risk.free.rate=0.01)
{
#conn <- blpConnect()     # sets up the connection for bloomberg
#Sys.setenv(TZ="GMT")     # Tells Bloomberg to use Green Mean Time

#	fieldList <- data.frame(bds(conn,ticker,"OPT_CHAIN")) # fieldlist contains tickers for all options
	fieldList <- bds(ticker,fields="OPT_CHAIN") #RBLPAPI VERSION variable field list contains the tickers for all the options
	option_tickers<-c(unlist(fieldList)) #  this command converts the 'list' of tickers into a vector 
if (TICKERS_ALREADY_DOWNLOADED==TRUE)
{
option.chain <- read.table(file="c:/temp/testdata.txt", header=TRUE,sep=",")
}
else
{
#	option.chain <- bdp(conn, option_tickers, fields=c("SECURITY_DES", "OPT_EXPIRE_DT","OPT_STRIKE_PX","OPT_PUT_CALL","PX_LAST","PX_BID","PX_ASK","PX_MID","OPT_UNDL_PX"))
#
	option.chain <- bdp(securities = option_tickers,                
	                    fields=c("SECURITY_DES", "OPT_EXPIRE_DT","OPT_STRIKE_PX",
						         "OPT_PUT_CALL", "PX_LAST",      "PX_BID",
								 "PX_ASK",       "PX_MID",       "OPT_UNDL_PX")) 


	write.table(option.chain,row.names=TRUE,"c:/temp/testdata.txt",sep=",")
}

# 	write.table(option.chain,row.names=TRUE,"c:/temp/testdata.txt")
	##trim all NAs
	option.chain <- option.chain[is.na(option.chain$PX_MID)==FALSE,]

#	write.table(option.chain,row.names=TRUE,"c:/temp/testdata.txt")

	Calls <- option.chain[option.chain$OPT_PUT_CALL=="Call",]
#	LaggedCalls <- c(NA,Calls[-nrow(Calls),])

	Puts  <- option.chain[option.chain$OPT_PUT_CALL=="Put",]

	CombinedCallPut <- merge(x=Calls, y=Puts, by.x=c("OPT_EXPIRE_DT","OPT_STRIKE_PX","OPT_UNDL_PX"),by.y=c("OPT_EXPIRE_DT","OPT_STRIKE_PX","OPT_UNDL_PX"))

	diffprice <- abs(CombinedCallPut$PX_MID.x - CombinedCallPut$PX_MID.y)
	CombinedCallPut <- cbind(CombinedCallPut,diffprice) 
	CombinedCallPut <- CombinedCallPut[!names(CombinedCallPut) %in% c("Security.y","Security.y")]
	CombinedCallPut  <- CombinedCallPut[!is.na(CombinedCallPut$diffprice),]
	Expiry.dates <- unique(option.chain$OPT_EXPIRE_DT)
	no.of.expiry.dates <- length(Expiry.dates)

#	write.table(CombinedCallPut,row.names=TRUE,"c:/temp/CombinedCallPutv2.txt")

	for(expiry.no in 1:6)
	{
	CurrentExpiry <- Expiry.dates[expiry.no]
	option <- CombinedCallPut[CombinedCallPut$OPT_EXPIRE_DT==CurrentExpiry,]
	option <- option[order(option$OPT_EXPIRE_DT,option$OPT_STRIKE_PX),]
	minoption<-min(option$diffprice)

	ForwardPrice <- option[option$diffprice == minoption,]$OPT_STRIKE_PX

	Time.to.Expiry <- abs(as.numeric(difftime(Sys.Date(),as.Date(CurrentExpiry),units="days"),units="days")/365)
	putstrikes	<-matrix(data=option[option$OPT_STRIKE_PX<=ForwardPrice,]$OPT_STRIKE_PX)
	callstrikes	<-matrix(data=option[option$OPT_STRIKE_PX>ForwardPrice,]$OPT_STRIKE_PX)
	put_prices  <-matrix(data=option[option$OPT_STRIKE_PX<=ForwardPrice,]$PX_MID.y)
	callprices  <-matrix(data=option[option$OPT_STRIKE_PX>ForwardPrice,]$PX_MID.x)
#	risk.free.rate <- 0.01

	Variance <- (2/Time.to.Expiry) * (exp(-risk.free.rate*Time.to.Expiry)*sum(callprices * 1/(callstrikes^2))+sum(put_prices* 1/(putstrikes^2)))

	Volatility <- sqrt(Variance)

	CurrentExpiry <-	strptime(CurrentExpiry,"%Y-%m-%d")
	NumericExpiry<-(CurrentExpiry$year+1900)*10000+(CurrentExpiry$mon+1)*100+CurrentExpiry$mday
	if (expiry.no==1)
		{
#		VarianceSwaps <- c(NumericExpiry,Volatility,Variance)	
		VarianceSwaps <- data.frame(CurrentExpiry,Volatility,Variance,Time.to.Expiry*365,ForwardPrice)
		}
	else
		{
	#	VarianceSwaps <- rbind(VarianceSwaps, c(NumericExpiry,Volatility,Variance))
		VarianceSwaps <- rbind(VarianceSwaps,data.frame(CurrentExpiry,Volatility,Variance,Time.to.Expiry*365,ForwardPrice))

		}


} 
	return(VarianceSwaps)
}
##################################################################################################################
##  R doesn't have a lag function so I downloaded this one from the internet
##
##################################################################################################################
shift<-function(x,shift_by){
    stopifnot(is.numeric(shift_by))
    stopifnot(is.numeric(x))
 
    if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
 
    out<-NULL
    abs_shift_by=abs(shift_by)
    if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
    else if (shift_by < 0 )
        out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
    else
        out<-x
    out
}


cvt_idx_to_rtns <- function (data)
{
	dataset <-data[-1,2:dim(data)[2]] /data[-dim(data)[1],2:dim(data)[2]]-1
	dates   <-data[2:dim(data)[1],1]
	dataset<-cbind(dates,dataset)
	return(dataset) 
}

hallerbach.summary.table<-function(rtns,tgt_wgts,cap_wgts)
{
#var_names <- colnames(rtns)

num_vars<-dim(xs_rtns)[2]
capwtd_rtn <- as.matrix(xs_rtns)%*%cap_wgts
tgtwtd_rtn <- as.matrix(xs_rtns)%*%tgt_wgts


betas<-matrix(ncol=num_vars,nrow=1,)

for (counter in 1:num_vars)
{
betas[counter]<-coef(lm(xs_rtns[,counter]~tgtwtd_rtn))[2]
}


Sigma<-cov(xs_rtns)
mgnl_risk_contribution <- mrc(tgt_wgts, Sigma, percentage = TRUE)

cap_wgtd_port_variance <-var(capwtd_rtn)
cap_wgtd_port_rtn <-mean(capwtd_rtn)

tgt_wgtd_port_variance <-var(tgtwtd_rtn)

#colnames() <- colnames()



relative_risk_aversion_coefficient <- cap_wgtd_port_rtn / cap_wgtd_port_variance
factor_variances<-apply(xs_rtns, 2, var)


implied_risk_premiums_mthly<-as.numeric(relative_risk_aversion_coefficient) * betas * as.numeric(tgt_wgtd_port_variance)
implied_risk_premiums<-((1+implied_risk_premiums_mthly)^12-1)*100
factor_stdevs<-apply(xs_rtns, 2, sd)*sqrt(12)*100


implied_sharpes<-implied_risk_premiums/factor_stdevs

tgt_wgts.df<-as.data.frame(t(tgt_wgts))
colnames(tgt_wgts.df) <- colnames(rtns)

betas.df<-as.data.frame(betas)
colnames(betas.df) <- colnames(rtns)

mgnl_risk_contribution.df<-as.data.frame(t(mgnl_risk_contribution))
colnames(mgnl_risk_contribution.df) <- colnames(rtns)

implied_risk_premiums.df<-as.data.frame(implied_risk_premiums)
colnames(implied_risk_premiums.df) <- colnames(rtns)

implied_sharpes.df<-as.data.frame(implied_sharpes)
colnames(implied_sharpes.df) <- colnames(rtns)


obj <- list(Weight=tgt_wgts.df, Beta=betas.df, Pct_Risk_Contrib=mgnl_risk_contribution.df,Implied=implied_risk_premiums.df,Implied_sharpe=implied_sharpes.df)
return (obj)

}



#ch <- getBBGVarianceFast("ukx Index")  
#ch <- getBBGVarianceFast("spx Index")                                     
#ch <- getBBGVarianceFast("svxy Equity",TEST=TRUE)
#ch <- getBBGVarianceFast("VIX INDEX")
#ch <- getBBGVarianceFast("uvxy Equity")

#undebug(getBBGVarianceFast)

