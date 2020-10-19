ds <- index(pca.ts[,1])
#y <-  pca.ts[,1]
y <-  pca.ts[,2]
#y <-  pca.ts[,3]

#rates.df<- window(x=rates.ts.all,start=as.Date("2017-01-01"),end=as.Date("2020-06-30")) 
rates.df<- window(x=rates.ts.all,start=as.Date("2017-01-01"),end=as.Date("2019-12-30")) 
pca <- prcomp(x=rates.df,center=TRUE,scale=FALSE,retx=TRUE,tol=0.00001,rank=number.of.PCA.factors.to.use) # carries out the principal components analysis
pca.ts<-xts(pca$x,order.by=index(rates.df))

length(y)
length(ds)

df <- data.frame(ds,y)
names(df)<-c('ds','y')
m<-prophet(df)
future<-make_future_dataframe(m,182)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)



xts(forecast['yhat'],order.by = as.Date(future))
#par(mfrow=c(1,1))
plot(m, forecast)

rates.df2<- window(x=rates.ts.all,start=as.Date("2017-01-01"),end=as.Date("2020-06-30")) 
pca2 <- prcomp(x=rates.df2,center=TRUE,scale=FALSE,retx=TRUE,tol=0.00001,rank=number.of.PCA.factors.to.use) # carries out the principal components analysis

#print(pca$rotation)
pca2.ts<-xts(pca2$x,order.by=index(rates.df2))
#plot(pca.ts,main="All PCs", col=colours.for.barcharts)
#plot(pca.ts[,1:1],main="2nd PCAs",col=colours.for.barcharts[1:1])
qplot(y=pca2.ts[,2:2],x=index(pca2.ts[,2:2]),main="2nd PCAs",col=colours.for.barcharts[2:2])
#plot(pca.ts[,3:3],main="3rd PCAs",col=colours.for.barcharts[3:3])



