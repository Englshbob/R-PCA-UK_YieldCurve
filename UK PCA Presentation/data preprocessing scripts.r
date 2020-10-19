## Pre-processing scripts for loading data
#First we need to load the tickers from either Bloomberg or from file into the dataframe. If you are #using Bloomberg then you need to choose DAILY OR WEEKLY OR MONTHLY.  I assume that the first column #always has the dates. You need to strip the dates from the first column. This is done in the second line

require(readxl)
require(xts)

#action <-"downloadallinflationdata"
action <- "downloadallnominaldata"
#action <- "downloadallswapdata"
#dataset <- "boe1979_2020"
getNewDailyBoEData<-FALSE
#dataset <- "German2s5s7s10s"

##################################################################################################################
###########################                  This loads the Bank of England data                             #####
##################################################################################################################

dataDirectory <- "/media/john/6E34B3A529A1D545/Data"

#"/media/john/6E34B3A529A1D545/Data/BoE Data/glcnominalddata.zip"
#paste(dataDirectory,"/BoE Data/glcnominalddata.zip",sep="")
#"/media/john/6E34B3A529A1D545/Data/BoE Data/latest-yield-curve-data.zip"
#paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep="")
#"/media/john/6E34B3A529A1D545/Data/BoE Data/latest-yield-curve-data.zip"
#paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep="")





if(action == "downloadallnominaldata"){
  
  
  if(!(file.exists(paste(dataDirectory,"/glcnominalddata.zip",sep="")))){
    reloadHistoricMonths <- TRUE
  }
  
  
  
  xurl  <-"https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/glcnominalddata.zip"
#  xdest <- paste(dataDirectory,"/BoE Data/glcnominalddata.zip",sep="")
   xdest <- paste(dataDirectory,"/glcnominalddata.zip",sep="")
  
## does the historic downloads file exist
    ## check to see if the month of latest historic download matches the current   month
  
  if (reloadHistoricMonths==TRUE){
    download.file(url=xurl, destfile=xdest, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
  }
  unzip(xdest,overwrite=TRUE,exdir=dataDirectory) 

  if (file.exists(paste(dataDirectory,"/temp.csv",sep=""))){
    file.remove(paste(dataDirectory,"/temp.csv",sep="")) # make sure that temp file is deleted
    }

  yield_curve_files <- as.character(unzip(paste(dataDirectory,"/BoE Data/glcnominalddata.zip",sep=""), list = TRUE)$Name)
  
  number.of.files <- length(yield_curve_files)
  
  xl_path <- paste(dataDirectory,"/",yield_curve_files[number.of.files],sep="") #identify last file
  r1 <- read_excel(path = xl_path, sheet = 6,skip=3,col_names=TRUE,progress=TRUE,n_max=0)[-1,]  # read the header from ltsa file
  write.table(r1,file=paste(dataDirectory,"/temp.csv",sep=""),sep=",",append=TRUE) # write headers in to the csv file
  
  for (counter in 1:number.of.files){
    xl_path <- paste(dataDirectory,"/",yield_curve_files[counter],sep="")     
    xlsx_load <- read_excel(path = xl_path, sheet = 6,skip=3,col_names =TRUE,progress=TRUE)[-1,]  
    write.table(x=xlsx_load, file=paste(dataDirectory,"/temp.csv",sep=""),append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
    file.remove(xl_path)
  }  
  write.csv2(x=format(Sys.time(), '%Y%m%d'), file=paste(dataDirectory,"/HistNominalYieldsLastLoadedOn.csv",sep=""))
  
  dateloaded <- read.csv2(paste(dataDirectory,"/","NominalYieldsLastLoadedOn.csv",sep=""))[2]
  if(dateloaded!=format(Sys.time(), '%Y%m%d')){
    xurl<-"https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip?la=en&hash=89B8A093FA97EF7DD79382044E15867840E45204"
    xdest<-paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep="")
    download.file(url=xurl, destfile=xdest, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
  }

    unzip(paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep=""),overwrite=TRUE,exdir=dataDirectory)   
  xl_path <- paste(dataDirectory,"/GLC Nominal daily data current month.xlsx",sep="")
  xlsx_load <- read_excel(path = xl_path, sheet = 6,skip=3,col_names =TRUE,progress=TRUE)[-1,]  
  write.table(x=xlsx_load, file=paste(dataDirectory,"/temp.csv",sep=""),append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
  file.remove(xl_path)  
  
  
  boe_rates <- read.csv(file=paste(dataDirectory,"/temp.csv",sep=""),sep=",",skip=0,header=TRUE)
  file.remove(paste(dataDirectory,"/temp.csv",sep="")) # delete the temp file
  
  for (counter in 2:(dim(boe_rates)[1])-1){
    if (is.na(boe_rates[(counter+1),3])){
      boe_rates[(counter+1),2:(dim(boe_rates)[2])] <- boe_rates[counter,2:(dim(boe_rates)[2])] # This fills the bank holidays with the last available value
    }
  }
  
  boe_rates <- boe_rates[-(1:1),-(2:2)] # remove column 2. i.e. the one with the 6mth yields
  #limitcolumns<- 1:80
  limitcolumns<- c(1,c((1:40)*2))
  #limitcolumns<- c(1,10,20,30,40,50,60,70,80)
  #boe_rates <- boe_rates[,c(9,19,29,39,49,59,69,79)] # limit columns to 5,10,15,..., 40 yrs
  boe_rates<-boe_rates[,limitcolumns]
  write.table(boe_rates,file="/media/john/6E34B3A529A1D545/Data/nominaldukrates.csv", row.names=FALSE, sep=",")

  file.remove("/media/john/6E34B3A529A1D545/Data/allukrates.csv")  
  file.copy(from="/media/john/6E34B3A529A1D545/Data/nominaldukrates.csv",to="/media/john/6E34B3A529A1D545/Data/allukrates.csv",overwrite=TRUE)
  
  #  rm(xl_path,xlsx_load,xurl,xdest,yield_curve_files,boe_rates, number.of.files,r1) # delete used objects
  #date <- read.csv2(paste(dataDirectory,"/","NominalYieldsLastLoadedOn.csv"))  

  write.csv2(x=format(Sys.time(), '%Y%m%d'), file=paste(dataDirectory,"/NominalYieldsLastLoadedOn.csv",sep=""))

}
if(action == "downloadallinflationdata"){
  
  xurl<-"https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/glcinflationddata.zip"
  xdest<-paste(dataDirectory,"/BoE Data/glcinflationddata.zip",sep="")
  if (reloadHistoricMonths==TRUE){
    download.file(url=xurl, destfile=xdest, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
  }
  unzip("/media/john/6E34B3A529A1D545/Data/BoE Data/glcinflationddata.zip",overwrite=TRUE,exdir=dataDirectory) 
  
  if (file.exists(paste(dataDirectory,"/temp.csv",sep=""))){
    file.remove(paste(dataDirectory,"/temp.csv",sep="")) # make sure that test file is deleted
  }
  
  yield_curve_files <- as.character(unzip(paste(dataDirectory,"/BoE Data/glcinflationddata.zip",sep=""), list = TRUE)$Name)
  
  number.of.files <- length(yield_curve_files)
  
  xl_path <- paste(dataDirectory,"/",yield_curve_files[number.of.files],sep="") #identify last file
  r1 <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names=TRUE,progress=TRUE,n_max=0)[-1,]  # read the header from ltsa file
  write.table(r1,file=paste(dataDirectory,"/temp.csv",sep=""),sep=",",append=TRUE) # write headers in to the csv file

  for (counter in 6:number.of.files){
    #for (counter in 1:number.of.files){
    xl_path <- paste(dataDirectory,"/",yield_curve_files[counter],sep="")     

    if(counter<=4){ 
     # print(excel_sheets(path = xl_path)[6])
      xlsx_load <- read_excel(path = xl_path, sheet = "4.  inf spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,]  
      }

    if(counter==5){
    #  print(excel_sheets(path = xl_path)[6])
      xlsx_load <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,]  
      }
      
    if(counter == 6){
      print(excel_sheets(path = xl_path))
      xlsx_load <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,] 
    }
    
    print(xl_path)
    print(excel_sheets(path = xl_path))

    write.table(x=xlsx_load, file=paste(dataDirectory,"/temp.csv",sep=""),append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
    
    file.remove(xl_path)
  }  
  
  
  dateloaded <- read.csv2(paste(dataDirectory,"/InflationYieldsLastLoadedOn.csv",sep=""))[2]
  if(dateloaded!=format(Sys.time(), '%Y%m%d')){
  
    xurl<-"https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip?la=en&hash=89B8A093FA97EF7DD79382044E15867840E45204"
    xdest<-paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep="")
    download.file(url=xurl, destfile=xdest, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
  }
    
    unzip(paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep=""),overwrite=TRUE,exdir=dataDirectory)   
    xl_path <- paste(dataDirectory,"/GLC Inflation daily data current month.xlsx",sep="")
    xlsx_load <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,]  
    write.table(x=xlsx_load, file=paste(dataDirectory,"/temp.csv",sep=""),append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
    file.remove(xl_path)  
  
    boe_inflation <- read.csv(file=paste(dataDirectory,"/temp.csv",sep=""),sep=",",skip=0,header=TRUE)
    file.remove(paste(dataDirectory,"/temp.csv",sep="")) # delete the temp file
  
  
    print("working here")
      for (counter in 2:(dim(boe_inflation)[1])-1){
          if (is.na(boe_inflation[(counter+1),5])){
              boe_inflation[(counter+1),2:(dim(boe_inflation)[2])] <- boe_inflation[counter,2:(dim(boe_inflation)[2])] # This fills the bank holidays with the last available value
          }
      }
  
    boe_inflation <- boe_inflation[-(1:1),-(2:6)]
    limitcolumns<- c(1,2,12,22,32,42,52,62,72)
    limitcolumns<- c(1,32:42)
    boe_inflation <- boe_inflation[-(1:1),limitcolumns]
    write.table(x=boe_inflation,file=paste(dataDirectory,"/inflationdukrates.csv",sep=""), row.names=FALSE, sep=",")
  
    file.remove(paste(dataDirectory,"/allukrates.csv",sep=""))
  
    file.copy(from=paste(dataDirectory,"/inflationdukrates.csv",sep=""),to=paste(dataDirectory,"/allukrates.csv",sep=""),overwrite=TRUE)
    rm(xl_path,xlsx_load,xurl,xdest,yield_curve_files, boe_inflation, number.of.files,r1) # delete used objects
  
    write.csv2(x=format(Sys.time(), '%Y%m%d'), file=paste(dataDirectory,"/","InflationYieldsLastLoadedOn.csv",sep=""))
  
}
if(action == "downloadallrealdata"){
  
  xurl<-"https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/glcrealddata.zip"
  xdest<-paste(dataDirectory,"/BoE Data/glcrealddata.zip",sep="")

  if (reloadHistoricMonths==TRUE){
    download.file(url=xurl, destfile=xdest, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
  }
  
  unzip(paste(dataDirectory,"/BoE Data/glcrealddata.zip",sep=""),overwrite=TRUE,exdir=dataDirectory) 
  
  if (file.exists(paste(dataDirectory,"/temp.csv",sep=""))){
    file.remove(paste(dataDirectory,"/temp.csv",sep="")) # make sure that test file is deleted
  }
  
  yield_curve_files <- as.character(unzip(paste(dataDirectory,"/BoE Data/glcrealddata.zip",sep=""), list = TRUE)$Name)
  
  number.of.files <- length(yield_curve_files)
  
  xl_path <- paste(dataDirectory,"/",yield_curve_files[number.of.files],sep="") #identify last file
  r1 <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names=TRUE,progress=TRUE,n_max=0)[-1,]  # read the header from ltsa file
  write.table(r1,file=paste(dataDirectory,"/temp.csv",sep=""),sep=",",append=TRUE) # write headers in to the csv file
  
  for (counter in 6:number.of.files){
    #for (counter in 1:number.of.files){
    xl_path <- paste(dataDirectory,"/",yield_curve_files[counter],sep="")     
    
    if(counter<=4){ 
      # print(excel_sheets(path = xl_path)[6])
      xlsx_load <- read_excel(path = xl_path, sheet = "4.  inf spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,]  
    }
    
    if(counter==5){
      #  print(excel_sheets(path = xl_path)[6])
      xlsx_load <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,]  
    }
    
    if(counter == 6){
      print(excel_sheets(path = xl_path))
      xlsx_load <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,] 
    }
    
    print(xl_path)
    print(excel_sheets(path = xl_path))
    
    write.table(x=xlsx_load, file=paste(dataDirectory,"/temp.csv",sep=""),append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
    
    file.remove(xl_path)
  }  

  dateloaded <- read.csv2(paste(dataDirectory,"/","RealYieldsLastLoadedOn.csv",sep=""))[2]
  if(dateloaded!=format(Sys.time(), '%Y%m%d')){
    
  xurl<-"https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves/latest-yield-curve-data.zip?la=en&hash=89B8A093FA97EF7DD79382044E15867840E45204"
  xdest<-paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep="")
  download.file(url=xurl, destfile=xdest, quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
  }
  unzip(paste(dataDirectory,"/BoE Data/latest-yield-curve-data.zip",sep=""),overwrite=TRUE,exdir=dataDirectory)   
  
  xl_path <- paste(dataDirectory,"/","GLC Real daily data current month.xlsx",sep="")
  xlsx_load <- read_excel(path = xl_path, sheet = "4. spot curve",skip=3,col_names =TRUE,progress=TRUE)[-1,]  
  write.table(x=xlsx_load, file=paste(dataDirectory,"/temp.csv",sep=""),append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
  file.remove(xl_path)  
  
  boe_inflation <- read.csv(file=paste(dataDirectory,"/temp.csv",sep=""),sep=",",skip=0,header=TRUE)
  file.remove(paste(dataDirectory,"/temp.csv",sep="")) # delete the temp file
  
  for (counter in 2:(dim(boe_inflation)[1])-1){
    if (is.na(boe_inflation[(counter+1),5])){
      boe_inflation[(counter+1),2:(dim(boe_inflation)[2])] <- boe_inflation[counter,2:(dim(boe_inflation)[2])] # This fills the bank holidays with the last available value
    }
  }
  
  boe_inflation <- boe_inflation[-(1:1),-(2:6)]
  #limitcolumns<- c(1,2,12,22,32,42,52,62,72)
  #limitcolumns<- c(1,32:52)
  limitcolumns<- c(1:72)
  boe_inflation <- boe_inflation[-(1:1),limitcolumns]
  
  write.table(x=boe_inflation,file=paste(dataDirectory,"/realdukrates.csv",sep=""), row.names=FALSE, sep=",")
  
  file.remove(paste(dataDirectory,"/allukrates.csv",sep="")) 
  
  file.copy(from=paste(dataDirectory,"/realdukrates.csv",sep=""),to=paste(dataDirectory,"/allukrates.csv",sep=""),overwrite=TRUE)
  rm(xl_path,xlsx_load,xurl,xdest,yield_curve_files, boe_inflation, number.of.files,r1) # delete used objects
  write.csv2(x=format(Sys.time(), '%Y%m%d'), file=paste(dataDirectory,"/","RealYieldsLastLoadedOn.csv",sep=""))
}


##################################################################################################################
########################### German 2s 5s 7s 10s data from Scaller Book                                       #####
##################################################################################################################

if (action == "German2s5s7s10s"){
  xl_path <- paste(dataDirectory,"/","PCA_Bund.xls",sep="")
  germanrates <- read_excel(path = xl_path, sheet = 1,range = "A9:E137",col_names=TRUE,progress=TRUE)
  write.table(germanrates,file="/media/john/6E34B3A529A1D545/Data/allukrates.csv", row.names=FALSE, sep=",")
  }


rates <-(read.csv(file="/media/john/6E34B3A529A1D545/Data/allukrates.csv", sep=",", header=TRUE))
dates<-as.Date(as.matrix(rates[,1]))

rates.ts.all<- (xts(x=rates[,-1], order.by=dates))






