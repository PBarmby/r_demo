pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  setwd(directory)
  goodval<-vector()
  for(i in id) {
    filen<- paste(sprintf("%03d",i),'.csv',sep="")
#    print(filen)
    data = read.csv(filen)
    meas = data[pollutant]
    goodmeas = meas[!is.na(meas)]
#    print(goodmeas)
    goodval<-append(goodval,goodmeas) 
#    print(goodval)
  }
  mean(goodval)
}
