
#Finding the mean of pollutant and ids entered by the user

pollutantmean<-function(pollutant,id=1:332){
  means<-vector()
  for (i in id){
      x<-paste('00',as.character(i),sep='')
      y<-substr(x,nchar(x)-2,nchar(x))
      df<-read.csv(paste(y,'.csv',sep=''))
      a<-mean(df[,pollutant],na.rm=TRUE)
      if (is.na(a)==FALSE){means<-c(means,a)}
  
      }
  
  mean(means)
  
  
  
}


#Finding the number of complete observations in each of the entered ids
complete<-function(id=1:332){
  nobs<-vector()
  for (i in id){
    num=0
    x<-paste('00',as.character(i),sep='')
    y<-substr(x,nchar(x)-2,nchar(x))
    df<-read.csv(paste(y,'.csv',sep=''))
    null_data<-is.na(df[,c('sulfate','nitrate')])
    for (j in 1:nrow(null_data)){
      if (null_data[j,1]==FALSE & null_data[j,2]==FALSE){num<-num+1}
    }
    nobs<-c(nobs,num)
    
    
  }
  nobs
  data.frame(i,nobs)
}




#Finding the correlation of the pollutants for ids having complete entries more than the 
#given threshold value

corr<-function(threshold=0){
  nobs<-vector()
  for (i in 1:332){
    num=0
    x<-paste('00',as.character(i),sep='')
    y<-substr(x,nchar(x)-2,nchar(x))
    df<-read.csv(paste(y,'.csv',sep=''))
    null_data<-is.na(df[,c('sulfate','nitrate')])
    for (j in 1:nrow(null_data)){
      if (null_data[j,1]==FALSE & null_data[j,2]==FALSE){num<-num+1}
    }
    if (num>threshold){
      c=cor(df[,'sulfate'],df['nitrate'],use='complete.obs')
      nobs<-c(nobs,c)
    }
  }
    nobs
    
}


pollutantmean('nitrate',2:25)
pollutantmean('sulphate')

complete(10:20)
complete()

corr(threshold=1000)
corr()
