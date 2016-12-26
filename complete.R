# This programs uses a data set of fine particulate pollution data collected in the US
# This data is from 332 pollution monitors around the country for a period of an year
# The data from each monitor is available as a csv file
# THe data is available for download at:
# https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip
# THis program accepts as input a number series that can go from 1 to 332
# and returns the number of observations in each file of that sequence of files

complete<-function(directory,id=1:332){
  
  #setting the working directory to specdata
  current_wd<-getwd()
  old_wd<-"C:/Users/krupakar_singampally/Documents"
  if (current_wd==old_wd){setwd(directory)}
  
  #Getting the total list of monitor files into a vector
  file_list<-dir()
  
  #initialize dataframe
  nobs_df<-data.frame(id=integer(),nobs=integer())

  
  for(i in 1:length(id)){
    #set the monitor value in column 1
    nobs_df[i,1]<-id[i]
  
    j=id[i] 
    
    file_i<-read.csv(file_list[j])  
    #Compute number of observations in that file
    No_NA<-sum(complete.cases(file_i))
    #Assign total observations to data frame column 2 
    nobs_df[i,2]<-No_NA
    
  }
  nobs_df
  
  
}
