#This script contains a function, 'Generate_Random_Data_from_Response_Probabilities.R',
#which simulates data based on random sampling from  Graded Response Model response probabilities
#across theta. This script is intended to be used with the 'Calculate_Response_Probabilities' script

#function 'Generate_Random_Data_from_Response_Probabilities.R' requires a 'Response_Probabilities'
#list object created by the 'Calculate_Response_Probabilities' script, which contains a matrix of
#response probabilities across theta for each item. 

#The'Generate_Random_Data_from_Response_Probabilities.R' script outputs a dataframe with:
#rows for each person, with a simulated 'True' Tscore sampled based on a normal distribution 
#columns for each item, with random data sampled from item response options and weighted by probabilities for a given 'True' Tscore
 
Generate_Random_Data_from_Response_Probabilities<-function(N, Response_Probabilities, mean=50, sd=10, ExistingData=NA)
{
  #'N'= sample size to simulate for random dataset
  #'Response_Probabilities'= list object of each item's response option probabilities across theta/tscore
  #'mean'= mean of simulated 'True' Tscores
  #'sd'= standard deviation of simulated 'True' Tscores
  
 if(is.na(ExistingData){
   #create simulated 'True' 'Tscore', based on normal distribution of Tscores  
   Tscore<-round(rnorm(N, mean, sd))}
  else{Tscore=ExistingData}
  
  #trim 'True' Tscores that happen to fall outside of our measurement range of 10-90
  Tscore[which(Tscore<10)]<-10
  Tscore[which(Tscore>90)]<-90
  
  #create a dataframe 'random_data' with: 
  #number of columns = number of items, length(Response_Probabilities)
  #number of rows = sample size, length(Tscore)
  random_data<-data.frame(matrix(NA,ncol=length(Response_Probabilities),nrow=length(Tscore)))
  
  #for each simulated Tscore, represented by index 'i'...
  for(i in 1:length(Tscore))
  {
    #for each item in 'Response_Probabilities', represented by index 'j'
    for(j in 1:length(Response_Probabilities))
    {
      #simulated a single response based on the item's response options,
      #weighted by 'Response_Probabilities' for a given Tscore level
      random_data[i,j]<-as.numeric(sample(
        names(Response_Probabilities[[j]]),
        size=1,
        prob=Response_Probabilities[[j]][which(rownames(Response_Probabilities[[j]])==Tscore[i]),]))
    }
  }
  
  #rename columns in 'random_data' based on their corresponding items in 'Response_Probabilities'
  names(random_data)<-names(Response_Probabilities)
  #add 'true' Tscore variable onto the 'random_data' dataframe
  random_data$Tscore<-Tscore
  
  #function returns the simulated data, 'random_data'
  return(random_data)
}
