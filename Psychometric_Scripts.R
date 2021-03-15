unique_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){toString(unlist(sort(unique(data[,y]))))}))}

missing_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){sum(is.na(data[,y]))}))}

length_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){length(data[,y])}))}

mean_sd=function(data, x){return(sapply(x,simplify=TRUE,function(y){paste0(round(mean(data[,y],na.rm=T),2),"(",round(sd(data[,y],na.rm=T),2),")")}))}

floor_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){round(sum(data[,y] %in% range(data[,y], na.rm=TRUE)[1])/length(data[,y]),2)}))}

ceiling_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){round(sum(data[,y] %in% range(data[,y], na.rm=TRUE)[2])/length(data[,y]),2)}))}

Outlier_n=function(data,x){sapply(x, function(y){z=scale(data[,y]);return(sum(which(abs(z)>3.29)));})}

SWtest=function(data, x){
  return(sapply(x, simplify=TRUE, function(y){
    z=shapiro.test(Scored_data$Anxiety_tscore)$p.value;
    if(z>0.01){z="<0.1"};
    as.character(z);}))}


