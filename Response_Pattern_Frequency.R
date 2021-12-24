
#pattern response frequency scripts:
response_frequency_sort=function(Data){
  Data[is.na(Data)]="?"
  temp=data.frame(table(apply(Data,1, paste, collapse="")))
  temp$SS=sapply(temp$Var1, simplify=TRUE,
                 function(x){sum(as.numeric(unlist(strsplit(as.character(gsub("[^0-9.-]", "", x)),split=""))))})
    temp$SD=sapply(temp$Var1, simplify=TRUE,
                   function(x){sd(as.numeric(unlist(strsplit(as.character(gsub("[^0-9.-]", "", x)),split=""))))})
  return(temp[order(temp$SS,temp$SD),])}
