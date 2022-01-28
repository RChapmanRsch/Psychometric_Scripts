unique_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){toString(unlist(sort(unique(data[,y]))))}))}

missing_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){sum(is.na(data[,y]))}))}

length_list=function(data, x){return(sapply(x, simplify=TRUE, function(y){sum(!is.na(data[,y]))}))}

response_frequency_list=function(data, x, min=NA, max=NA){
	if(!is.na(min) & !is.na(max)){range=min(min):max(max)}
	else{range=min(data[,x], na.rm=TRUE):max(data[,x], na.rm=TRUE)}
  return(t(sapply(x, simplify=TRUE, function(y){table(factor(data[,y], levels=range))})))}

mean_sd=function(data, x){return(sapply(x,simplify=TRUE,function(y){paste0(round(mean(data[,y],na.rm=T),2),"(",round(sd(data[,y],na.rm=T),2),")")}))}

mean_sd_range=function(data, x){return(sapply(x,simplify=TRUE,function(y){paste0(
	round(mean(data[,y],na.rm=TRUE),2)," (",round(sd(data[,y],na.rm=TRUE),2),"), ",
    	as.character(round(min(data[,y],na.rm=TRUE),2)),"-",as.character(round(max(data[,y],na.rm=TRUE),2)))}))}

categical_n_percent=function(data, x){  return(sapply(x,simplify=TRUE,function(y){data.frame(matrix(
	paste0(table(data[,y])," (",round(prop.table(table(data[,y]))*100,0),"%)"),
	 nrow=1, dimnames = list(NA,names(table(data[,y])))))}))}

floor_list=function(data, x, min=NA, collapsed=NA){	
	return(sapply(x, simplify=TRUE, function(y){
		if(is.na(min) | y %in% collapsed){min=range(data[,y], na.rm=TRUE)[1]}
		round(sum(data[,y] %in% min)/length(data[,y]),2)}))}

ceiling_list=function(data, x, max=NA, collapsed=NA){return(sapply(x, simplify=TRUE, function(y){
	if(is.na(max) | y %in% collapsed){max=range(data[,y], na.rm=TRUE)[2]}
	round(sum(data[,y] %in% max)/length(data[,y]),2)}))}

item_change=function(data){
	  return(
	  t(apply(data, 1, function(x){
	    temp=x
	    temp[is.na(temp)]=0
	    temp=diff(temp) 
	    temp[unique(c(which(is.na(x[-1])), which(is.na(x[-1]))+1))]=NA
	    temp
	  })))}

Outlier_n=function(data,x){sapply(x, function(y){z=scale(data[,y]);return(sum(which(abs(z)>3.29)));})}

Code_to_SPSS_Label <- function(Data){return(as.factor(names(attributes(Data)$labels)[match(Data, attributes(Data)$labels)]))}

Freq_Prop_Table <- function(Data)
{
  Freq_Tbl<-table(Data, useNA="always")
  Prop_Tbl<-round(prop.table(Freq_Tbl),2)*100
  Freq_Prop_Tbl<-Freq_Tbl
  for(i in 1:length(Freq_Prop_Tbl))
  {
    Freq_Prop_Tbl[i]<-paste0(Freq_Tbl[i], "(",Prop_Tbl[i],"%)")
  }
  
  return(Freq_Prop_Tbl)
}


SWtest=function(data, x){
  return(sapply(x, simplify=TRUE, function(y){
    z=shapiro.test(Scored_data$Anxiety_tscore)$p.value;
    if(z>0.01){z="<0.1"};
    as.character(z);}))}


#########VIF scripts below######
  VIF<-function(LM){
    list_of_dvs=names(LM$model)[!(names(LM$model) %in% as.character(LM$terms[[2]]))]
      sapply(list_of_dvs, simplify=TRUE, function(y)
        {(1/(1-summary(lm(formula(paste0(y,"~ .")), data=temp))$r.squared))})}


thetaSE.eap<-function(ipar,resp.data,maxCat=5,model=1,minTheta=-4.0,maxTheta=4.0,inc=0.1,prior.dist=1,prior.mean=0.0,prior.sd=1.0,D=1.0) {
  ni<-nrow(ipar); #number of items
  nExaminees<<-nrow(resp.data); #number of examiness
  NCAT<-ipar$NCAT;
  
  theta<-seq(minTheta,maxTheta,inc);
  nq<-length(theta);
  
  if (prior.dist==1) {
    prior<-dnorm((theta-prior.mean)/prior.sd); #normal prior
  } else if (prior.dist==2) {
    prior<-exp((theta-prior.mean)/prior.sd)/(1+exp((theta-prior.mean)/prior.sd))^2; #logistic prior
  } else if (prior.dist==3) {
    prior<-rep(1,nq); #uniform prior
  }
  DISC<-ipar[["a"]];
  CB<-ipar[paste("cb",1:(maxCat-1),sep="")];
  
  prep.prob<-function(){
    pp<-array(0,c(nq,ni,maxCat));
    if (model==1) {
      for (i in 1:ni) {
        ps<-matrix(0,nq,NCAT[i]+1);
        ps[,1]<-1;
        ps[,NCAT[i]+1]<-0;
        for (k in 1:(NCAT[i]-1)) {
          ps[,k+1]<-1/(1+exp(-D*DISC[i]*(theta-CB[i,k])));
        }
        #pp[,i,1]<-1-ps[,1];
        #pp[,i,NCAT[i]]<-ps[,NCAT[i]];
        for (k in 1:NCAT[i]) {
          pp[,i,k]=ps[,k]-ps[,k+1];
        }
      }
    } else if (model==2) {
      for (i in 1:ni) {
        cb<-unlist(CB[i,]);
        cb<-c(0,cb);
        zz<-matrix(0,nq,NCAT[i]);
        sdsum<-0;
        den<-rep(0,nq);
        
        for (k in 1:NCAT[i]) {
          sdsum<-sdsum+cb[k];
          zz[,k]<-exp(D*DISC[i]*(k*theta-sdsum));
          den<-den+zz[,k];
        }
        for (k in 1:NCAT[i]) {
          pp[,i,k]<-zz[,k]/den;
        }
      }
    }
    
    return(pp);
  }
  
  pp<-prep.prob();
  
  calcEAP<-function() {
    posterior<-matrix(rep(prior,nExaminees),nExaminees,nq,byrow=T);
    
    for (i in 1:ni) {
      resp<-matrix(resp.data[,i],nExaminees,1);
      prob<-t(pp[,i,resp]);
      prob[is.na(prob)]<-1.0
      posterior<-posterior*prob;
    }
    EAP<-as.vector(posterior%*%theta/rowSums(posterior));
    SE<-as.vector(sqrt(rowSums(posterior*(matrix(theta,nExaminees,nq,byrow=T)-matrix(EAP,nExaminees,nq))^2)/rowSums(posterior)));
    return(list(theta=EAP,SE=SE))
  }
  return(data.frame(calcEAP()));
}


ThetaSEeap_wrapper<-function(IPAR, Variables, Data)
{
  ThetaSEeap_table<-thetaSE.eap(IPAR[Variables,2:ncol(IPAR)], Data[,Variables], maxCat = max(IPAR$NCAT))
  ThetaSEeap_table$raw_sum_score<-apply(Data[,Variables], 1, sum)
  
  ThetaSEeap_table$TScore <- NA
  ThetaSEeap_table$TScore <- ThetaSEeap_table$theta*10+50
  ThetaSEeap_table$SE <- ThetaSEeap_table$SE*10
  
  
  
  for(i in 1:nrow(ThetaSEeap_table))
  {
  if(!is.na(ThetaSEeap_table$raw_sum_score[i]))
    {
    ThetaSEeap_table$TScore[i] <- ThetaSEeap_table$theta[i]*10+50
    ThetaSEeap_table$SE[i] <- ThetaSEeap_table$SE[i]*10
  }
    else
    {
      ThetaSEeap_table$theta[i] <- NA
      ThetaSEeap_table$SE[i] <- NA  
    }
  }
  return(ThetaSEeap_table)
}


rsss<-function(ipar,model=1,minTheta=-4.0,maxTheta=4.0,inc=0.01,prior.mean=0.0,prior.sd=1.0,D=1.0,maxCat=5,minScore=1,Tscore=T){
	NCAT<-ipar[,"NCAT"]
	DISC<-ipar[,"a"]
	CB<-ipar[paste("cb",1:(maxCat-1),sep="")]

	ni<-dim(ipar)[1] # number of items

	theta<-seq(minTheta,maxTheta,by=inc) # populate theta vector
	nq<-length(theta) # number of quadrature points
	
	pp<-array(0,c(nq,ni,maxCat))

	if (model==1) {
		for (i in 1:ni) {
			ps<-matrix(0,nq,NCAT[i]+1);
			ps[,1]<-1;
			ps[,NCAT[i]+1]<-0;
			for (k in 1:(NCAT[i]-1)) {
				ps[,k+1]<-1/(1+exp(-D*DISC[i]*(theta-CB[i,k])));
			}
			pp[,i,1]<-1-ps[,1];
			pp[,i,NCAT[i]]<-ps[,NCAT[i]];
			for (k in 1:NCAT[i]) {
				pp[,i,k]=ps[,k]-ps[,k+1];
			}
		}
	} else if (model==2) {
		for (i in 1:ni) {
			cb<-unlist(CB[i,]);
			cb<-c(0,cb);
			zz<-matrix(0,nq,NCAT[i]);
			sdsum<-0;
			den<-rep(0,nq);

			for (k in 1:NCAT[i]) {
				sdsum<-sdsum+cb[k];
				zz[,k]<-exp(D*DISC[i]*(k*theta-sdsum));
				den<-den+zz[,k];
			}
			for (k in 1:NCAT[i]) {
				pp[,i,k]<-zz[,k]/den;
			}
		}
	}

	min.Raw.Score<-0 #minimum obtainable raw score
	max.Raw.Score<-sum(ipar[,"NCAT"])-ni #maximum obtainable raw score

	nScore<-max.Raw.Score-min.Raw.Score+1 #number of score points
	TCCinv<-numeric(nScore) #initialize TCC scoring table
	Raw.Score<-min.Raw.Score:max.Raw.Score #raw scores

	LH<-matrix(0,nq,nScore) #initializing distribution of summed scores

	ncat<-ipar[1,"NCAT"]
	maxScore<-0
	LH[,1:ncat]<-pp[,1,1:ncat]
	idx<-ncat

	for (i in 2:ni) {
		ncat<-ipar[i,"NCAT"] #number of categories for item i
		maxScore<-ncat-1 #maximum score for item i
		score<-0:maxScore #score values for item i
		prob<-pp[,i,1:ncat] #category probabilities for item i

		pLH<-matrix(0,nq,nScore) #place holder for LH

		for (k in 1:ncat) {
			for (h in 1:idx) {
				sco<-Raw.Score[h]+score[k]
				position<-which(Raw.Score==sco)
				pLH[,position]<-pLH[,position]+LH[,h]*prob[,k]
			}
		}
		idx<-idx+maxScore
		LH<-pLH
	}

	Scale.Score<-numeric(nScore) #score table for EAP
	SE<-numeric(nScore) #SE for EAP

	prior<-dnorm((theta-prior.mean)/prior.sd)
	posterior<-LH*prior #posterior distribution
	den<-colSums(posterior)
	den<-matrix(rep(den,rep(nq,nScore)),nq,nScore)
	posterior<-posterior/den

	for (j in 1:nScore) {
		Scale.Score[j]<-sum(posterior[,j]*theta)/sum(posterior[,j]) #EAP
		SE[j]<-sqrt(sum(posterior[,j]*(theta-Scale.Score[j])^2)/sum(posterior[,j])) #EAP
	}

	if (minScore==1) Raw.Score<-Raw.Score+ni

	if (Tscore) {
		Scale.Score=round(Scale.Score*10+50,1)
		SE=round(SE*10,1)
	}


	rsss.table<-data.frame(Raw=Raw.Score,Scale=Scale.Score,SE)

	return(rsss.table)
}


RSSS_scoring_wrapper<-function(IPAR, Variables, Data)
{
  RSSS_table<-rsss(IPAR[Variables,2:ncol(IPAR)])
  
  RSSS_scoring<-data.frame(matrix(NA,ncol=3,nrow=nrow(Data)))
  names(RSSS_scoring)<-c("raw_sum_score","TScore","SE")
  RSSS_scoring$raw_sum_score<-apply(Data[,Variables], 1, sum, na.rm = FALSE)
  RSSS_scoring$SD<-apply(Data[,Variables], 1, sd)
  
  for(i in 1:nrow(RSSS_table))
  {
    matched_scores=which(RSSS_scoring$raw_sum_score==RSSS_table$Raw[i])
    RSSS_scoring$TScore[matched_scores]<-RSSS_table$Scale[i]
    RSSS_scoring$SE[matched_scores]<-RSSS_table$SE[i]
  }
  
  return(RSSS_scoring)
  
}

AlphaAlphaDropOmegaHECV=function(Data){
  Alpha=tryCatch(alpha(Data), error=function(e){list("total"=list("std.alpha"=NA),"alpha.drop"=list("std.alpha"=NA))})
  OmegaECV=tryCatch(omegah(Data, plot=FALSE), error=function(e){list("omega_h"=NA,"ECV"=NA)})
  return(c(
  "Alpha"=round(Alpha$total$std.alpha,2),
  "AlphaDrop"=round(range(Alpha$alpha.drop$std.alpha),2),
  "OmegaH"=round(OmegaECV$omega_h,2),
  "ECV"=round(OmegaECV$ECV,2)))
}

#Function to calculate n, mean, sd and Cohen's by known group
KG_MeanSDES = function(KG_Data, KG_By){

  
  
  KG_Out=data.frame(matrix(NA,nrow=length(levels(as.factor(KG_By))),ncol=0))
  if(!is.data.frame(KG_Data)){KG_Data=as.data.frame(KG_Data)}  
  for(Scale in names(KG_Data)){
    MeanSDES=data.frame(
      "n"=tapply(KG_Data[,Scale], KG_By, function(x) length(na.omit(x))),
      "mean"=tapply(KG_Data[,Scale], KG_By, mean,na.rm=TRUE),
      "sd"=tapply(KG_Data[,Scale], KG_By, sd,na.rm=TRUE),
      "es"=NA)
    for(row in 1:(nrow(MeanSDES))){
      if(row!=nrow(MeanSDES)){MeanSDES$es[row]=
        (MeanSDES$mean[row]-MeanSDES$mean[row+1])/sqrt(
        ((MeanSDES$n[row]-1)*MeanSDES$sd[row]^2+(MeanSDES$n[row+1]-1)*MeanSDES$sd[row+1]^2)/
        (MeanSDES$n[row]+MeanSDES$n[row+1]-2))}} 
    names(MeanSDES)=paste(Scale,names(MeanSDES),sep=".")
    KG_Out=data.frame(KG_Out,MeanSDES)}
  return(KG_Out)}
