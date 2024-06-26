require(reshape2)
require(plyr)
require(ggplot2)
require(scales)

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

rescale_df=function(data,groupvar=NULL){
  if(is.null(groupvar)) df=data
  else df=data[,-which(names(data) %in% groupvar)]
  
  select=sapply(df,is.numeric)
  df[select]=lapply(df[select], scales::rescale)
  if(!is.null(groupvar)) {
    df=cbind(df,data[[groupvar]])
    colnames(df)[length(df)]=groupvar
  }        
  df
}


ggRadar=function(data=iris,
                 xvars=NULL,
                 yvar=NULL,autorescale=FALSE,
                 groupvar=NULL,legend.position="bottom",
                 radar=TRUE,polar=FALSE,
                 mean=TRUE,nrow=FALSE,
                 colour="red"){
  if(is.null(xvars)) {
    select=sapply(data,is.numeric)
    xvars=colnames(data)[select]
  }
  if(is.null(yvar)){
    # if(!is.null(groupvar)) {
    #         for(i in 1:length(groupvar)) data[[groupvar[i]]]=factor(data[[groupvar[i]]])
    # }
    # data
    if(autorescale) data=rescale_df(data,groupvar)
    longdf=melt(data,id.vars=groupvar,measure.vars=xvars)
    longdf
    if(mean)
      df=ddply(longdf,c(groupvar,"variable"),summarize,mean(value,na.rm=TRUE))
    if(nrow) 
      df=ddply(longdf,c(groupvar,"variable"),"nrow") 
    
    colnames(df)[length(df)]="value"
    #print(df)
  } else{
    longdf=data
  }
  
  if(is.null(groupvar)){
    p<-ggplot(data=df,aes_string(x="variable",y="value",group=1))+
      geom_point(size=3,colour=colour)+
      geom_polygon(colour=colour,fill=colour,alpha=0.4)
    
  } else {
    df=df[!(df$variable %in% groupvar),]
    for(i in 1:length(groupvar)) df[[groupvar[i]]]=factor(df[[groupvar[i]]])
    p<-ggplot(data=df,aes_string(x="variable",y="value",
                                 colour=groupvar,fill=groupvar,group=groupvar))+
      geom_point(size=3)+
      geom_polygon(alpha=0.4)
  }        
  p<- p+ xlab("")+ylab("")+theme(legend.position=legend.position)
  
  if(radar==TRUE) p<-p+coord_radar()
  if(polar==TRUE) p<-p+coord_polar()    
  p    
}