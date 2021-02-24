#' 
#' @title lexisDS2
#' @description The second serverside function called by ds.lexis.
#' @details This is the assign
#' function which actually creates
#' the expanded dataframe containing surival data for a piecewise exponential
#' regression. lexisDS2 also
#' carries out a series of disclosure checks and if the arguments or data fail any of
#' those tests,
#' creation of the exapanded dataframe is blocked and an appropriate serverside error
#' message is stored.
#' For more details see the extensive header for ds.lexis.
#' @param datatext a clientside provided character string specifying the data.frame
#' holding the data set to be expanded
#' @param intervalWidth a clientside generated character string specifying the width
#' of the survival epochs in the
#' expanded data
#' @param maxmaxtime a clientside generated object specifying the maximum follow up
#' time in any of the sources
#' @param idCol a clientside generated character string specifying the variable
#' holding the IDs of indivuals in the data set to be expanded
#' @param entryCol a clientside specified character string identifying the variable
#' holding the time that each individual starts follow up
#' @param exitCol a clientside specified character string identifying the variable
#' holding the time that each individual ends follow up (is censored or fails)
#' @param statusCol a clientside specified character string identifying the variable
#' holding the final censoring status (failed/censored)
#' @param vartext is a clientside provided vector of character strings denoting the
#' column names of additional variables to include in the 
#' final expanded table. If the 'variables' argument is not set (is null) but the
#' 'data' argument is set the full data.frame will be expanded and carried forward
#' @author Burton PR
#' @export
#'
lexisDS2 <- function(datatext=NULL, intervalWidth, maxmaxtime, idCol, entryCol, exitCol, statusCol, vartext=NULL){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset<-as.numeric(thr$nfilter.subset)
  #nfilter.string<-as.numeric(thr$nfilter.string)
  #############################################################
  
  starttime <- eval(parse(text=entryCol), envir = parent.frame())
  endtime <- eval(parse(text=exitCol), envir = parent.frame())
  cens <- eval(parse(text=statusCol), envir = parent.frame())
  id.orig <- eval(parse(text=idCol), envir = parent.frame())
  
  starttime <- as.numeric(starttime)
  endtime <- as.numeric(endtime)
  
  if(is.factor(cens)){
    cens <- as.character(cens)
    cens <- as.numeric(cens)
  }
  
  CENS <- as.numeric(cens)
  SURVTIME <- endtime-starttime
  STARTTIME <- starttime
  ENDTIME <- endtime
  ID <- id.orig
  
  #CREATE A NEW ORDERED SEQUENTIAL ID - INDEPENDENT FROM ORIGINAL idCol
  #THIS WILL ENABLE SORTING REGARDLESS OF THE NATURE OF THE ORIGINAL idCol 
  #USE exitCol FOR LENGTH BECAUSE idCol MAY POTENTIALLY BE NULL
  idSeq <- 1:length(SURVTIME)
  length.collapseDF <- length(idSeq)
  
  #IDENTIFY VARIABLES TO BE CARRIED WITH THE EXPANDED SURVIVAL DATA
  
  if(is.null(vartext)){
    datatext2<-paste0("data.frame(",datatext,")")
    DF <- eval(parse(text=datatext2), envir = parent.frame())
  }
  
  if(!is.null(vartext)){
    vartext2<-paste0("data.frame(",vartext,")")
    DF<-eval(parse(text=vartext2), envir = parent.frame())
  }
  
  if(is.null(datatext)&&is.null(vartext)){
    DF<-data.frame(SURVTIME,CENS)
  }
  
  ###############################################
  #SCRIPT.TO.CHECK.VALIDITY.OF.EVALUATABLE.RCODE#
  ###############################################
  
  #SPECIFY ARGUMENT TO BE EVALUATED	
  code.input<-intervalWidth
  code.c<-unlist(strsplit(code.input, split=","))
  code.n<-as.numeric(code.c)
  
  #In this case, code must only contain numeric elements split by ",",
  #anything else will fail outright or will have returned an NA to
  #code.num and so the following sum will exceed 0
  
  if(sum(is.na(code.n))>0){
    studysideMessage<-"FAILED: IntervalWidth argument contains non-numerics"
    stop(studysideMessage, call. = FALSE)
  }else{
    
    intervalWidth<-code.n
  }
  
  #CHECK CREATED VECTOR IS NOT SO LONG THAT THERE IS A RISK OF USING IT
  #TO CREATE A CONVENIENT VECTOR TO INFER IDENTITY. BECAUSE THIS IS AN
  #ASSIGN FUNCTION AND - IN PRICIPLE - SUCH A VECTOR COULD POTENTIALLY
  #BE WRITTEN TO THE SERVER SIDE R ENVIRONMENTS. THOUGH NOT CLEAR HOW
  #IN PRACTICE THAT COULD BE DONE
  
  #COMPARE LENGTH OF ENCODED VECTOR TO MAX NUMBER OF PARAMETERS ALLOWED
  #IN A glm MODEL. NB THIS IS DELIBERATELY SET TO A STRANGE PROPORTION OF
  #THE SAMPLE SIZE IN EACH STUDY (0.37) SO THAT IT CANNOT BE USED TO CREATE
  #A VECTOR WHICH COULD FOR EXAMPLE BE COMBINED FOUR TIMES (IF PROPORTION
  #= 0.25) TO PRECISELY LINE UP TO LENGTH OF PRIMARY DATA VECTORS 
  
  num.intervals<-length(code.n)
  
  if(num.intervals>(nfilter.glm*length.collapseDF)){
    studysideMessage<-"FAILED: IntervalWidth vector is too long. It may be disclosive - please shorten"
    stop(studysideMessage, call. = FALSE)
  }
  ###############################################
  #                 END.OF.SCRIPT               #
  ###############################################
  
  
  #intervalWidth IS A SINGLE VALUE
  if(is.null(intervalWidth)||is.na(intervalWidth)||intervalWidth==0){
    return("A VALID NON-ZERO intervalWidth ARGUMENT MUST BE SPECIFIED") 
  }
  
  
  # if entry time is not provided set all entry times to 0
  if(is.null(entryCol)){
    entryCol <- "STARTTIME"
    STARTTIME <- rep(0, dim(DF)[1])
    DF <- data.frame(DF, STARTTIME)
  }
  
  DF.orig<-DF
  DF.add<-data.frame(idSeq,ID,STARTTIME,ENDTIME,SURVTIME,CENS)
  DF<-data.frame(DF.add,DF.orig)
  
  carry.data<-DF
  
  
  
  #GENERATE BREAKS FROM intervalWidth
  
  ###############################
  #NEED FIRST CALL FROM CLIENT SIDE TO GET MAX ENDTIME (+RANDOM MASK)
  #Here we set from study alone
  ##################################
  #COERCE CHARACTER VALUES OF intervalWidth INTO NUMERIC
  intervalWidth<-as.numeric(intervalWidth)
  
  #USE MAX TOTAL SURVIVAL TIME (WITH RANDOM +VE INCREMENT
  #ACROSS ALL STUDIES IN ANALYSIS TO DEFINE END OF FINAL PERIOD 
  max.end<-maxmaxtime
  
  #IF intervalWidth IS A SINGLE VALUE 
  if(length(intervalWidth)==1){
    numfullintervals<-floor(max.end/intervalWidth)
    start.breaks<-c(0,intervalWidth*(1:numfullintervals))
    end.breaks<-c(intervalWidth*(1:numfullintervals),max.end)
    numends<-numfullintervals
  }
  
  #IF intervalWidth IS A VECTOR
  if(length(intervalWidth)>1){
    numends<-length(intervalWidth)
    
    if(sum(intervalWidth)>=max.end){
      start.breaks<-c(0,cumsum(intervalWidth[1:(numends-1)]))
      end.breaks<-cumsum(intervalWidth)
    }		
    
    if(sum(intervalWidth)< max.end){
      start.breaks<-c(0,cumsum(intervalWidth))
      end.breaks<-c(cumsum(intervalWidth),max.end)
    }		
  }
  
  
  #STRIP BREAKS where start.break>=max.end
  end.breaks<-end.breaks[start.breaks<max.end]
  start.breaks<-start.breaks[start.breaks<max.end]
  period.surv<-end.breaks-start.breaks
  
  
  print(start.breaks)
  print(end.breaks)
  totints<-length(end.breaks)
  totsubs<-dim(DF)[1]
  
  print(totints)
  print(totsubs)
  
  
  survival.matrix<-matrix(data=0,totsubs,totints)
  cens.matrix<-matrix(0,totsubs,totints)
  idSeq.matrix<-matrix(0,totsubs,totints)
  
  
  #ERROR IN ORIGINAL lexisDS2 CORRECTED
  
  ###ERR numperiods.exposed<-rep(0,totsubs)
  first.int.exposed<-rep(1,totsubs)
  last.int.exposed<-rep(0,totsubs)
  
  for(j in 1:totints){
    ###ERR	numperiods.exposed<-numperiods.exposed+(start.breaks[j]<SURVTIME) #ERROR AS MISCOUNTS IF EXPLICIT (NON 0) STARTIME VECTOR SPECIFIED
    
    first.int.exposed<-first.int.exposed+(end.breaks[j]<STARTTIME)
    last.int.exposed<-last.int.exposed+(start.breaks[j]<=ENDTIME)
  }
  #NEED TO ADD DUMMY COLUMNS TO HARMLESSLY DIRECT NA VALUES IN numperiods.exposed
  
  
  for(m in 1:totints){
    
    survival.matrix[,m]<-	
      (#1
        ((last.int.exposed>first.int.exposed)*
           (
             ((m==first.int.exposed)*(end.breaks[m]-STARTTIME))+
               (((m>first.int.exposed)&(m<last.int.exposed))*period.surv[m])+
               ((m==last.int.exposed)*(ENDTIME - start.breaks[m]))
           ))+
          
          ((last.int.exposed==first.int.exposed)*
             (((m==first.int.exposed)&(m==last.int.exposed))*(ENDTIME-STARTTIME)))
      )#1			
    
    
    
    cens.matrix[,m]<-(m!=last.int.exposed)*0+
      (m==last.int.exposed)*CENS	
    idSeq.matrix[,m]<- idSeq
    
  }
  
  idSeq.vector<-as.vector(idSeq.matrix)
  cens.vector<-as.vector(cens.matrix)
  survival.vector<-as.vector(survival.matrix)
  
  
  cens.vector[survival.vector<=0]<-NA
  cens.matrix<-matrix(cens.vector,nrow=totsubs,ncol=totints)
  
  idSeq.vector[survival.vector<=0]<-NA
  idSeq.matrix<-matrix(idSeq.vector,nrow=totsubs,ncol=totints)
  
  
  survival.vector[survival.vector<=0]<-NA
  survival.matrix<-matrix(survival.vector,nrow=totsubs,ncol=totints)
  
  
  idSeq.vector<-as.vector(t(idSeq.matrix))
  cens.vector<-as.vector(t(cens.matrix))
  survival.vector<-as.vector(t(survival.matrix))
  
  ######################################################################
  ##########ERROR NEEDS CORRECTING
  time.id.vector<-stats::ave(idSeq.vector,idSeq.vector,FUN=seq_along)
  
  time.id.vector<-time.id.vector+first.int.exposed[idSeq.vector]-1
  ######################################################################
  
  
  expanded.template<-cbind(idSeq.vector,time.id.vector,survival.vector,cens.vector)
  
  expanded.template<-expanded.template[stats::complete.cases(expanded.template),]
  
  expanded.carry.data<-carry.data[expanded.template[,1],]
  
  expanded.ID<-dimnames(expanded.carry.data)[1]
  
  expanded.table<-data.frame(expanded.ID,expanded.template,expanded.carry.data)
  
  var.names.vector<-names(expanded.table)
  var.names.vector[1:11]<-c("UID.expanded", "IDSEQ","TIME.PERIOD", "SURVTIME","CENSOR","idseq.orig","id.orig","starttime.orig","endtime.orig","exposure.time.orig","cens.orig")
  names(expanded.table)<-var.names.vector
  
  ##########################################################################################
  #BEFORE RETURNING RESULT, CHECK THAT NO TIME INTERVAL HAS A DISCLOSIVE NUMBER OF FAILURES#
  ##########################################################################################
  
  time.intervals.invalid<-0
  
  tabvar<-table(time.id.vector,useNA="no")[table(time.id.vector,useNA="no")>=1]
  min.category<-min(tabvar)
  if(min.category<nfilter.tab)time.intervals.invalid<-1
  
  #TERMINATE CALCULATION IF time.intervals.invalid==1
  if(time.intervals.invalid==1){
    studysideMessage<-"FAILED: At least one time interval has too few failures - please change times"
    stop(studysideMessage, call. = FALSE)
  }
  
  
  return(list(expanded.table=expanded.table))
  
}
#ASSIGN FUNCTION
# lexisDS2
