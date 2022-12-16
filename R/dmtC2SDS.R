#' @title Copy a clientside data.frame, matrix or tibble (DMT) to the serverside.
#' @description Creates a data.frame, matrix or tibble on the serverside
#' that is equivalent to that same data.frame, matrix or tibble (DMT) on the clientside.
#' @details dmtC2SDS is a serverside assign function called by ds.dmtC2S.
#' For more information about how it works see help for ds.dmtC2S
#' @param dfdata.mat.transmit a character string in a format that can pass through
#' the DataSHIELD R parser which specifies the name of the DMT
#' to be copied from the clientside to the serverside. Value fully specified by
#' <dfdata> argument of ds.dmtC2S.
#' @param inout.object.transmit a character string taking values "DF", "MAT" or "TBL".
#' The value of this argument is automatically set by ds.dmtC2S depending on
#' whether the clientside DMT is a data.frame, matrix or tibble.
#' Correspondingly, its value determines
#' whether the object created on the serverside is a data.frame, matrix or tibble.
#' This is unlikely to always work (some class misspecifications may occur)
#' but it works in all the test cases.
#' @param from a character string specifying the source of <dfdata>.
#' Fixed by clientside function as "clientside.matdftbl".
#' @param nrows.transmit specifies the number of rows in the matrix to be created.
#' Fixed by the clientside function as equal to the number of rows in
#' the clientside DMT to be transferred.
#' @param ncols.transmit specifies the number of columns in the matrix to be created.
#' Fixed by the clientside function as equal to the number of columns in
#' the clientside DMT to be transferred.
#' @param colnames.transmit a parser-transmissable vector specifying the name of each column
#' in the DMT being transferred from clientside to serverside.
#' Generated automatically by clientside function from colnames of clientside DMT.
#' @param colclass.transmit a parser-transmissable vector specifying the class of the
#' vector representing each individual column in the DMT to be transferred.
#' Generated automatically by clientside function. This allows the transmission of DMTs
#' containing columns with different classes.If something is going to go wrong with
#' class misspecification (see inout.object.transmit) it is a DMT with a complex
#' combination of data/column types that will most likely be the cause. This suggests
#' that you always check the class of the serverside DMT and its individual columns
#' (if the latter is important). If a situation arises where the class of the columns
#' is crucial and the function cannot do what is needed please contact the DataSHIELD
#' forum  and we can try to remedy the problem.
#' @param byrow a logical value specifying whether the DMT created on the serverside
#' should be filled row by row or column by column. This is fixed by the clientside
#' function as FALSE (fill column by column).
#' @return the object specified by the <newobj> argument (or default name
#' "matdftbl.copied.C2S") which is written as a  data.frame, matrix or tibble
#' to the serverside.
#' @author Paul Burton for DataSHIELD Development Team - 3rd June, 2021
#' @export
#'
dmtC2SDS <- function(dfdata.mat.transmit, inout.object.transmit, from, nrows.transmit, ncols.transmit,
                             colnames.transmit,colclass.transmit, byrow){

    # Check Permissive Privacy Control Level.
    dsBase::checkPermissivePrivacyControlLevel(c('permissive'))

#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
#thr<-dsBase::listDisclosureSettingsDS()                                #
#nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
#nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
#nfilter.subset<-as.numeric(thr$nfilter.subset)                         #
#nfilter.string<-as.numeric(thr$nfilter.string)                         #
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)               #
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################

    #<from> has been forced to be clientside.matdf

    dfdata.mat.text<-strsplit(dfdata.mat.transmit, split=",")

    dfdata.mat<-eval(parse(text=dfdata.mat.text)) #"get" alternative

    serverside.vector<-dfdata.mat

    #Evaluate inout.object.transmit
    inout.object.text<-strsplit(inout.object.transmit, split=",")

    if(inout.object.text!="MAT"&&inout.object.text!="DF"&&inout.object.text!="TBL")
    {
        studysideMessage<-"FAILED - OBJECT TO COPY FROM CLIENT TO SERVER MUST BE A MATRIX, DATA.FRAME OR TIBBLE"
        return(list(studysideMessage=studysideMessage))
    }

    #Evaluate nrows.transmit
    nrows.text<-strsplit(nrows.transmit, split=",")
    nrows.c<-eval(parse(text=nrows.text))
    nrows<-as.numeric(nrows.c)

    #Evaluate ncols.transmit
    ncols.text<-strsplit(ncols.transmit, split=",")
    ncols.c<-eval(parse(text=ncols.text))
    ncols<-as.numeric(ncols.c)

    #convert columns to correct class
    colclass.vector<-unlist(strsplit(colclass.transmit, split=","))
    colnames.vector<-unlist(strsplit(colnames.transmit, split=","))

    #convert 2 matrix
    #serverside.matrix<-matrix(as.numeric(serverside.vector),nrow=nrows,ncol=ncols)

    serverside.matrix<-matrix(serverside.vector,nrow=nrows,ncol=ncols)

    colnames(serverside.matrix)<-colnames.vector

    for(j in 1:ncols)
    {
        class(serverside.matrix[,j])<-colclass.vector[j]
    }

    serverside.df<-data.frame(serverside.matrix)

    serverside.tbl<-dplyr::tibble(serverside.matrix)

    for(m in 1:ncols)
    {
        activate.text<-paste0("as.",colclass.vector[m],"(as.character(serverside.df[,",m,"]))")
        serverside.df[,m]<-eval(parse(text=activate.text))
    }

    colnames(serverside.df)<-colnames.vector

    if(inout.object.text=="MAT")
    {
        return(serverside.matrix)
    }

    if(inout.object.text=="DF")
    {
        return(serverside.df)
    }

    if(inout.object.text=="TBL")
    {
        return(serverside.tbl)
    }
}

#ASSIGN FUNCTION
# dmtC2SDS
