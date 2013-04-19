#' Generates a 2D table
#'
#' @param a a numerical vector 
#' @param b a numerical vector
#' @return a 2d table
#' @author Burton, P.
#' @export
#' 
table.2d <- function (a,b) {
  tab<-table(a,b)
  tab.2d<-tab
  nr<-dim(tab)[1]
  nc<-dim(tab)[2]
  
  #tab.def tests for deficient cell count
  tab.def<-tab.2d*0
  
  #SET MINIMUM CELL SIZE TO 4 FOR REAL IMPLEMENTATION
  min.cell.size<-4
  for(j in 1:nr)
  {
    for(k in 1:nc)
    {
      if(tab[j,k]<=min.cell.size && tab[j,k]>0){tab.def[j,k]<-(-1)}
    }
  }
  
  tab.def.mat<-as.matrix(tab.def)
  names(dimnames(tab.def.mat))<-list("Var1","Var2")
  
  
  tab.2d.mat<-as.matrix(tab.2d)
  names(dimnames(tab.2d.mat))<-list("Var1","Var2")
  
  
  if(sum(tab.def)<0)matprint<-tab.def.mat
  if(sum(tab.def)==0)matprint<-tab.2d.mat
  matprint
}