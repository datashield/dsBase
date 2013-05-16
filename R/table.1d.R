#' Generates 'valid' 1-dimensional table for all sources
#' 
#' @param a a numerical vector
#' @export
#' 

table.1d <- function (a) {
  tab<-table(a)
  tab.1d<-tab
  nc<-dim(tab)
  #tab.def tests for deficient cell count
  tab.def<-tab.1d*0
  
  #SET MINIMUM CELL SIZE TO 4 FOR REAL IMPLEMENTATION
  min.cell.size<-4
  for(k in 1:nc)
  {
    if(tab[k]<=min.cell.size && tab[k]>0){tab.def[k]<-(-1)}
  }
  
  tab.def.mat<-as.matrix(tab.def)
  names(dimnames(tab.def.mat))<-"Categories"
  dimnames(tab.def.mat)[2]<-"    Adequacy of cell counts"
  names(dimnames(tab.def.mat))[2]<-"COUNTS BETWEEN 1 & 4 IN CELLS DESIGNATED -1"
  
  
  
  tab.1d.mat<-as.matrix(tab.1d)
  names(dimnames(tab.1d.mat))<-"Categories"
  dimnames(tab.1d.mat)[2]<-"Counts"
  names(dimnames(tab.1d.mat))[2]<-"1D.TABLE"
  
  
  
  if(sum(tab.def)<0)matprint<-tab.def.mat
  if(sum(tab.def)==0)matprint<-tab.1d.mat
  matprint
}
