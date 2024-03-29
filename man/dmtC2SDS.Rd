% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dmtC2SDS.R
\name{dmtC2SDS}
\alias{dmtC2SDS}
\title{Copy a clientside data.frame, matrix or tibble (DMT) to the serverside.}
\usage{
dmtC2SDS(
  dfdata.mat.transmit,
  inout.object.transmit,
  from,
  nrows.transmit,
  ncols.transmit,
  colnames.transmit,
  colclass.transmit,
  byrow
)
}
\arguments{
\item{dfdata.mat.transmit}{a character string in a format that can pass through
the DataSHIELD R parser which specifies the name of the DMT
to be copied from the clientside to the serverside. Value fully specified by
<dfdata> argument of ds.dmtC2S.}

\item{inout.object.transmit}{a character string taking values "DF", "MAT" or "TBL".
The value of this argument is automatically set by ds.dmtC2S depending on
whether the clientside DMT is a data.frame, matrix or tibble.
Correspondingly, its value determines
whether the object created on the serverside is a data.frame, matrix or tibble.
This is unlikely to always work (some class misspecifications may occur)
but it works in all the test cases.}

\item{from}{a character string specifying the source of <dfdata>.
Fixed by clientside function as "clientside.matdftbl".}

\item{nrows.transmit}{specifies the number of rows in the matrix to be created.
Fixed by the clientside function as equal to the number of rows in
the clientside DMT to be transferred.}

\item{ncols.transmit}{specifies the number of columns in the matrix to be created.
Fixed by the clientside function as equal to the number of columns in
the clientside DMT to be transferred.}

\item{colnames.transmit}{a parser-transmissable vector specifying the name of each column
in the DMT being transferred from clientside to serverside.
Generated automatically by clientside function from colnames of clientside DMT.}

\item{colclass.transmit}{a parser-transmissable vector specifying the class of the
vector representing each individual column in the DMT to be transferred.
Generated automatically by clientside function. This allows the transmission of DMTs
containing columns with different classes.If something is going to go wrong with
class misspecification (see inout.object.transmit) it is a DMT with a complex
combination of data/column types that will most likely be the cause. This suggests
that you always check the class of the serverside DMT and its individual columns
(if the latter is important). If a situation arises where the class of the columns
is crucial and the function cannot do what is needed please contact the DataSHIELD
forum  and we can try to remedy the problem.}

\item{byrow}{a logical value specifying whether the DMT created on the serverside
should be filled row by row or column by column. This is fixed by the clientside
function as FALSE (fill column by column).}
}
\value{
the object specified by the <newobj> argument (or default name
"matdftbl.copied.C2S") which is written as a  data.frame, matrix or tibble
to the serverside.
}
\description{
Creates a data.frame, matrix or tibble on the serverside
that is equivalent to that same data.frame, matrix or tibble (DMT) on the clientside.
}
\details{
dmtC2SDS is a serverside assign function called by ds.dmtC2S.
For more information about how it works see help for ds.dmtC2S
}
\author{
Paul Burton for DataSHIELD Development Team - 3rd June, 2021
}
