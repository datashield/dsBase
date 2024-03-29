% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranksSecureDS5.R
\name{ranksSecureDS5}
\alias{ranksSecureDS5}
\title{Secure ranking of "V2BR" (vector to be ranked) across all sources}
\usage{
ranksSecureDS5(output.ranks.df)
}
\arguments{
\item{output.ranks.df}{a character string which specifies an optional name
for the data.frame written to the serverside on each data source that
contains 11 of the key output variables from the ranking procedure pertaining
to that particular data source. This argument is set by the argument with
the same name in ds.ranksSecure.}
}
\value{
extracts 5 key vectors from the larger data frame created by
ranksSecureDS4 to produce a summary data frame that is written to the
serverside. It is given a name specified by the argument.
}
\description{
Summarises the serverside data frame written by ranksSecureDS4
which is identified by the name given by the argument (<output.ranks.df>) of
the ds.ranksSecure function to produce a new output data frame containing
only 5 key variables.
}
\details{
Serverside assign function called by clientside function
ds.ranksSecure. Takes the serverside data frame written by ranksSecureDS4
which is identified by the name given by the argument (<output.ranks.df>) of
the ds.ranksSecure function. This holds 11 vectors including the final
global ranks across all studies and final global quantiles. The data frame
is ordered according to the argument <ranks.sort.by> in ds.ranksSecure. The
ranksSecureDS5 function then extracts 5 key vectors from the larger data
frame to produce a summary data frame that is given a name specified by
the argument (<summary.output.ranks.df>) the ds.ranksSecure function.
This data frame includes the following components: (1) The values of a
sequential ID variable (ID.seq.real.orig) created to lie alongside the
original V2BR vector in the same order as that vector was itself ordered.
These ID values therefore reflect which row in the original data corresponds
to a given row in the output. If the argument <ranks.sort.by> in
ds.ranksSecure is set to "ID.orig" the values of the ID.seq.real.orig vector
in the output data frame simply run sequentially from 1 to N where N is the
number of individuals in the corresponding study. If <ranks.sort.by> is
set to "vals.orig" the values of the ID.seq.real.orig vector will be
determined by the magnitude of the corresponding V2BR value and will
appear to be ordered in a haphazard manner; (2) the original values of V2BR;
(3) the global ranks corresponding to the original values in V2BR, with ties
reflected appropriately; (4) the global quantiles corresponding to the
original values in V2BR, with ties reflected appropriately; (5) a studyid
vector in which all elements take the value n in the nth study.
}
\author{
Paul Burton 9th November, 2021
}
