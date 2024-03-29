% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blackBoxRanksDS.R
\name{blackBoxRanksDS}
\alias{blackBoxRanksDS}
\title{Secure ranking of "V2BR" (vector to be ranked) across all sources}
\usage{
blackBoxRanksDS(input.var.name = NULL, shared.seedval)
}
\arguments{
\item{input.var.name}{a character string specifying the name of the
vector holding the global ranks. This argument is set automatically by
the clientside function ds.ranksSecure}

\item{shared.seedval}{a pseudorandom number seed that ensures that the
processes generating the order and parameterisation of the encryption
algorithms are the same in each study. This argument is set by the argument
<shared.seed.value> in the clientside function ds.ranksSecure. The
seed value shared by all studies in setting up the encryption procedures
in blackBoxRanksDS is arbitrarily changed from that used to set up the
encryption procedures in blackBoxDS, so the the set of 7 encryption
algorithms is deliberately different. For more
details, including future plans to share this starting seed in a more secure
way, please see the associated document entitled "secure.global.ranking.docx"
and the header file for ds.ranksSecure.}
}
\value{
writes a data frame object entitled blackbox.ranks.df to the
serverside. In each study this contains the encrypted global ranks
and a range of other key components from the second stage (ranking of global
ranks for real observations only) of the ranking procedure. For more details
see the associated document entitled "secure.global.ranking.docx"
}
\description{
The second key serverside function that prepares the global
ranks of the the real data only generated in the first stage of the
ranking procedure and encrypts them in preparation for generating global
ranks that correspond 1 to 1 with only the real data in V2BR.
}
\details{
Severside assign function called by ds.ranksSecure. It takes the
global ranks currently held in sR5.df which reflect the global ranks based on
the "combined real+pseudo data vector" as encrypted by blackBoxDS 
but with all pseudo-data stripped out. It then uses these global ranks (of
the real data) as if they were a new variable to be ranked. This is then
equivalent to blackBoxDS with the primary difference that no
pseudo-data are needed. This is because the global ranks are fundamentally
non-disclosive and so can be transferred to the clientside with no risk of
disclosure. However, in order to ensure that the client cannot compare
the list of global.ranks in sR4.df (after initial global ranking based on
ranking of real and pseudo-data combined) with the global.ranks to be
generated by blackBoxRanksDS (based solely on the real data they are 
processed through seven more rounds of encryption as before in blackBoxDS.
In consequence the client remains unable to determine which of the original
global ranks corresponded to real data and which to pseudo-data. In
addition, blackBoxRanksDS does not need to determine the number of decimal
places in the data because it is only applied to ranks which are assumed to
be integers. For more details about the cluster of functions that
collectively enable secure global ranking and estimation of global quantiles
see the associated document entitled "secure.global.ranking.docx". Also
see the header file for ds.ranksSecure and the header file for blackBoxDS
}
\author{
Paul Burton 9th November, 2021
}
