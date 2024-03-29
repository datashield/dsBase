% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bp_standardsDS.R
\name{bp_standardsDS}
\alias{bp_standardsDS}
\title{Calculates Blood pressure z-scores}
\usage{
bp_standardsDS(
  sex = sex,
  age = age,
  height = height,
  bp = bp,
  systolic = systolic
)
}
\arguments{
\item{sex}{the name of the sex variable. The variable should be coded as 1 for males
and 2 for females. If it is coded differently (e.g. 0/1), then you can use the 
ds.recodeValues function to recode the categories to 1/2 before the use of 
ds.bp_standards}

\item{age}{the name of the age variable in years.}

\item{height}{the name of the height variable in cm}

\item{bp}{the name of the blood pressure variable.}

\item{systolic}{logical. If TRUE (default) the function assumes conversion of 
systolic blood pressure. If FALSE the function assumes conversion of diastolic 
blood pressure.}
}
\value{
assigns a new object on the server-side. The assigned object is a list 
with two elements: the 'Zbp' which is the zscores of the blood pressure and 'perc'
which is the percentiles of the BP zscores.
}
\description{
The function calculates blood pressure z-scores in two steps:
Step 1. Calculates z-score of height according to CDC growth chart (Not the 
WHO growth chart!). Step 2. Calculates z-score of BP according to the fourth
report on BP management, USA
}
\note{
The z-scores of height based on CDC growth charts are calculated 
by the sds function from the childsds R package.
}
\author{
Demetris Avraam for DataSHIELD Development Team
}
