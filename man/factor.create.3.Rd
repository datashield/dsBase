\name{factor.create.3}
\alias{factor.create.3}
\title{Generates a factor variable}
\usage{
  factor.create.3(x = base::character(), maxval = NA,
    minval = NA, Xval1 = NA, Xval2 = NA, Xval3 = NA,
    Xval4 = NA, Xval5 = NA, levels, labels = levels,
    exclude = NA, ordered = base::is.ordered(x))
}
\arguments{
  \item{x}{a categorical variable (numerical or character)}

  \item{maxval}{highest factor value}

  \item{minval}{lowest factor value}

  \item{Xval1}{optional value to specify}

  \item{Xval2}{optional value to specify}

  \item{Xval3}{optional value to specify}

  \item{Xval4}{optional value to specify}

  \item{Xval5}{optional value to specify}

  \item{levels}{optional vector of values that 'x' might
  have taken}

  \item{labels}{labels for the different levels}

  \item{exclude}{values to be excluded}

  \item{ordered}{tells if the variable 'x' is ordered}
}
\description{
  Generates a factor variable
}

