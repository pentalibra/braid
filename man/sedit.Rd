\name{sedit}
\alias{sedit}
\title{String editing function, adapted from Hmisc.}
\usage{
  sedit(text, from, to, test = NULL, wild.literal = FALSE)
}
\arguments{
  \item{text}{a vector of character strings for sedit,
  substring2, substring2<- or a single character string for
  substring.location, replace.substring.wild.}

  \item{from}{a vector of character strings to translate
  from, for sedit. A single asterisk wild card, meaning
  allow any sequence of characters (subject to the test
  function, if any) in place of the "*". An element of from
  may begin with "^" to force the match to begin at the
  beginning of text, and an element of from can end with
  "$" to force the match to end at the end of text.}

  \item{to}{a vector of character strings to translate to,
  for sedit. If a corresponding element in from had an "*",
  the element in to may also have an "*". Only single
  asterisks are allowed. If to is not the same length as
  from, the rep function is used to make it the same
  length.}

  \item{test}{a function of a vector of character strings
  returning a logical vector whose elements are TRUE or
  FALSE according to whether that string element qualifies
  as the wild card string for sedit,
  replace.substring.wild}

  \item{wild.literal}{set to TRUE to not treat asterisks as
  wild cards and to not look for "^" or "$" in old}
}
\description{
  String editing function, adapted from Hmisc.
}
\note{
  This code is copied from Hmisc
}
\keyword{Internal}

