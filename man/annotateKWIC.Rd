% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate.R
\name{annotateKWIC}
\alias{annotateKWIC}
\title{Title}
\usage{
annotateKWIC(
  kwic = data.frame(IPId = paste("a", 1:10, sep = ""), a = 1:10, b = 10:1),
  corpus = data.frame(IPId = paste("a", 1:30, sep = ""), a = 1:30, b = 30:1),
  objectName = paste(deparse(substitute(kwic)), "_ann", sep = ""),
  pathPraat = "../praat"
)
}
\arguments{
\item{kwic}{concordanz}

\item{corpus}{corpus by ip}

\item{objectName}{name of object tp save to}

\item{pathPraat}{path where praat and sendpraat are saved does not end with /}
}
\description{
Title
}
