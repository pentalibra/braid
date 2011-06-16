\name{as.braid}
\alias{as.braid}
\title{Creates object of class braid.}
\usage{as.braid(path_latex=Sys.getenv()["TEMP"], path_graphics=path_latex,
    output_filename=file.path(path_latex, "braid.tex"),
    counter_start=1, default_plot_size=c(5, 3), dpi=600)}
\description{Creates object of class braid.}
\details{A braid object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the braid package.}
\value{A list object of class braid}
\arguments{\item{path_latex}{Path where latex will be saved}
\item{path_graphics}{Path where graphics files will be saved}
\item{output_filename}{Filename where latex output will be saved}
\item{counter_start}{The starting number for a counter used to store graphs,}
\item{default_plot_size}{Plot size in inches, e.g. c(4, 3)}
\item{dpi}{Dots per inch, passed to ggsave()}}
