\name{plot_colorRampPC}
\alias{plot_colorRampPC}
\alias{plot_colorVec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Plot a color ramp. 
}
\description{
    Plot a color ramp. Mainly used to inspect/choose ramps for use elsewhere
}
\usage{
    plot_colorRampPC(colramps = "", n=300, ...)

plot_colorVec(colvec,
              xleft=0,xright=1,ybottom=0,ytop=1,
              breaks=NULL,
              vertical=FALSE, add=FALSE, 
              add_axis=!add,
              plotWidthScale=0.5,
              plotHeightScale=1.0,
              las=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colramps}{
    Name of color ramp from the colorRampPC package. Use colorRampPC() to see all available ramps
}
  \item{colvec}{
    Vector of named colors
}
  \item{n}{
    Number of colors to use
}
  \item{xleft, xright, ybottom, ytop}{
    Coordinates of box
}
  \item{breaks}{
    Numeric vector of breaks defining the colorbar scale.
    This will be interpolated from to define axis labels if add_axis=TRUE
}
  \item{vertical}{
    Logical: Draw bars vertically?
}
  \item{add}{
    Logical: Add to existing plot?
}
  \item{add_axis}{
    Add a scale to the colorbar. Values will be interpolated from 'breaks' if
    that is provided, otherwise they will be interpolated from ybottom-ytop if
    vertical=TRUE, or from xleft-xright if vertical=FALSE 
}
  \item{plotWidthScale, plotHeightScale}{
    If add=FALSE, these can be used to adjust the relative scale of the colorbar
    compared with the plot axes
}
\item{las}{
    las parameter controlling colorRamp label orientation. See ?par for details on las
}
  \item{...}{
    Further arguments to colorRampPC
}
}
\details{
    NA
}
\value{
    None, but plots the colorRamp as a side effect
}
\references{
    NA
}
\author{
    Gareth Davies
}
\note{
    NA
}


\seealso{
    colorRamp
}
\examples{
    # Plot all colorramps
    devAskNewPage(TRUE)
    par(mfrow=c(15,9))
    par(mar=c(0,0,1,0))
    plot_colorRampPC()
    devAskNewPage(FALSE)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
