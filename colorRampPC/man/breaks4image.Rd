\name{breaks4image}
\alias{breaks4image}
\alias{resample_colorVec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
breaks4image / ressample_colorVec
}
\description{
Various functions for 'spreading' colors over a domain

breaks4image can compute a vector of 'breaks' suitable for use in the 'image'
function, with various methods to distribute colors (e.g. equal-area, linearly,
...). 

This is designed to mimic some functionality of the QGIS 1-band raster plugin,
which allows users to distribute colors to equalise the image area, or
linearly, or by various transformations.

resample_colorVec is something like 'approx' for colors: It takes a color bar
defined by breaks,colVec, [with length(breaks)=length(colvec)+1] and evenly
interpolates along breaks these to return 'n' output colors, or alternatively,
to return a function which does the interpolation. In some cases, it is easier
to work with an evenly interpolated colorbar. See the example.

}
\usage{
breaks4image(mycolRamp, inputdata, type = "equal-area", 
             minval = NULL, maxval = NULL, power = 0.5, 
             zeroval = 0, zerothresh = 1, ncell = 5e+05)

resample_colorVec(colVec, breaks, n=NULL, 
                  return_function=(class(colVec)=='function' & is.null(n)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mycolRamp}{
    A (character) vector of colors (actually, anything with length=number_of_colors will work)
}
  \item{inputdata}{
    Data to distribute colors over. This can be anything with a 'quantile / min / max' method, or a RasterLayer
}
  \item{type}{
    How to compute the breaks in 'breaks4image'. See Details
}
  \item{minval}{
    Force the min of breaks to minval. Computed if NULL
}
  \item{maxval}{
    Force the max of breaks to maxval. Computed if NULL
}
  \item{power}{
    TODO: Relates to power transformation
}
  \item{zeroval}{
    TODO: Relates to log transformation
}
  \item{zerothresh}{
    TODO: Relates to log transformation
}
  \item{ncell}{
    If 'inputdata' is a RasterLayer, at most this many cells are used in 'quantile' computation
}
  \item{colVec}{
    A (character) vector of colors, or a function which takes values in [0-1] and returns vector of colors
}
  \item{breaks}{
    A numeric vector of breaks over which to spread the colors. If colVec is a vector, it must be of length(colVec)+1
}
  \item{n}{
    A number of colors to return
}

  \item{return_function}{
    Logical: If true, return a function taking numbers from the range of breaks, and returning a (character) color vector
}
}
\details{
    Supported values of 'type' are 'equal_area' (so colors are evenly spread over inputdata), 'linear' (so colours are linear between minval and maxval)
}
\value{
    A vector of 'breaks' with length = (length(mycolRamp)+1). This is suitable for use in "image" along with mycolRamp
}
\references{
}
\author{
    Gareth Davies
}
\note{
    
}


\seealso{
    cut, findInterval, colorRamp, colors, heat.colors
}
\examples{

    # Standard use of filled.contour 
    devAskNewPage(TRUE)
    mycolVec=terrain.colors(21)
    data(volcano)
    filled.contour(volcano,col=mycolVec, main='Standard approach')

    # Compute new breaks which spread the colors evenly over volcano
    newbreaks=breaks4image(mycolVec, volcano,type='equal-area')
    filled.contour(volcano, col=mycolVec,levels=newbreaks, main='Equal-area breaks')

    # How to compute colors to be linearly varying along the last colorbar?
    newcolVec=resample_colorVec(mycolVec, newbreaks,n=30)
    filled.contour(volcano,col=newcolVec, levels=seq(min(volcano), max(volcano), len=31), main='Resampled equal-area colors')
    devAskNewPage(FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ colors }
