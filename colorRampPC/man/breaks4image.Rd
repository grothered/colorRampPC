\name{breaks4image}
\alias{breaks4image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
breaks4image
}
\description{
Compute 'breaks' of a dataset suitable for use in the 'image' function, with various methods
to distribute colors (e.g. equal-area, linearly, ...)

This is designed to mimic some functionality of the QGIS 1-band raster plugin, which allows
users to distribute colors to equalise the image area, or linearly, or by various transformations.
}
\usage{
breaks4image(mycolRamp, inputdata, type = "equal-area", minval = NULL, maxval = NULL, power = 0.5, zeroval = 0, zerothresh = 1, ncell = 5e+05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mycolRamp}{
    A vector of colors (actually, anything with length=number_of_colors will be okay)
}
  \item{inputdata}{
    Data to distribute colors over. This can be anything with a 'quantile / min / max' method, or a RasterLayer
}
  \item{type}{
    How to compute the breaks. See Details
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
    #FIXME
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ colors }
