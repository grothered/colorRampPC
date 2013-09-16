\name{colorRampPC}
\alias{colorRampPC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Access various pre-canned colorRamps
}
\description{
    Access various pre-canned colorRamps FIXME
}
\usage{
    colorRampPC(ramp = "", n = NULL, alpha = 255, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ramp}{
    Name of color ramp. If the function is called with no arguments, all available ramps will be listed
}
  \item{n}{
    Number of colors to return. They will be evenly spaced over the ramp. If NULL, return a function which
    interpolates the colors
}
  \item{alpha}{
    Vector of transparancy values (in [0-255]). This can be of any length, and will be linearly interpolated
    along the color ramp.
}
  \item{\dots}{
    Any other argument to the colorRamp function
}
}
\details{
    FIXME
}
\value{
    A vector of colors (if n!=NULL), or a function which computes these from inputs in [0,1]
}
\references{
    NA
}
\author{
    Gareth Davies
}
\note{
    None
}

\seealso{
    colorRamp
}
\examples{
    # List all color ramps
    allRamps=colorRampPC()
    allRamps
    # Get a ramp
    myramp=allRamps[10]
    myramp
    mycols=colorRampPC(myramp,n=10)
    mycols
    plot_colorRamp(mycols)
    title('Basic color ramp')
    # Ramp with variable transparency
    mycols=colorRampPC(myramp,n=10, alpha=c(0,100, 255, 0))
    mycols
    plot_colorRamp(mycols)
    title('Basic color ramp with variable transparency')
    # Return a function instead, and still use transparency, and use other
    # arguments from colorRamp
    mycolsFun=colorRampPC(myramp,alpha=c(0,100,255,255,100,0), 
                          bias=0.5,space='Lab', interpolate='spline')
    mycolsFun(c(0,0.5,1.0))
    plot_colorRamp(mycolsFun(seq(0,1,len=100)))
    title('Basic color ramp with variable transparency,\n output as a function')

    ########### APPLICATION EXAMPLE #########    
 
    # Nice example
    data(volcano)

    # Set up plot
    mar_old=par('mar')
    par(mfrow=c(4,4))
    devAskNewPage(ask=TRUE)
    
    # Plot all color ramps
    par(mar=c(0,0,1,0))
    plot_colorRampPC()

    # Loop over all color schemes, and plot volcano data
    par(mar=c(0,0,0,0)+0.2)
    for(i in 1:length(colorRampPC())){
        colpal=colorRampPC()[i]
        # Equal area colors
        mycolpal=colorRampPC(colpal,n=100)
        mycolbreaks=breaks4image(mycolpal,volcano,type='equal-area')
        image(volcano, col=mycolpal,breaks=mycolbreaks,axes=FALSE)
        title(paste(colpal, '\n equal-area'),line=-2, col.main='white')    
        # Linear colors
        mycolbreaks=breaks4image(mycolpal,volcano,type='linear')
        image(volcano, col=mycolpal,breaks=mycolbreaks,axes=FALSE)
        title(paste(colpal, '\n linear'),line=-2, col.main='white')    

    }


    par(mar=mar_old)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
