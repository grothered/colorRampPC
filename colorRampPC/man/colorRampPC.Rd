\name{colorRampPC}
\alias{colorRampPC}
\alias{name_all_colorRamps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Access various pre-canned colorRamps
}
\description{
    Access various pre-canned colorRamps as either a character vector of
    colors, or a function from [0-1] to the colors

    The names of the ramps can be found by calling colorRampPC without arguments, 
    or with the alias name_all_colorRamps()

    The ramps were sourced from (the GPL parts of) cpt-city, and from the QGIS 1-band Raster Colors plugin (developed by bccConsulting).
}
\usage{
    colorRampPC(ramp = "", n = NULL, alpha = 255, 
                 reverse=FALSE, invert=FALSE,...)

    name_all_colorRamps()
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
  \item{reverse}{
    Logical: Reverse color order
}
  \item{invert}{
    Logical: Invert colors (rgb --> 255 - rgb)
}
  \item{\dots}{
    Any other argument to the colorRamp function
}
}
\value{
    A vector of colors (if n!=NULL), or a function which computes these from inputs in [0,1]
    
    name_all_colorRamps() and colorRampPC() return a character vector with the names of all available ramps. 
}
\author{
    Gareth Davies
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
    plot_colorVec(mycols)
    title('Basic color ramp')
    # Ramp with variable transparency
    mycols=colorRampPC(myramp,n=10, alpha=c(0,100, 255, 0))
    mycols
    plot_colorVec(mycols)
    title('Basic color ramp with variable transparency')
    # Return a function instead, and still use transparency, and use other
    # arguments from colorRamp
    mycolsFun=colorRampPC(myramp,alpha=c(0,100,255,255,100,0), 
                          bias=0.5,space='Lab', interpolate='spline')
    mycolsFun(c(0,0.5,1.0))
    plot_colorVec(mycolsFun(seq(0,1,len=100)))
    title('Basic color ramp with variable transparency,\n output as a function')

    ########### APPLICATION EXAMPLE #########    
 
    # Nice example
    data(volcano)

    v=round(seq(1,length(allRamps),len=9))

    # Set up plot
    mar_old=par('mar')
    par(mfrow=c(3,3))
    devAskNewPage(ask=TRUE)
    
    # Plot all color ramps
    par(oma=c(0,0,2,0))
    par(mar=c(1,1,2,1))
    plot_colorRampPC(allRamps[v])
    title('Some Color Ramps', outer=TRUE)

    # Plot the reversed color ramps
    #par(mar=c(0,0,1,0))
    par(oma=c(0,0,2,0))
    par(mar=c(1,1,2,1))
    plot_colorRampPC(allRamps[v],reverse=TRUE)
    title('Reversed Ramps', outer=TRUE)
   
    # Plot inverted color ramps 
    #par(mar=c(0,0,1,0))
    par(oma=c(0,0,2,0))
    par(mar=c(1,1,2,1))
    plot_colorRampPC(allRamps[v],invert=TRUE)
    title('Inverted Ramps', outer=TRUE)

    # Loop over all color schemes, and plot volcano data
    #par(mar=c(0,0,0,0)+0.2)
    for(i in 1:length(v[1:3])){
        colpal=allRamps[v][i] #colorRampPC()[i]
        # Equal area colors
        mycolpal=colorRampPC(colpal,n=100)
        mycolbreaks=breaks4image(mycolpal,volcano,type='equal-area')
        image(volcano, col=mycolpal,breaks=mycolbreaks,axes=FALSE)
        #title(paste(colpal, '\n equal-area'),line=-2, col.main='white')    
        title(paste(colpal, '\n equal-area'))    

        # Linear colors
        mycolbreaks=breaks4image(mycolpal,volcano,type='linear')
        image(volcano, col=mycolpal,breaks=mycolbreaks,axes=FALSE)
        #title(paste(colpal, '\n linear'),line=-2, col.main='white')    
        title(paste(colpal, '\n linear'))    
        
        mycolbreaks=breaks4image(mycolpal,volcano,type='log',log_stretch_factor=1.)
        image(volcano, col=mycolpal,breaks=mycolbreaks,axes=FALSE)
        #title(paste(colpal, '\n log'),line=-2, col.main='white')    
        title(paste(colpal, '\n log'))    

    }

    # plots of them all
    par(mfrow=c(3,3))
    plot_colorRampPC()

    par(mar=mar_old)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
