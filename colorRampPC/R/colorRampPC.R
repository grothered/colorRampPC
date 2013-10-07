### Code to import pre-canned colorRamps
#ramps_dir='../inst/extdata' 
ramps_dir=paste0(system.file(package='colorRampPC'), '/extdata')
all_ramps=c(dir(ramps_dir, pattern='\\.tbl',recursive=T), 
            dir(ramps_dir,pattern='\\.cpt',recursive=T))

## This function was copied from the 'raster' package
## by Robert J Hijmans.
## (package version raster_2.1-49)
extension<-function (filename, value = NULL, maxchar = 10) 
{
    # Find the file extension
    if (!is.null(value)) {
        extension(filename) <- value
        return(filename)
    }
    lfn <- nchar(filename)
    ext <- list()
    for (f in 1:length(filename)) {
        extstart <- -1
        for (i in lfn[f]:2) {
            if (substr(filename[f], i, i) == ".") {
                extstart <- i
                break
            }
        }
        if (extstart > 0) {
            ext[f] <- substr(filename[f], extstart, lfn[f])
        }
        else {
            ext[f] <- ""
        }
    }
    ext <- unlist(ext)
    ext[nchar(ext) > maxchar] <- ""
    return(ext)
}

###

cpt2rgb<-function(cptTxt){
    # Convert cpt text to rgb

    # Tread rare file formats
    cptTxt=gsub('\t', ' ', cptTxt)

    
    start_col=grep('COLOR_MODEL', cptTxt)+1
    # Treat rare color model cases
    if(length(start_col)==0) start_col=grep('COLOR MODEL', cptTxt)+1
    if(length(start_col)==0) start_col=max(grep('#',cptTxt))+1

    # 
    tmp=grep('^[A-Z]', cptTxt)
    if(length(tmp)>0){
        end_col=min(tmp)-1
    }else{
        end_col=length(cptTxt)
    }
   
    # Remove occasional characters in color specification 
    cptTxt[start_col:end_col]=gsub('[A-Z]', '', cptTxt[start_col:end_col])
    cptlines=read.table(text=cptTxt[start_col:end_col]) 
    
    # Remove the 'elevation' lines
    cptlines=matrix( c(t(as.matrix(cptlines[,c(2,3,4,6,7,8)]))), ncol=3,byrow=T)

    # If the colors are HSV, convert to rgb before output
    if(length(grep('HSV', cptTxt[start_col-1]))!=0){
        # Sometimes the scale is [0-1], othertimes [0-360]. Normalise
        if(max(cptlines[,1])>1) cptlines[,1]=cptlines[,1]/360
        if(max(cptlines[,2])>1) cptlines[,2]=cptlines[,2]/360
        if(max(cptlines[,3])>1) cptlines[,3]=cptlines[,3]/360
        tmpcol=hsv(cptlines[,1], cptlines[,2], cptlines[,3])
        cptlines=t(col2rgb(tmpcol))    
    }

    return(cptlines)
}

#####################################################

get_ramp<-function(ramp){
    # Function to read a color ramp file.
    # Allows us to treat different formats

    switch(extension(ramp),
           '.tbl'={
                read.table(paste0(ramps_dir,'/',ramp),skip=1)
            },
           '.cpt'={
                mycpt=readLines(paste0(ramps_dir,'/',ramp))
                output=try(cpt2rgb(mycpt))
                # Catch various error cases
                if(class(output)=='try-error') output=matrix(c(0,0,0),ncol=3,nrow=2)
                if(ncol(output)!=3) output=matrix(c(0,0,0),ncol=3,nrow=2)
                output
            },
            stop('ramp type not supported')
          )

}


##########################################################################################
colorRampPC<-function(ramp="", n=NULL, alpha=255,reverse=FALSE,invert=FALSE, ... ){
    # Read a "Pre-Canned" color ramp (name='ramp'), 
    # and return a function (output of 'colorRamp')
    # which interpolates among these colors, or n colors
    #
    # INPUT: ramp = name of 'pre-canned' color ramp
    #               If ramp="", then print a vector of available color ramps
    #        alpha = vector of transparency values,which will be interpolated along the color ramp
    #        n = number of colours. If n=NULL, return 'colorRamp' function       
    #        reverse = switch order of colors
    #        invert = invert colors (255 - col)
    #        ... = further arguments to colorRamp
    #
    # OUTPUT: (if is.null(n)) Return a function interpolating colors from ramp, domain [0,1]. 
    #                         Colors are reported as characters
    #         If (!is.null(n)), then Return n colors along the ramp instead (as character vectors)
    #       
    #
    if(ramp==""){
        # Return a list of the color ramps
        return(all_ramps)
    }else if(ramp%in%all_ramps){
        # Read in ramp
        myramp=get_ramp(ramp)

        if(reverse){
            l = dim(myramp)[1]
            myramp=myramp[l:1,]
        }

        if(invert){
            myramp = 255 - myramp
        }

        # Interpolate 'alpha' transparency values if needed
        #if(length(alpha)!=1){
        #    alpha2=approx(seq(0,1,len=length(alpha)), alpha,
        #                  xout=seq(0,1,len=length(myramp[,1])))$y
        #}else{
        #    alpha2=alpha
        #}
    
        #mycols=rgb(myramp[,1], myramp[,2], myramp[,3],alpha2, maxColorValue=255)
        mycols=rgb(myramp[,1], myramp[,2], myramp[,3], maxColorValue=255)

        # colorRamp will not treat 'alpha' values
        outramp=colorRamp(mycols, ...)
     
        # Make a function for interpolating colors which treats alpha values 
        outramp2=make_color_char_function(outramp, alpha)
 
        # If n!=NULL, return vector of 'n' colors
        if(!is.null(n)){
            if(n>0 & round(n)==n){
                #outramp2=rgb(outramp(seq(0,1,len=n)),maxColorValue=255)
                outramp2=outramp2(seq(0,1,len=n))
            }else{
                stop(paste('Invalid "n" value (must be positive integer or NULL):', n))
            }
        }

        #
        return(outramp2)

    }else{
        # Report error
        stop(paste('Unknown ramp', ramp))
    }     
}

########################################################################################

make_color_char_function<-function(outramp,alpha){
    # outramp is a function created by 'grDevices::colorRamp'
    # alpha is a vector or scalar of transparency values in [0,255]
    # Return a function like 'outramp' which accounts for transparency,
    # and which returns character values rather than rgb values

    # Create function of x (vector in [0,1])
    outramp2<-function(x){
        myrgb=outramp(x)
        if(length(alpha)!=1){
            # Approximate 'length(x)' points along alpha
            alpha3=approx(seq(0,1,len=length(alpha)), alpha,
                      xout=seq(0,1,len=length(x)))$y
        }else{
            alpha3=alpha
        }
        return(rgb(myrgb[,1],myrgb[,2],myrgb[,3],alpha3,maxColorValue=255))
    }
    # Return the function
    return(outramp2)
}

####################################################################################

breaks4image<-function(mycolRamp, inputdata, type='equal-area',
                       minval=NULL,maxval=NULL,power=0.5,
                       log_one_val=minval, log_stretch_factor=1, 
                       zerothresh=1, ncell=1e+04){
    # Make a 'breaks' argument for an image plot,
    # based on the input-data, and a choice of break types

    output= switch(type,
        'equal-area' = {
            if(class(inputdata)!='RasterLayer'){
                quantile(inputdata,prob=seq(0,1,len=length(mycolRamp)+1))
            }else{
                require(raster)
                output = quantile(inputdata,prob=seq(0,1,len=length(mycolRamp)+1),ncells=ncell)

                # Make sure min/max coincide with input values or raster values
                if(!is.null(minval)){
                    output[1] = minval
                }else if(is.finite(inputdata@data@min)){
                    output[1]=inputdata@data@min
                }
                if(!is.null(maxval)){
                    output[length(output)] = maxval
                }else if(is.finite(inputdata@data@max)){
                    output[length(output)] = inputdata@data@max
                }
                output
            }
        },
        'linear'={
            # Compute min/max if not already provided
            if(class(inputdata)!='RasterLayer'){
                if(is.null(minval)) minval=min(inputdata,na.rm=T)
                if(is.null(maxval)) maxval=max(inputdata,na.rm=T) 
            }else{
                # Rasters have a different min/max function
                require(raster)
                if(is.null(minval)) minval=raster::minValue(inputdata)
                if(is.null(maxval)) maxval=raster::maxValue(inputdata) 
            }
            seq(minval,maxval,len=length(mycolRamp)+1)
        },
        'log'={
            # Compute min/max if not already provided
            if(class(inputdata)!='RasterLayer'){
                if(is.null(minval)) minval=min(inputdata,na.rm=T)
                if(is.null(maxval)) maxval=max(inputdata,na.rm=T) 
            }else{
                require(raster)
                # Rasters have a different min/max function
                if(is.null(minval)) minval=raster::minValue(inputdata)
                if(is.null(maxval)) maxval=raster::maxValue(inputdata) 
            }

            if(is.null(log_one_val)) log_one_val=minval

            if(!(log_one_val<maxval)){
                stop('ERROR: For log-type breaks, log_one_val must be set as less than maxval')
            }

            log_seq=(10**seq(log10(1), log10(1+log_stretch_factor), len=length(mycolRamp)+1)-1)/(log_stretch_factor) 
            breaks=log_one_val + (maxval-log_one_val)*log_seq

            breaks

        },
        'power'={


        },
        { stop('type not found in stretch_colorRamp')}
    )

    return(output)
}

##################################################################################

plot_colorRampPC<-function(colramps="", n=300, ...){
    # Make a barplot of the colorRamp
    if(is.na(colramps)) stop('colramps is NA')

    if(colramps=="") colramps=colorRampPC()

    for(mycolramp in colramps){
        mycol=colorRampPC(mycolramp,n, ...)
        plot_colorVec(mycol)
        title(mycolramp,line=0,col='white')
    }
}

#################################################################################
plot_colorVec<-function(colvec,
                        xleft=0,xright=1,ybottom=0,ytop=1,
                        breaks=NULL,
                        vertical=FALSE, add=FALSE, 
                        add_axis=!add,
                        plotWidthScale=0.5,
                        plotHeightScale=1.0){
    # Plot a vector of colors as a bar plot
    # Optionall add axes, with labels based on 'breaks', and adjust the scale

    n=length(colvec)
    if(!vertical){
        xl=seq(xleft,xright,len=n+1)[1:n]
        xr=seq(xleft,xright,len=n+1)[2:(n+1)]
        ybt=rep(ybottom,length(xl))
        ytp=rep(ytop,length(xl))
    }else{
        xl=rep(xleft,len=n)
        xr=rep(xright,len=n)
        ybt=seq(ybottom, ytop,len=n+1)[1:n]
        ytp=seq(ybottom, ytop,len=n+1)[2:(n+1)]
    }
    if(add==FALSE) plot(xleft+c(0,xright-xleft)/plotWidthScale,ybottom+c(0,ytop-ybottom)/plotHeightScale,col=0, axes=FALSE,ann=FALSE)
    rect(xl,ybt,xr,ytp,col=colvec,border=NA)

    if(add_axis){
        # Add an axis for the colorbar
        if(is.null(breaks)){
            if(vertical){
                breaks=seq(ybottom,ytop,len=length(colvec)+1)
            }else{
                breaks=seq(ybottom,ytop,len=length(colvec)+1)
            }
        }
        if(vertical){
            axis(side=4,pos=xright,las=2,at=seq(ybottom,ytop,len=5),
                 labels=signif(approx(seq(0,1,len=length(breaks)),
                 breaks, xout=seq(0,1,len=5))$y,3),las=2)
        }else{
            axis(side=1,pos=ybottom,las=2,at=seq(xleft,xright,len=5),
                 labels=signif(approx(seq(0,1,len=length(breaks)),
                 breaks, xout=seq(0,1,len=5))$y,3))
        }
    }
}

#################################################################################
resample_colorVec<-function(colVec, breaks, n=NULL,
                               return_function=(class(colVec)=='function' & is.null(n))){
    # Spread 'colVec' over 'breaks'
    # If n is an integer, return that many colors
    # If n=NULL, and colVec is a vector, then return a vector of length colVec
    # If colVec is a function from [0-1] returning a character vector of
    # colors, and return_function=TRUE, then return a function from the range of breaks into the colorspace
    #
    if(class(colVec)!='function'){
        if((length(colVec)+1)!=length(breaks)){
            print('In resample_colorVec, if colVec is a vector of colors with length "q", then breaks must be of length "q+1"')
            print(paste0('The given lengths are instead', length(colVec), length(breaks)))
            stop('Incorrect inputs')
        }
        #browser()
        # Create a function which interpolates over colVec, and returns a character vector
        colVec2=col2rgb(colVec,alpha=T) # Extract r,g,b,alpha values from colors
        colVec2p5=rgb(colVec2[1,], colVec2[2,],colVec2[3,],maxColorValue=255)
        colVec3=colorRamp(colVec2p5) # Make a color ramp function (but 'colorRamp' cannot treat alpha values, and returns rgb)
        colVecfun=make_color_char_function(colVec3, colVec2[4,]) # colVec2[4,] = transparency values
    }else{
        colVecfun=colVec
    }

    # Make a function to go from breaks to 0-1

    breaks_to_unit_interval=approxfun(breaks,seq(0,1,len=length(breaks)))
   
    outputfun<-function(x){
        # Make function which returns colors given inputs along 'breaks'
        x_on_unit_interval=breaks_to_unit_interval(x)
        output_cols=colVecfun(x_on_unit_interval)
        return(output_cols)    
    }

    if(return_function){
        return(outputfun) 
    }else{
        if(is.null(n)){
            if(class(colVec=='function')){
                 stop('Must supply n (number of colors) for resample_colorRampPC when colVec is a function and return_function==FALSE')
            }
            # Assume we want 'length(colVec)' colors
            n = length(colVec)
        }
        outputcols=outputfun(seq(min(breaks),max(breaks),len=n))
        return(outputcols)
    }
}


pdf_ramps<-function(pdfname='all_color_ramps.pdf'){
    pdf(pdfname,width=7,height=5)
    par(mfrow=c(24,6))
    par(mar=c(0.0,0,1,0.0))
    all_colorRamps=colorRampPC()
    for(i in 1:length(all_colorRamps)){
        #mycpt=readLines(distrib_cpts[i])
        #rgbtmp=try(cpt2rgb(mycpt))
        #if(class(rgbtmp)=='try-error') next
        #mycols=rgb(rgbtmp[,1], rgbtmp[,2], rgbtmp[,3],maxColorValue=255)
        mycols=colorRampPC(all_colorRamps[i],n=300)
        plot_colorVec(mycols,plotWidthScale=0.9,add_axis=FALSE)
        title(all_colorRamps[i],cex.main=0.5)
    }
    dev.off()
}
