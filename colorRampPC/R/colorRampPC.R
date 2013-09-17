### Code to import pre-canned colorRamps
#ramps_dir='../inst/extdata' 
ramps_dir=paste0(system.file(package='colorRampPC'), '/extdata')
all_ramps=dir(ramps_dir, pattern='.tbl')

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
    # OUTPUT: (if is.null(n)) Function interpolating colors from ramp, domain [0,1]. 
    #                         Colors are reported as characters
    #         If (!is.null(n)), then return n colors along the ramp instead
    #       
    #
    if(ramp==""){
        # Return a list of the color ramps
        return(all_ramps)
    }else if(ramp%in%all_ramps){
        # Read in ramp
        myramp=read.table(paste0(ramps_dir,'/',ramp),skip=1)

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

        outramp=colorRamp(mycols, ...)
       
        # Create function of x (vector in [0,1])
        outramp2<-function(x){
            myrgb=outramp(x)
            if(length(alpha)!=1){
                alpha3=approx(seq(0,1,len=length(alpha)), alpha,
                          xout=seq(0,1,len=length(myrgb[,1])))$y
            }else{
                alpha3=alpha
            }
            return(rgb(myrgb[,1],myrgb[,2],myrgb[,3],alpha3,maxColorValue=255))
        }

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

breaks4image<-function(mycolRamp, inputdata, type='equal-area',
                       minval=NULL,maxval=NULL,power=0.5,
                       zeroval=0,zerothresh=1, ncell=5e+05){
    # Make a 'breaks' argument for an image plot,
    # based on the input-data, and a choice of break types

    output= switch(type,
        'equal-area' = {
            if(class(inputdata)!='RasterLayer'){
               quantile(inputdata,prob=seq(0,1,len=length(mycolRamp)+1))
            }else{
               quantile(inputdata,prob=seq(0,1,len=length(mycolRamp)+1),ncells=ncell)
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
                if(is.null(minval)) minval=raster::minValue(inputdata,na.rm=T)
                if(is.null(maxval)) maxval=raster::maxValue(inputdata,na.rm=T) 
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
                if(is.null(minval)) minval=raster::minValue(inputdata,na.rm=T)
                if(is.null(maxval)) maxval=raster::maxValue(inputdata,na.rm=T) 
            }


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
        plot_colorRamp(mycol, mycolramp)
    }
}

#################################################################################
plot_colorRamp<-function(mycol, mycolramp=""){
    # Plot a vector of colors as a bar plot
    n=length(mycol)
    xleft=seq(0,1,len=n+1)[1:n]
    xright=seq(0,1,len=n+1)[2:(n+1)]
    ybottom=rep(0,length(xleft))
    ytop=rep(1,length(xleft))
    plot(c(0,1),c(0,1),col=0, axes=FALSE,ann=FALSE)
    rect(xleft,ybottom,xright,ytop,col=mycol,border=NA)
    title(mycolramp,line=0,col='white')
}
