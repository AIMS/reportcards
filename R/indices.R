##' @title Index formulations
##' @description Index formulations including 'Binary'.
##' @param x a numeric vector of observed values
##' @param GL a numeric vector the same length as \code{x}
##'     representing the Guideline/reference/threshold value
##' @param Lower a numeric vector the same length as \code{x}
##'     representing the lower range for items that have an ideal
##'     optimum.  Only applied when DOF='B'.
##' @param Upper a numeric vector the same length as \code{x}
##'     representing the upper range for items that have an ideal
##'     optimum.  Only applied when DOF='B'.
##' @param lBound a numeric vector the same length as \code{x}
##'     representing the lower bound of the index scaling and capping
##'     region.  If non NA, \code{fold} will be ignored.
##' @param uBound a numeric vector the same length as \code{x}
##'     representing the upper bound of the index scaling and capping
##'     region. If non NA, \code{fold} will be ignored.
##' @param Cuts a character vector indicating, the conversion chart
##'     breaks and corresponding scores (via key=value pairs
####'     separated by ':')
##' @param fold a numeric vector the same length as \code{x}.  Applied
##'     when \code{type='MAMP'}, indicating the multiplier and
##'     divisory applied to the \code{GL} (on a log_2 scale) for
##'     determining the lower and upper bounds of the index scaling
##'     and capping region.
##' @param DOF a character vector the same length as \code{x}
##'     representing an indication of the Direction Of Failure.  Must
##'     be one of 'H' (higher), 'L' (lower) or 'B' (both).
##' @param type an abbreviation of the index formulation to use.  Must
##'     be one of 'Binary' (Binary), 'MAMP' (Modified Amplitude).
##' @param capped a logical indicating whether the indices should be
##'     capped to the range [\code{lBound}, \code{uBound}].
##' @param scaled a logical indicating whether the indices should be
##'     scaled to the range [0,1].  This cannot be applied to HSAMP or
##'     LAMP, the former of which is already scaled.
##' @details
##'
##' Binary formulation:
##' \deqn{Score_i = \left\{\begin{array}{l l}1 & \text{if}~x_i \le
##'     G_i\\0 & \text{if}~x_i~\text{else}\\\end{array}}{
##' Score_i = [if x_i <= GL_i] 1
##'           [else:] 0
##' }
##' Benchmark and Worst Case Scenario formulation:
##' Should not be applied to items with an optimum.
##' Modified Amplitude formulation:
##' \deqn{}{
##' Score_i = [if x_i > GL_i = fail:]
##'                  log_2(x_i/G_i)^-1\cr
##'           [if x_i < GL = fail:]
##'                  log_2(x_i/G_i)^1
##' }
##' @return a numeric vector of index values
##' @author Murray Logan
##' @seealso \code{\link{RC_visualize_indices}}
##' @export
##' @examples
##' library(tidyverse)
##' ## A very simple example
##' RC_index(x=c(30,40,60,70), GL=50, DOF='H', type='Binary')
##'
##' ## Using built-in fabricated ecological monitoring and guidelines
##' ##    data
##' library(tidyverse)
##' data(reportCard)
##'reportCard.idx = reportCard %>%
##' mutate(Score = case_when(
##'               Measure %in% c('Chla') ~ RC_index(x = Value, GL = GL, fold = 2, DOF = DOF, type = 'MAMP', capped = TRUE, scaled = TRUE),
##'               Measure %in% c('TN', 'TP') ~ RC_index(x = Value, GL = GL, lBound = lBound, uBound = uBound, T = 2, DOF = DOF, type = 'HSAMP'),
##'               Measure %in% c('Biomass', 'Secchi') ~ RC_index(x = Value, GL = GL, lBound = lBound, uBound = uBound, T = 3, DOF = DOF, type = 'HSAMP'),
##'               Measure %in% c('pH') ~ RC_index(x = Value, Lower = Lower, Upper = Upper, lBound = lBound, uBound = uBound, T = 3, DOF = DOF, type = 'HSAMP'),
##'               Measure %in% c('Cover') ~ RC_index(x = Value, Cuts = Cuts, type = 'Chart')
##'       )
##'    )
##' reportCard.idx %>% as.data.frame %>% head
RC_index <- function(x=NA, GL=NA, Lower=NA, Upper=NA, lBound=NA, uBound=NA, Cuts=NA, fold=2, DOF='H', T=NA, type='Binary', capped=FALSE, scaled=FALSE) {
    if (all(is.na(x))) stop('You must supply a vector of values with at least some non NA values.')
    if (!is.numeric(x)) stop('You must supply a vector of numeric values.')
    if (!type %in% c('Binary','WCS','MAMP','HSAMP','LAMP','Chart')) stop("type must be one of 'Binary','WCS', 'MAMP','HSAMP','LAMP','Chart'")
    switch(type,
           'Binary'=RC_index.binary(x=x, GL=GL, Lower=Lower, Upper=Upper, DOF=DOF),
           'WCS'=RC_index.wcs(x=x, GL=GL, Lower=Lower, Upper=Upper, lBound=lBound, uBound=uBound, DOF=DOF, scaled=scaled),
           'MAMP'=RC_index.mamp(x=x, GL=GL, Lower=Lower, Upper=Upper, fold=fold, DOF=DOF, capped=capped, scaled=scaled),
           'HSAMP'=RC_index.hsamp(x=x, GL=GL, Lower=Lower, Upper=Upper, lBound, uBound, T=T, DOF=DOF),
           'LAMP'=RC_index.lamp(x=x, GL=GL, Lower=Lower, Upper=Upper, T=T, DOF=DOF),
           'Chart'=RC_index.chart(x=x, Cuts=Cuts)
           )
}

RC_index.chart <- function(x=NA, Cuts=NA) {
    if (all(is.na(x))) stop('At least some non-NA values must be supplied')
    if (all(is.na(Cuts))) stop('At least some Cuts must be supplied')
    FUN = t(sapply(strsplit(unlist(strsplit(Cuts, ':')), '='), 'as.numeric'))
    approxfun(FUN)(x)                
}

## The following function is a hidden function that performs the Binary index formulation
RC_index.binary <- function(x=NA, GL=NA, Lower=NA, Upper=NA, DOF=NA) {
    s <- ifelse(is.na(x) | (is.na(GL) & is.na(Lower)) | is.na(DOF), NA,
         ifelse((DOF=='H' & !x > GL) | (DOF=='L' & !x < GL), 1,
         ifelse((DOF=='H' & x > GL) | (DOF=='L' & x > 0), 0,
         ifelse(DOF=='B' & (x>=Lower & x<=Upper), 1, 0))))
    return(s)
}



RC_index.wcs <- function(x=NA, GL=NA, Lower=NA, Upper=NA, lBound=NA, uBound=NA, DOF=NA, scaled=FALSE) {
    s <- ifelse(is.na(x) | (is.na(GL) & (is.na(lBound) | is.na(uBound))) | is.na(DOF), NA,
         ifelse((DOF=='H' & x <= GL) | (DOF=='L' & x>=GL), 100,
         ifelse((DOF=='H' & x >= uBound ) | (DOF=='L' & x<=lBound),0,
         ifelse(DOF=='H' & x>GL & x<uBound, (1-abs((x-GL)/(uBound-GL)))*100,
         ifelse(DOF=='L' & x<GL & x>lBound, (1-abs((x-GL)/(lBound-GL)))*100,
         ifelse(DOF=='B' & x>=Lower & x<=Upper, 100,
         ifelse(DOF=='B' & (x > uBound | x < lBound),0,
         ifelse(DOF=='B' & x>Upper, (1-abs((x-Upper)/(uBound-Upper)))*100,
         ifelse(DOF=='B' & x<Lower, (1-abs((x-Lower)/(lBound-Lower)))*100, NA
                )))))))))
    if (scaled==TRUE)     {
        r = function(x, l, u, a, b) (b-a)*((x-l)/(u-l))+a
        s = r(s,0,100,0,1)
    }
    return(s)
}


RC_index.mamp <- function(x=NA, GL=NA, Lower=NA, Upper=NA, fold=NA, DOF=NA, capped=FALSE, scaled=FALSE) {
    s <- ifelse(is.na(x) | (is.na(GL) & is.na(Lower)) | is.na(DOF), NA,
         ifelse(DOF=='H' & !is.na(x), log((x/GL)^-1,2),
         ifelse(DOF=='L' & !is.na(x), log((x/GL)^1,2),
         ifelse(DOF=='B' & !is.na(Lower) & x < Lower, 1+log((x/Lower)^1,2),
         ifelse(DOF=='B' & !is.na(Upper) & x > Upper, 1+log((x/Upper)^-1,2),
                1)))))
    if (capped==TRUE) {
        s = ifelse(DOF=='H' & x<GL/fold, log((fold)^1,2),
            ifelse(DOF=='H' & x>GL*fold, log((fold)^-1,2),
            ifelse(DOF=='L' & x<GL/fold, log((fold)^-1,2),
            ifelse(DOF=='L' & x>GL*fold, log((fold)^1,2),
            ifelse(DOF=='B' & x<Lower/fold, 1+log((fold)^-1,2),
            ifelse(DOF=='B' & x>Upper*fold, 1+log((fold)^-1,2),s))))))
    }
    if (scaled==TRUE)     {
        r = function(x, l, u, a, b) (b-a)*((x-l)/(u-l))+a
        if (capped==TRUE) {
            s = ifelse(DOF!='B' & !is.na(s) & !is.na(fold), r(s,-log(fold,2),log(fold,2),0,1), r(s,1-log(fold,2),1,0,1))
        } else if (capped==FALSE & length(s) > 1 ) {
            s = r(s,min(s),max(s),0,1)
        } else {
            s
        }
    }
    return(s)
}


RC_index.hsamp <- function(x=NA, GL=NA, Lower=NA, Upper=NA, lBound=NA, uBound=NA, T=NA, DOF=NA) {
    g = function(x) GL-x
    g1 = function(x) Lower-x
    g2 = function(x) x-Upper
    r = function(x, l, u, a, b) (b-a)*((x-l)/(u-l))+a
    h = function(x) (exp(x) - exp(-x))/2
    s=ifelse(DOF=='H' & g(x)>=0,
             r(g(x), 0, g(lBound), a=0, b=T),
      ifelse(DOF=='H' & g(x)<0,
             r(g(x), g(uBound), 0, a=-T, b=0),
      ifelse(DOF=='L' & g(x)>=0,
             r(-g(x), 0, g(lBound), a=0, b=T),
      ifelse(DOF=='L' & g(x)<0,
             r(-g(x), g(uBound), 0, a=-T, b=0),
      ifelse(DOF=='B' & x<Lower,
             r(g1(x), g1(lBound), 0, a=-T, b=0),
      ifelse(DOF=='B' & x>Upper,
             r(g2(x), g2(uBound), 0, a=-T, b=0),
      ifelse(DOF=='B' & x>= Lower & x<=Upper, 0.0,NA)
      ))))))
    s=ifelse(s< -T, -T, ifelse(s>T,T, s))
    return(ifelse(s>=0 & DOF!='B', r(h(s),l=0, u=h(T), a=0.5, b=1),
           ifelse(s<0 & DOF!='B',r(h(s),l=h(-T), u=0, a=0, b=0.5),
           ifelse(s>0 & DOF=='B', r(h(s),l=0, u=h(T), a=0.5, b=1),
           ifelse(s<0 & DOF=='B', r(h(s),l=h(-T), u=0, a=0, b=1),1
                  )))))
}

RC_index.lamp <- function(x=NA, GL=NA, Lower=NA, Upper=NA, T=NA, DOF=NA) {
    lambda <- ifelse(DOF=='H', 1, -1)
    s <- ifelse(DOF!='B' & x<=GL, 1/(1+exp(lambda*((x/GL)-1)*T)),
         ifelse(DOF!='B' & x>GL, 1/(1+exp(lambda*-1*((GL/x)-1)*T)),
         ifelse(DOF=='B' & x>=Lower & x<=Upper, 1,
         ifelse(DOF=='B' & x<Lower, 1/(0+exp(-1*((x/Lower)-1)*T)),
                1/(exp(1*((x/Upper)-1)*T))))
         ))
    return(s)
}


#' @title Visualize indices
#' @description Visualize the general response curve associated with a
#'     index formulation.
#' @param type an abbreviation of the index formulation to use.  Must
#'     be one of 'Binary' (Binary), 'MAMP' (Modified Amplitude).
#' @param fold a numeric vector the same length as \code{x}.  Applied
#'     when \code{type='MAMP'}, indicating the multiplier and
#'     divisory applied to the \code{GL} (on a log_2 scale) for
#'     determining the lower and upper bounds of the index scaling
#'     and capping region.
#' @param capped a logical indicating whether the indices should be
#'     capped to the range [\code{lBound}, \code{uBound}].
#' @param scaled a logical indicating whether the indices should be
#'     scaled to the range [0,1].  This cannot be applied to HSAMP or
#'     LAMP, the former of which is already scaled.
#' @param min, max minimum and maximum expected data range
#' @param GL guideline value
#' @param Lower, Upper lower and upper guideline values (when
#'     guidelines are a range)
#' @param lBound, uBound lower and upper scaling and capping limits
#' @param cutsH, cutsL, cutsB a string of Break:Score pairs for Chart
#'     index formulation.
#' @return a ggplot
#' @author Murray Logan
#' @examples
#' library(tidyverse)
#' RC_visualize_indices(type='WCS', scaled=TRUE)
#' RC_visualize_indices(type='WCS', scaled=TRUE)
#' RC_visualize_indices(type='MAMP', fold=2)
#' RC_visualize_indices(type='MAMP', fold=2, scaled=TRUE)
#' RC_visualize_indices(type='MAMP', fold=2, capped=TRUE, scaled=FALSE)
#' RC_visualize_indices(type='MAMP', fold=5, capped=TRUE, scaled=FALSE)
#' RC_visualize_indices(type='MAMP', fold=5, capped=TRUE, scaled=TRUE)
#' RC_visualize_indices(type='MAMP', fold=2, capped=TRUE, scaled=TRUE)
#' RC_visualize_indices(type='HSAMP')
#' RC_visualize_indices(type='HSAMP', T=5)
#' RC_visualize_indices(type='LAMP', T=2)
#' RC_visualize_indices(type='LAMP', T=7)
#' RC_visualize_indices(type='Chart')
#' @export
RC_visualize_indices <- function(type=NA, fold=2, T=2, capped=FALSE, scaled=FALSE, min=NA, max=NA, GL=50, Lower=40, Upper=60, lBound=20, uBound=80, cutsH='0=0:15=0.2:30=0.4:50=0.6:65=0.8:80=1:100=1', cutsL='0=1:5=1:15=0.8:30=0.6:50=0.4:65=0.2:80=0:100=0', cutsB='0=0:10=0.20:20=0.35:30=0.5:40=0.7:45=0.9:55=0.9:60=0.7:70=0.5:80=0.35:90=0.2:100=0') {
    if (!type %in% c('Binary','WCS','MAMP', 'HSAMP','LAMP', 'Chart')) stop("type must be one of 'Binary','MAMP', 'HSMAMP','LAMP', 'Chart'")
    switch(type,
           'Binary'={
               if (is.na(min)) min=0
               if (is.na(max)) max=100
               x <- seq(min,max, l=1000)
               iH = RC_index.binary(x=x, GL=GL, DOF='H')
               iL = RC_index.binary(x=x, GL=GL, DOF='L')
               iB = RC_index.binary(x=x, GL=GL, Lower=Lower, Upper=Upper, DOF='B')
               df = rbind(data.frame(DOF='H', x=x, i=iH),
                          data.frame(DOF='L', x=x, i=iL),
                          data.frame(DOF='B', x=x, i=iB))
               gl = rbind(data.frame(DOF='H', g=GL, l=NA, u=NA),
                          data.frame(DOF='L', g=GL, l=NA, u=NA),
                          data.frame(DOF='B', g=NA, l=Lower, u=Upper))
               ggplot2::ggplot() +
                   ggplot2::geom_line(data=df, ggplot2::aes(y=i,x=x)) +
                   ggplot2::geom_rect(data=gl, ggplot2::aes(ymin=Inf, ymax=-Inf, xmin=l, xmax=u), fill='blue', alpha=0.3) +
                   ggplot2::geom_vline(data=gl, ggplot2::aes(xintercept=g), color='blue') +
                   ggplot2::facet_grid(~DOF) +
                   ggplot2::scale_y_continuous('Binary')
           },
           'WCS'={
               if (is.na(min)) min=0
               if (is.na(max)) max=100
               x <- seq(min,max, l=1000)
               iH = RC_index.wcs(x=x, GL=Lower, uBound=uBound,DOF='H', scaled=scaled)
               iL = RC_index.wcs(x=x, GL=Lower, lBound=lBound,DOF='L', scaled=scaled)
               iB = RC_index.wcs(x=x, GL=Lower, Lower=Lower, Upper=Upper, lBound=lBound, uBound=uBound, DOF='B', scaled=scaled)
               df = rbind(data.frame(DOF='H', x=x, i=iH),
                          data.frame(DOF='L', x=x, i=iL),
                          data.frame(DOF='B', x=x, i=iB))
               gl = rbind(data.frame(DOF='H', g=Lower, l=NA, u=NA),
                          data.frame(DOF='L', g=Lower, l=NA, u=NA),
                          data.frame(DOF='B', g=NA, l=Lower, u=Upper))
               ggplot2::ggplot() +
                   ggplot2::geom_line(data=df, ggplot2::aes(y=i,x=x)) +
                   ggplot2::geom_rect(data=gl, ggplot2::aes(ymin=Inf, ymax=-Inf, xmin=l, xmax=u), fill='blue', alpha=0.3) +
                   ggplot2::geom_vline(data=gl, ggplot2::aes(xintercept=g), color='blue') +
                   ggplot2::facet_grid(~DOF) +
                   ggplot2::scale_y_continuous('WCS')
           },
           'MAMP'={
               xx=c(GL/fold, GL, GL*fold)
               #if (is.na(min)) xx <- c(25,50,100)
               if (is.na(min)) min=1
               if (is.na(max)) max=1/0.001
               x <- seq(min,max, l=1000)
               iH = RC_index.mamp(x=x, GL=GL,fold=fold, DOF='H', capped=capped, scaled=scaled)
               iL = RC_index.mamp(x=x, GL=GL,fold=fold, DOF='L', capped=capped, scaled=scaled)
               iB = RC_index.mamp(x=x, GL=GL,Lower=Lower, Upper=Upper,fold=fold, DOF='B', capped=capped, scaled=scaled)
               df = rbind(data.frame(DOF='H', x=x, i=iH),
                          data.frame(DOF='L', x=x, i=iL),
                          data.frame(DOF='B', x=x, i=iB))
               gl = rbind(data.frame(DOF='H', g=GL, l=NA, u=NA),
                          data.frame(DOF='L', g=GL, l=NA, u=NA),
                          data.frame(DOF='B', g=NA, l=Lower, u=Upper))
               log2it <- function(x) (x)^2/(1+(x)^2)
               ggplot2::ggplot() +
                   ggplot2::geom_line(data=df, ggplot2::aes(y=i,x=log2it(x/GL))) +
                   ggplot2::geom_rect(data=gl, ggplot2::aes(ymin=Inf, ymax=-Inf, xmin=log2it(l/GL), xmax=log2it(u/GL)), fill='blue', alpha=0.3) +
                   ggplot2::geom_vline(data=gl, ggplot2::aes(xintercept=log2it(g/GL)), color='blue') +
                   ggplot2::scale_x_continuous('x', breaks=log2it(xx/GL), labels=round(xx,2), limits=c(0,1)) +
                   ggplot2::facet_grid(~DOF) +
                   {if (scaled==TRUE) ggplot2::scale_y_continuous('fsMAMP')} + 
                   {if (scaled==FALSE) ggplot2::scale_y_continuous('MAMP')}
           },
           'HSAMP'={
               if (is.na(min)) min=0
               if (is.na(max)) max=100
               x <- seq(min,max, l=1000)
               iH = RC_index.hsamp(x=x, GL=GL, lBound=lBound, uBound=uBound, T=T, DOF='H')
               iL = RC_index.hsamp(x=x, GL=GL, lBound=lBound, uBound=uBound, T=T, DOF='L')
               iB = RC_index.hsamp(x=x, GL=NA, Lower=Lower, Upper=Upper, lBound=lBound, uBound=uBound, T=T, DOF='B')
               df = rbind(data.frame(DOF='H', x=x, i=iH),
                          data.frame(DOF='L', x=x, i=iL),
                          data.frame(DOF='B', x=x, i=iB))
               gl = rbind(data.frame(DOF='H', g=GL, l=NA, u=NA, L=lBound, U=uBound),
                          data.frame(DOF='L', g=GL, l=NA, u=NA, L=lBound, U=uBound),
                          data.frame(DOF='B', g=NA, l=Lower, u=Upper, L=lBound, U=uBound))
               ggplot2::ggplot() +
                   ggplot2::geom_line(data=df, ggplot2::aes(y=i,x=x)) +
                   ggplot2::geom_rect(data=gl, ggplot2::aes(ymin=Inf, ymax=-Inf, xmin=l, xmax=u), fill='blue', alpha=0.3) +
                   ggplot2::geom_vline(data=gl, ggplot2::aes(xintercept=g), color='blue') +
                   ggplot2::facet_grid(~DOF) +
                   ggplot2::scale_y_continuous('HSAMP')
           },
           'LAMP'={
               if (is.na(min)) min=0.0011
               if (is.na(max)) max=1/0.001
               x <- seq(min,max, l=1000)
               iH = RC_index.lamp(x=x, GL=GL, T=T, DOF='H')
               iL = RC_index.lamp(x=x, GL=GL, T=T, DOF='L')
               iB = RC_index.lamp(x=x, GL=GL, Lower=Lower, Upper=Upper, T=T, DOF='B')
               df = rbind(data.frame(DOF='H', x=x, i=iH),
                          data.frame(DOF='L', x=x, i=iL),
                          data.frame(DOF='B', x=x, i=iB))
               frth <- function() {
                   scales::trans_new('frth',function(x) x^0.25,function(x) x^4, domain=c(0,Inf))
               }
               ggplot2::ggplot(data=df, ggplot2::aes(y=i,x=x)) + ggplot2::geom_line() +
                   ggplot2::geom_vline(xintercept=GL) + 
                   ggplot2::scale_x_continuous('x', trans=frth(), breaks=c(25,50,100), limits=c(15,130)) +
                   ggplot2::facet_grid(~DOF)
           },
           'Chart'={
               getCuts = function(cuts) {
                   t(sapply(strsplit(unlist(strsplit(cuts, ':')), '='), 'as.numeric'))
                   }
               if (is.na(min)) min=0
               if (is.na(max)) max=100
               x <- seq(min,max, l=1000)
               iH = RC_index.chart(x=x, Cuts=cutsH)
               iL = RC_index.chart(x=x, Cuts=cutsL)
               iB = RC_index.chart(x=x, Cuts=cutsB)
               df = rbind(data.frame(DOF='H', x=x, i=iH),
                          data.frame(DOF='L', x=x, i=iL),
                          data.frame(DOF='B', x=x, i=iB))
               gl = rbind(data.frame(DOF='H', x=getCuts(cutsH)[,1], y=getCuts(cutsH)[,2]),
                          data.frame(DOF='L', x=getCuts(cutsL)[,1], y=getCuts(cutsL)[,2]),
                          data.frame(DOF='B', x=getCuts(cutsB)[,1], y=getCuts(cutsB)[,2]))
               ggplot2::ggplot() +
                   ggplot2::geom_line(data=df, ggplot2::aes(y=i,x=x)) +
                   ggplot2::geom_segment(data=gl, ggplot2::aes(yend=y, y=y, xend=-Inf, x=x), color='grey') +
                   ggplot2::geom_segment(data=gl, ggplot2::aes(yend=-Inf, y=y, xend=x, x=x), color='grey') +
                   #ggplot2::geom_rect(data=gl, ggplot2::aes(ymin=Inf, ymax=-Inf, xmin=l, xmax=u), fill='blue', alpha=0.3) +
                   #ggplot2::geom_vline(data=gl, ggplot2::aes(xintercept=g), color='blue') +
                   ggplot2::facet_grid(~DOF) +
                   ggplot2::scale_y_continuous('Chart')
           }
           )
}



