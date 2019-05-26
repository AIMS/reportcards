##' @title QAQC plots
##' @param df a data frame or tibble
##' @param type a string indicating the type of QAQC plot. Must be one
##'     of 'Missing' (spatio-temporal sample size), 'TS'
##'     (Spatio-temporal plot) or 'Outliers (Outlier detection plots).
##' @return a ggplot object
##' @author Murray Logan
##' @examples
##' library(tidyverse)
##' qaqc(df=reportcardData, type='Missing')
##' qaqc(df=reportcardData, type='TS')
qaqc <- function(df=NULL, GL=guidelines, year=max(df$ReportYear), type='Missing') {
    if (is.null(df) | class(df)!='data.frame') stop('You must supply a data.frame')
    if (!type %in% c('Missing','TS', 'Outliers')) stop("type must be one of 'Missing', 'TS' or 'Outliers'")
    switch(type,
           "Missing"=qaqc.missing(df=df),
           "TS"=qaqc.ts(df=df),
           "Outliers"=qaqc.outliers(df=df, GL=GL, year=2018)
           )
}



qaqc.missing <- function(df) {
    #full = with(df, expand.grid(Zone=factor(unique(Zone)), Year=unique(Year), Measure=unique(Measure))) %>%
    full = with(df, expand.grid(Site=factor(unique(Site)), Year=unique(Year), Measure=unique(Measure))) %>%
        mutate(Measure=as(Measure, Class=class(df$Measure)))
    print(head(full))
    df = df %>% full_join(full) %>%
        mutate(Missing=ifelse(is.na(Value),TRUE,FALSE))
    #df %>% dplyr::group_by(Zone,Year, Measure) %>% dplyr::count(Value) %>% print

    ggplot2::ggplot(df, ggplot2::aes(y=Site, x=Year)) +
        ggplot2::geom_point(ggplot2::aes(color=Missing)) +
        ggplot2::facet_grid(Zone~Subindicator+Measure, space='free', scale='free', as.table=TRUE) +
        ggplot2::scale_color_manual(breaks=c(FALSE,TRUE), values=c('black','red'), labels=c(FALSE,TRUE))
}

qaqc.ts <- function(df) {
    stripes = with(df, seq(as.Date(paste0(format(min(Date),'%Y'),'-10-01')),
                           as.Date(paste0(format(max(Date),'%Y'),'-10-01')), by='2 years'))
    
    df %>% 
        ggplot(aes(y=Value, x=Date, group=Site)) +
        geom_blank() +
        annotate(geom='rect', ymin=Inf, ymax=-Inf, xmin=stripes, xmax=stripes+365, alpha=0.1) +
        geom_line(color='grey70') + 
        geom_line(stat='summary', aes(x=as.Date(paste0(ReportYear,'-01-01'))), color='red') +
        geom_smooth(fill='blue', alpha=0.05) +
        geom_point() +
        facet_grid(Subindicator+Measure~Zone, scales='free') +
        theme_bw() 

}


qaqc.outliers <- function(df, GL=GL, type=NA, fold=2, year=NA) {
    df = df %>% filter(ReportYear==year)
    ##Round Dates to the month of sampling
    df = df %>% mutate(iDate = lubridate:::floor_date(Date,'month'),
                       iDate = as.numeric(as.factor(as.character(iDate))))

    GL = df %>%
        mutate(nSite=as.numeric(Site)) %>%
        group_by(Zone) %>% 
        mutate(nSite=nSite-min(nSite) + 1) %>%
        ungroup %>% 
        dplyr::select(GL, DOF, Lower, Upper, lBound, uBound, Measure, nSite, Zone) %>% distinct

    if (type=='MAMP') GL = GL %>% mutate(lGL=ifelse(DOF!='B',NA, Lower),
                                         uGL=ifelse(DOF!='B',NA, Upper),
                                         LWR=ifelse(DOF!='B', GL/fold, lGL/fold),
                                         UPR=ifelse(DOF!='B', GL*fold, uGL*fold)
                                         )
    if (type=='HSAMP') GL = GL %>% mutate(LWR=lBound, UPR=uBound) 
    if (type=='Binary') GL = GL %>% mutate(LWR=NA, UPR=NA) 

    df = df %>% full_join(GL) %>%
        mutate(Outlier = ifelse(!is.na(LWR) | Value<LWR | Value> UPR, TRUE,FALSE))
    
    df %>% 
        #left_join(GL) %>%
        ggplot() +
        geom_blank(aes(y=Site, x=Value)) +
        {if (type!='Binary') geom_rect(data=GL, aes(xmin=LWR, xmax=UPR, ymin=nSite-0.5, ymax=nSite+0.5), fill='red', alpha=0.3)} +
        {if (type!='Binary') geom_rect(data=GL, aes(xmin=LWR, xmax=UPR, ymin=nSite-0.5, ymax=nSite+0.5), fill='red', alpha=0.3)} +
        geom_vline(data=GL, aes(xintercept=GL), color='red') +
        geom_rect(data=GL, aes(xmin=lGL, xmax=uGL, ymin=nSite-0.5, ymax=nSite+0.5), fill='red', alpha=0.3) +
        #geom_point(aes(y=Site, x=Value)) +
        {if (type!='Binary') {geom_text(aes(y=Site, x=Value,label=iDate, color=Outlier), size=3) 
         } else geom_text(aes(y=Site, x=Value,label=iDate), size=3)} +
        #{if (type!='Binary') {
        #     scale_color_manual(breaks=c(FALSE,TRUE, NA), values=c('black','red', 'grey'))
        # 
        #scale_x_log10('',breaks=as.vector(c(1,2,5,10) %o%10^c(-1:5)))+
        facet_grid(Zone~Measure, scales='free', space='free_y', as.table=TRUE) +
        theme_classic(10)+
        theme(plot.margin=unit(c(0,0,0,0),'lines'),
              axis.ticks.length=unit(0.1,'lines'),
              panel.background=element_rect(color='black', fill=NA))

}
