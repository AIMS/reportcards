#' Generate the data sets
#' Assumes that the working directory is the root of this package
#' (i.e. the parent of the directory containing this file).
#' 

devtools::load_all()

library(dplyr)
library(tidyr)
set.seed(123)
source('../R/utils.R')

generateValues <- function(x, base, reportyear, month, site, sd, family, decimal.places=3) {
    Xmat <- model.matrix(~factor(ReportYear)+factor(Month) + factor(Site), x)
    coefs <- c(base, reportyear, month, site)
    round(family$linkinv(as.vector(coefs %*% t(Xmat)) + rnorm(n=nrow(x), mean=0, sd=sd)), decimal.places)
}

Year=2010:2018
Month=month.abb[seq(2,12, by=3)]
Site=paste('Site', 1:12) # these will be in Zones (ZoneA=1:3,
                         # ZoneB=1:2, ZoneC=1:4, ZoneD=1:3
Measure=c('TP','TN','Chla','Secchi','pH','Cover','Biomass')
reportcardData = expand.grid(Year=Year,Month=Month) %>% dplyr::arrange(Year)
reportcardData = cbind(Site,reportcardData[rep(1:nrow(reportcardData), each=length(Site)),]) %>%
    mutate(Site=factor(Site, levels=paste('Site', 1:12)))
reportcardData = cbind(Measure,reportcardData[rep(1:nrow(reportcardData), each=length(Measure)),]) %>%
    mutate(Measure=factor(Measure, levels=c('TP','TN','Chla','Secchi','pH','Cover','Biomass')))
reportcardData = reportcardData %>% group_by(Year) %>%
    mutate(Date=as.Date(paste0(Year,'-',Month,'-', runif(1,5,20)), format=c('%Y-%b-%d')),
           Season=RC_makeWetDrySeason(Date),
           ReportYear=RC_waterYear(Date)) %>%
    ungroup %>%
    dplyr::select(Date, ReportYear, Month, Season, Site, Measure) %>%
    arrange(Date, Site, Measure)


## widen (spread) the data to make it easier to simulate data
tmp=reportcardData %>% mutate(Value=1) %>%
    dplyr::select(ReportYear, Month, Site, Measure, Value) %>%
    spread(key=Measure, value=Value) %>%
    mutate(TP=generateValues(x=., base=log(3), reportyear=c(1,3,6,10,11,14,7,3,2)/20, month=c(-1,-1,0)/2, site = log(c(1.1,0.9, 1.5,2.7, 1.5,1.2,1.6,1.3, 1.1,1.2,1.6)/1), sd=0.2, family=Gamma(link='log')),
           TN=generateValues(x=., base=log(10), reportyear=log(c(1,3,6,10,11,14,7,3,2)), month=c(-1,-1,0)/2, site = log(c(1.1,0.9, 4.5,5, 1.5,1.2,1.6,1.3, 1.1,1.2,1.6)/5), sd=0.5, family=Gamma(link='log')),
           Chla=generateValues(x=., base=log(.5),reportyear=log(c(8,9,9.5,10,12,15,9,8.5,8)/10), month=c(-1,-1,0)/2, site = log(c(1.1,0.9, 1.6,1.7, 1.5,1.2,1.6,1.3, 1.1,1.2,1.6)), sd=0.1, family=Gamma(link='log')),
           Secchi=generateValues(x=., base=log(10),reportyear=log((15-c(1,3,6,10,11,14,7,3,2))/10), month=log(c(2,2,1)/2), site = log(c(1.1,0.9, 0.5,0.4, 1.5,1.2,1.6,1.3, 1.1,1.2,1.6)/2), sd=0.3, family=Gamma(link='log')),
           Cover=generateValues(x=., base=log(.2),reportyear=log((15-c(1,3,6,10,11,14,7,3,2))/5), month=c(1,1,0)/100, site = log(c(1.1,1.9, 4.5,5, 1.5,1.2,3.6,1.3, 1.1,1.2,1.6)/5), sd=0.4, family=binomial(link='logit')),
           Biomass=generateValues(x=., base=log(100),reportyear=log((15-c(1,3,6,10,11,14,7,3,2))/5), month=c(1,1,0), site = log(c(1.1,0.9, 4.5,5, 1.5,1.2,1.6,1.3, 1.1,1.2,1.6)/5), sd=0.3, family=Gamma(link='log')),
           pH=generateValues(x=., base=7,reportyear=c(1,3,6,10,11,14,7,3,2)/10, month=c(-1,-1,0), site = c(1.1,0.9, 4.5,5, 1.5,1.2,1.6,1.3, 1.1,1.2,1.6)/10, sd=0.3, family=gaussian())           
           ) %>%
    mutate(Biomass = ifelse(Site %in% paste('Site', c(1,4,5)), Biomass, NA),
           Cover = ifelse(!Site %in% paste('Site', c(4,5)), Cover, NA))

tmp = tmp %>% gather(key=Measure, value=Value, -ReportYear,-Month,-Site)
reportCard = reportcardData %>% left_join(tmp) %>% dplyr::select(Date, Site, Measure, Value)
reportcardData = reportcardData %>% left_join(tmp) %>% dplyr::select(Date, Site, Measure, Value) %>%
    spread(key=Measure, value=Value)
## remove an observations so that it is artificially unbalanced
## we will make this the TN observation from Site 6 on the last visit
reportcardData[with(reportcardData, which(Site=='Site 6' & Date==as.Date('2018-02-13'))),'TN'] = NA
reportCard = reportCard[-with(reportCard, which(Site=='Site 6' & Date==as.Date('2018-02-13') & Measure=='TN')),]

## reportcardData %>%
##     gather(key=Measure, value=Value, -Date, -Site) %>%
##     ggplot(aes(y=Value, x=Date)) + 
##     geom_line() +
##     facet_grid(Measure~Site, scales='free') +
##     theme_bw()     

## To complete these data, we also need the Measure, Spatial and Temporal hierarchies
indicator.hier = rbind(
    data.frame(Component='Environmental', Indicator='Habitats',Subindicator='Seagrass',Measure='Biomass'),
    data.frame(Component='Environmental', Indicator='Habitats',Subindicator='Coral',Measure='Cover'),
    data.frame(Component='Environmental', Indicator='Water Quality',Subindicator='Productivity',Measure='Chla'),
    data.frame(Component='Environmental', Indicator='Water Quality',Subindicator='Physical',Measure='pH'),
    data.frame(Component='Environmental', Indicator='Water Quality',Subindicator='Nutrients',Measure='TN'),
    data.frame(Component='Environmental', Indicator='Water Quality',Subindicator='Nutrients',Measure='TP'),
    data.frame(Component='Environmental', Indicator='Water Quality',Subindicator='Clarity',Measure='Secchi')
) 

spatial.hier = rbind(
    data.frame(Region='Whole Bay', Zone='Zone A', Site=paste('Site',1:3)),
    data.frame(Region='Whole Bay', Zone='Zone B', Site=paste('Site',4:5)),
    data.frame(Region='Whole Bay', Zone='Zone C', Site=paste('Site',6:9)),
    data.frame(Region='Whole Bay', Zone='Zone D', Site=paste('Site',10:12))
    )


usethis::use_data(reportcardData, indicator.hier, spatial.hier, overwrite = TRUE)




## ## The following function is used to generate fabricated report card data
## generateValues <- function(x, base, year, month, zone, site, sd, family, decimal.places=3) {
##     Xmat <- model.matrix(~factor(Year)+factor(Month) + factor(Zone) + factor(Site), x)
##     coefs <- c(base, year, month, zone, site)
##     round(family$linkinv(as.vector(coefs %*% t(Xmat)) + rnorm(n=nrow(x), mean=0, sd=sd)), decimal.places)
## }

## ## Define the various structural elements (variables) of the
## ## fabricated data
## Year=2010:2018
## Month=month.abb[seq(2,12, by=3)]
## Zone=paste0('Zone', LETTERS[1:4])
## Site=paste('Site', 1:3)
## Component='Environment'
## Indicator=c(rep('Water Quality',5),rep('Habitats',2))
## Subindicator=c('Nutrients','Nutrients','Productivity','Clarity','Physical','Coral','Seagrass')
## Measure=c('TP','TN','Chla','Secchi','pH','Cover','Biomass')


## ## Create the structure
## reportcardData = data.frame(Component,Indicator,Subindicator,Measure)
## reportcardData = cbind(Zone,reportcardData[rep(1:nrow(reportcardData), each=length(Zone)),])
## reportcardData = cbind(Site,reportcardData[rep(1:nrow(reportcardData), each=length(Site)),])
## reportcardData = cbind(Year,reportcardData[rep(1:nrow(reportcardData), each=length(Year)),])
## reportcardData = cbind(Month,reportcardData[rep(1:nrow(reportcardData), each=length(Month)),])
## reportcardData = reportcardData %>%
##     mutate(Date=as.Date(paste0(Year,'-',Month,'-', '15'), format=c('%Y-%b-%d')),
##            Season=RC_makeWetDrySeason(Date),
##            ReportYear=RC_waterYear(Date)) %>%
##     arrange(Date, Zone,Site,Component,Indicator,Subindicator,Measure)

## ## widen (spread) the data to make it easier to simulate data
## tmp=reportcardData %>% mutate(Value=1) %>%
##     dplyr::select(Year, Month, Zone, Site, Measure, Value) %>%
##     spread(key=Measure, value=Value) %>%
##     mutate(TP=generateValues(x=., base=1, year=c(1,3,6,10,11,14,7,3)/20, month=c(-1,-1,0), zone = c(2,1,1.3)/2, sd=0.2, site=c(1,2), family=Gamma(link='log')),
##            TN=generateValues(x=., base=log(10), year=log(c(1,3,6,10,11,14,7,3)), month=c(-1,-1,0), zone = log(c(3,1,1.3)), site=log(c(1,2)), sd=0.5, family=Gamma(link='log')),
##            Chla=generateValues(x=., base=log(.5), year=log(c(1,3,6,10,11,14,7,3)/5), month=c(-1,-1,0), zone = log(c(3,1,1.3)), site=log(c(1,2)), sd=0.1, family=Gamma(link='log')),
##            Secchi=generateValues(x=., base=log(10), year=log(15-c(1,3,6,10,11,14,7,3)/10), month=c(1,1,0), zone = log(c(3,10,10.3)/10), site=log(c(3,10)/10), sd=0.3, family=Gamma(link='log')),
##            Cover=generateValues(x=., base=log(.2), year=log((15-c(1,3,6,10,11,14,7,3))/5), month=c(1,1,0), zone = log(c(0.2,1,1.3)), site=log(c(0.1,0.2)), sd=0.4, family=binomial(link='logit')),
##            Biomass=generateValues(x=., base=log(100), year=log(15-c(1,3,6,10,11,14,7,3)/1), month=c(1,1,0), zone = log(c(3,10,5.3)/10), site=log(c(3,10)/10), sd=0.3, family=Gamma(link='log')),
##            pH=generateValues(x=., base=7, year=-1*c(1,3,6,10,11,14,7,3)/10, month=c(-1,-1,0), zone = c(3,10,5.3)/10, site=c(1,3), sd=0.3, family=gaussian()))

## tmp = tmp %>% gather(key=Measure, value=Value, -Year,-Month,-Zone,-Site)

## reportcardData = reportcardData %>% left_join(tmp)

## usethis::use_data(reportcardData, overwrite = TRUE)



## Now for the Guidelines data
Zone=paste('Zone', LETTERS[1:4])
                                        #guidelines = data.frame(Component,Indicator,Subindicator,Measure)
#guidelines = cbind(Zone,Measure=data.frame(guidelines[rep(1:nrow(guidelines), each=length(Zone)),])) %>%
guidelines = expand.grid(Zone=Zone,Measure=Measure) %>%
    mutate(GL=ifelse(Measure=='Cover', 0.2,
              ifelse(Measure=='Biomass', 200,
              ifelse(Measure=='Secchi', 10,
              ifelse(Measure=='TN',10,
              ifelse(Measure=='TP',2.5,
              ifelse(Measure=='Chla',0.5,
              ifelse(Measure=='pH',NA,NA)))))))) %>%
    mutate(Lower=ifelse(Measure=='Cover', NA,
              ifelse(Measure=='Biomass', NA,
              ifelse(Measure=='Secchi', NA,
              ifelse(Measure=='TN',NA,
              ifelse(Measure=='TP',NA,
              ifelse(Measure=='Chla',NA,
              ifelse(Measure=='pH',6.5,NA)))))))) %>%
    mutate(Upper=ifelse(Measure=='Cover', NA,
              ifelse(Measure=='Biomass', NA,
              ifelse(Measure=='Secchi', NA,
              ifelse(Measure=='TN',NA,
              ifelse(Measure=='TP',NA,
              ifelse(Measure=='Chla',NA,
              ifelse(Measure=='pH',7.5,NA)))))))) %>%
    mutate(lBound=ifelse(Measure=='Cover', 0.05,
              ifelse(Measure=='Biomass', 100,
              ifelse(Measure=='Secchi', 5,
              ifelse(Measure=='TN',5,
              ifelse(Measure=='TP',1,
              ifelse(Measure=='Chla',0.2,
              ifelse(Measure=='pH',6,NA)))))))) %>%
    mutate(uBound=ifelse(Measure=='Cover', 0.4,
              ifelse(Measure=='Biomass', 400,
              ifelse(Measure=='Secchi', 20,
              ifelse(Measure=='TN',20,
              ifelse(Measure=='TP',6,
              ifelse(Measure=='Chla',0.9,
              ifelse(Measure=='pH',8,NA)))))))) %>%
    mutate(DOF=ifelse(Measure=='Cover', 'L',
              ifelse(Measure=='Biomass', 'L',
              ifelse(Measure=='Secchi', 'L',
              ifelse(Measure=='TN','H',
              ifelse(Measure=='TP','H',
              ifelse(Measure=='Chla','H',
              ifelse(Measure=='pH','B',NA)))))))) %>%
    mutate(Cuts=ifelse(Measure=='Cover', '0=0:0.15=0.2:0.30=0.4:0.50=0.6:0.65=0.8:0.80=1:1=1',
              ifelse(Measure=='Biomass', '',
              ifelse(Measure=='Secchi', '',
              ifelse(Measure=='TN','',
              ifelse(Measure=='TP','',
              ifelse(Measure=='Chla','',
              ifelse(Measure=='pH','',NA)))))))) %>%    
    mutate(Measure=as.character(Measure))

usethis::use_data(guidelines, overwrite = TRUE)
 


reportCard = reportCard %>%
    mutate(Month=format(Date, '%b'),
           Season=RC_makeWetDrySeason(Date),
           ReportYear=RC_waterYear(Date)) %>%
    ungroup %>%
    left_join(spatial.hier) %>%
    left_join(indicator.hier) %>%
    left_join(guidelines)
usethis::use_data(reportCard, overwrite = TRUE)

