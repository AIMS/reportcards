
#' @title Generate Grades
#' @description Convert report card Scores into Grades.
#' @param x a vector of numerical values representing the report card
#'     Scores.  These values should be in the range of 0-1.
#' @param type a string indicating the conversion method.  Must be one
#'     of 'Uniform' (for Uniform grade boundaries used by the GBR
#'     Report Card), 'MMP' (for grade boundaries used by the GBR
#'     Marine Monitoring Program Report Card), GHHP (for grade
#'     boundaries used by the Gladstone Healthy Harbour Partnership
#'     Report Card) or 'MCWC' (for the grade boundaries used by the
#'     MidCoast Council Waterway and Catchment Report Card).
#' @return a vector of alpha-numeric Grades ('A', 'B', 'C', 'D' or 'E').
#' @export
#' @examples
#' RC_generateGrades(x=seq(0,1,length=6), type='Uniform')
RC_generateGrades <- function(x,type='MMP') {
  RC_gradeBoundaries.mmp = c(1, 0.5 + (2/3*0.5), 0.5+1/3*0.5, 0.5, 0.25,0)
  RC_gradeBoundaries.uniform = c(1, 0.8, 0.6, 0.4, 0.2, 0)
  RC_gradeBoundaries.ghhp = c(1, 0.85, 0.65, 0.5, 0.25, 0)
  RC_gradeBoundaries.mcwc = c(1, 0.93, 0.73, 0.56, 0.40, 0)

  if (!type %in% c('Uniform','MMP','GHHP', 'MCWC')) warning('type must be either MMP, GHHP or Uniform')
  g = switch(type,
             'Uniform'=ifelse(is.na(x) | x<0 | x>1,NA,ifelse(x>=RC_gradeBoundaries.uniform[2], 'A', ifelse(x>=RC_gradeBoundaries.uniform[3], 'B', ifelse(x>=RC_gradeBoundaries.uniform[4], 'C',  ifelse(x>=RC_gradeBoundaries.uniform[5], 'D', 'E'))))),
             'MMP'=ifelse(is.na(x) | x<0 | x>1,NA, ifelse(x>=RC_gradeBoundaries.mmp[2], 'A', ifelse(x>=RC_gradeBoundaries.mmp[3], 'B', ifelse(x>=RC_gradeBoundaries.mmp[4], 'C',  ifelse(x>=RC_gradeBoundaries.mmp[5], 'D', 'E'))))),
             'GHHP'=ifelse(is.na(x) | x<0 | x>1,NA,ifelse(x>=RC_gradeBoundaries.ghhp[2], 'A', ifelse(x>=RC_gradeBoundaries.ghhp[3], 'B', ifelse(x>=RC_gradeBoundaries.ghhp[4], 'C',  ifelse(x>=RC_gradeBoundaries.ghhp[5], 'D', 'E'))))),
             'MCWC'=ifelse(is.na(x) | x<0 | x>1,NA,ifelse(x>=RC_gradeBoundaries.mcwc[2], 'A', ifelse(x>=RC_gradeBoundaries.mcwc[3], 'B', ifelse(x>=RC_gradeBoundaries.mcwc[4], 'C',  ifelse(x>=RC_gradeBoundaries.mcwc[5], 'D', 'E')))))
  )
  return(g)
}


##' @title Simple hierarchical aggregation
##' @param df a data frame or tibble containing at least the following
##'     fields: Score, Weights
##' @param grouping_cols a vector of strings indicating which columns
##'     to group the aggregation by
##' @param fun a function used in the aggregation
##' @param type a string indicating the conversion method.  Must be one
##'     of 'Uniform' (for Uniform grade boundaries used by the GBR
##'     Report Card), 'MMP' (for grade boundaries used by the GBR
##'     Marine Monitoring Program Report Card), GHHP (for grade
##'     boundaries used by the Gladstone Healthy Harbour Partnership
##'     Report Card) or 'MCWC' (for the grade boundaries used by the
##'     MidCoast Council Waterway and Catchment Report Card).
##' @return an aggregated data frame (tibble)
##' @author Murray Logan
##' @seealso \code{\link{RC_boot_aggregate}}, \code{\link{RC_index}}
##' @examples
##' library(tidyverse)
##' data(reportCard)
##' reportCard.idx = reportCard %>%      #generate fsMAMP indices
##'     mutate(Score=RC_index(x=Value, GL=GL, Lower=Lower, #calculate index
##'     Upper=Upper, fold=2, DOF=DOF, type='MAMP', capped=TRUE,
##'     scaled=TRUE))
##' ## Aggregate over all the samples collected throughout the year
##' reportyear.site.measure = reportCard.idx %>%
##'     mutate(Weight=1) %>%   # add weights
##'     RC_aggregate(grouping_cols=c('ReportYear','Zone','Site','Component','Indicator','Subindicator', 'Measure'))
##' head(reportyear.site.measure)
##' @export
RC_aggregate <- function(df, grouping_cols=colnames(df)[!colnames(df) %in% c('Score','Grade')], fun=weighted.mean, gradetype='Uniform') {
  df %>%
      dplyr::group_by(!!!syms(grouping_cols)) %>%
      dplyr::summarize(Score=fun(Score, w=Weight, na.rm=TRUE)) %>%
      dplyr::mutate(Grade=RC_generateGrades(Score, type=gradetype)) %>%
      ungroup
}


## The following is a hidden function and should not be called directly.
RC_bootstrap = function(df, size=1000, group='Measure', fun=fun) {
  if (group=='' | is.null(group)) {
    grp=NULL
  } else if (is.list(group)) {
    grp=group
  } else {
    grp=group
  }

  df %>% do({
      x=.
      xx=replicate(n=size, tapply(x$Score, x[[grp]], function(x) sample(x, size=1)))
      if (is.null(dim(xx))) { #if there was only one level of grp,
                              #there is nothing to aggregate, so just
                              #return the original bootstrap scores 
          ##w=rep(x$Weight, size)
          #w=x$Weight
          #xxx=fun(xx, w=w, na.rm=TRUE)
          xxx = x$Score
      }
      else {  # if there are multiple levels of the factor represented
              # by grp 
          w = tapply(x$Weight, x[[grp]], mean)
          xxx = apply(xx, 2, function(x) fun(x, w = w, na.rm=TRUE))
          
      }
      data.frame(Score=xxx)
  })
}



##' @title Bootstrapp aggregation of Report Card Scores
##' @description This function performs bootstrapp aggregation.
##' @param boot a data frame or tibble containing at least the
##'     following fields: Score, Weights
##' @param size an integer, the number of bootstrapp samples
##' @param seed an integer, the random seed to apply
##' @param grouping_cols a vector of strings indicating which columns
##'     to group the aggregation by.
##' @param over a string indicating the column (categorical variable)
##'     to aggregate over
##' @param fun the function to apply in the aggregation
##' @return a list of length 2:
##'     dist: the bootstrapp samples
##'     sum:  summarized
##' @author Murray Logan
##' @details Put the details in
##' @seealso \code{\link{RC_boot_accumulate}},
##'     \code{\link{RC_boot_aggregate}}
##' @examples
##' library(tidyverse)
##' data(reportCard)
##' reportCard.idx = reportCard %>%      #generate fsMAMP indices
##'     mutate(Score=RC_index(x=Value, GL=GL, Lower=Lower, #calculate index
##'     Upper=Upper, fold=2, DOF=DOF, type='MAMP', capped=TRUE,
##'     scaled=TRUE))
##' ## Accummulate over all samples collected throught each year
##' seed=123
##' size=10
##' reportyear.site.measure.boot=reportCard.idx %>%
##'     mutate(Weight=1) %>%   # add weights
##'     RC_boot_accumulate(size=size, seed=seed, grouping_cols=c('ReportYear','Zone','Site','Component','Indicator','Subindicator', 'Measure'))
##' ## Aggregate over Measures within ReportYear/Site/Subindicators
##' reportyear.site.subindicator.boot = reportyear.site.measure.boot$dist %>%
##'     mutate(Weight=1) %>%   # add weights
##'     RC_boot_aggregate(size=size, seed=seed,
##'     grouping_cols=c('ReportYear','Zone','Site','Component','Indicator','Subindicator'), over='Measure')
##' reportyear.site.subindicator.boot$sum
##' @export
RC_boot_aggregate = function(boot=NULL, size=10, seed=123,
                             grouping_cols=colnames(df)[!colnames(df) %in% c('Score','Grade')],
                             over='', fun=weighted.mean) {
  ## Bootstrap
  set.seed(seed)
  x2=boot %>%
    dplyr::group_by(!!!syms(grouping_cols)) %>%
      RC_bootstrap(., size=size, group=over, fun=fun) %>% ungroup

  ## Confidence intervals
  x3=x2 %>%
    dplyr::group_by(!!!syms(grouping_cols)) %>%
    dplyr::summarize(Lower=quantile(Score, p=0.025, na.rm=TRUE),
              Upper=quantile(Score, p=0.975, na.rm=TRUE),
              Boot.Mean=mean(Score, na.rm=TRUE)
    ) %>%
    ungroup
  list(dist=x2, sum=x3)
}

##' @title Bootstrapp accumulation
##' @description This function performs bootstrapp accumulation.  The
##'     accumulation groups together single observations into a
##'     distribution (needed for meaningful bootstrapp aggregation)
##' @param boot a data frame or tibble containing at least the
##'     following fields: Score, Weights
##' @param size an integer, the number of resamples to generate the
##'     accumulated distributions
##' @param seed an integer, the random seed to apply
##' @param grouping_cols a vector of strings indicating which columns
##'     to group the aggregation by.
##' @return a list of length 2:
##'     dist: the bootstrapp samples
##'     sum:  summarized
##' @author Murray Logan
##' @details Put the details in
##' @seealso \code{\link{RC_boot_aggregate}}, \code{\link{RC_aggregate}}
##' @examples
##' library(tidyverse)
##' data(reportCard)
##' reportCard.idx = reportCard %>%      #generate fsMAMP indices
##'     mutate(Score=RC_index(x=Value, GL=GL, Lower=Lower, #calculate index
##'     Upper=Upper, fold=2, DOF=DOF, type='MAMP', capped=TRUE,
##'     scaled=TRUE))
##' ## Accummulate over all samples collected throught each year
##' seed=123
##' size=100
##' reportyear.site.measure.boot=reportCard.idx %>%
##'     mutate(Weight=1) %>%   # add weights
##'     RC_boot_accumulate(size=size, seed=seed, grouping_cols=c('ReportYear','Zone','Site','Component','Indicator','Subindicator', 'Measure'))
##' reportyear.site.measure.boot$sum
##' @export
RC_boot_accumulate = function(boot=NULL, size=10, seed=123,
                              grouping_cols=colnames(df)[!colnames(df) %in% c('Score','Grade')]) {
    ## Bootstrap
    set.seed(seed)
    x2 = boot %>% group_by(!!!syms(grouping_cols)) %>%
        do({
            x = .
            xx=replicate(n=size, sample(x$Score, prob=x$Weight, replace=TRUE))
            if (is.null(dim(xx))) data.frame(Score=rep(x$Score, size))
            else data.frame(Score=colMeans(xx))
        }) %>% ungroup
    x3 = x2 %>%
        group_by(.dots=grouping_cols) %>%
        summarize(Lower=quantile(Score, p=0.025, na.rm=TRUE),
                  Upper=quantile(Score, p=0.975, na.rm=TRUE),
                  Boot.mean=mean(Score)) %>%
        ungroup
    list(dist=x2, sum=x3)
}
