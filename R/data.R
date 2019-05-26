#' @title Fabricated report card data
#' @docType data
#' @name reportcardData
#' @format A data frame with 9 variables:
#' \describe{
#' \item{\code{Date}}{Sample date}
#' \item{\code{Site}}{A lower level spatial unit nested within Zone}
#' \item{\code{Biomass}}{Seagrass Biomass}
#' \item{\code{Chl-a}}{Chlorophyl-a concentration}
#' \item{\code{Cover}}{Percent coral cover}
#' \item{\code{pH}}{pH}
#' \item{\code{Secchi}}{Secchi depth}
#' \item{\code{TN}}{Total nitrogen concentration}
#' \item{\code{TP}}{Total phosphorus concentration}
#' }
#' @seealso \code{\link{reportCard}}, \code{\link{indicator.hier}}, \code{\link{spatial.hier}}, \code{\link{guidelines}}
"reportcardData"

#' @title Fabricated indicator hierarchy
#' @docType data
#' @name indicator.hier
#' @format A data frame with 4 variables:
#' \describe{
#' \item{\code{Component}}{The highest level of the measure
#'     hierarchy}
#' \item{\code{Indicator}}{Indicator level}
#' \item{\code{Subindicator}}{Subindicator level}
#' \item{\code{Measure}}{Measure level.  The lowest level of the
#'     variable hierarchy}
#' }
#' @seealso \code{\link{reportcardData}}, \code{\link{spatial.hier}}, \code{\link{guidelines}}
"indicator.hier"

#' @title Fabricated spatial hierarchy
#' @docType data
#' @name spatial.hier
#' @format A data frame with 3 variables:
#' \describe{
#' \item{\code{Region}}{The top level of the spatial hierarchy}
#' \item{\code{Zone}}{a spatial unit nested within \code{Region}}
#' \item{\code{Site}}{the fixed locations at which the measurements
#'     were collected. These are nested within \code{Zone}}
#' }
#' @seealso \code{\link{reportcardData}}, \code{\link{indicator.hier}}, \code{\link{guidelines}}
"spatial.hier"


##' @title Fabricated guidelines data
##' @docType data
##' @name guidelines
##' @format A data frame with 11 variables:
##' \describe{
##' \item{\code{Zone}}{A major spatial unit}
##' \item{\code{Component}}{The highest level of the variable
##'     hierarchy}
##' \item{\code{Indicator}}{Indicator level}
##' \item{\code{Subindicator}}{Subindicator level}
##' \item{\code{Measure}}{Measure level.  The lowest level of the
##'     variable hierarchy}
##' \item{\code{GL}}{The guideline/threshold values}
##' \item{\code{Lower, Upper}}{Lower and upper bounds for guidelines that apply
##'     to a range (for example pH of Dissolved oxygen)}
##' \item{\code{DOF}}{Direction of Failure.  This will be either 'H'
##'     for higher (indicating that values higher than the guideline
##'     consistute an excedence of the guideline), 'L' for lower than
##'     the guideline or 'B' for a guideline range.}
##' \item{\code{Cuts}}{Break=Score pairs for defining the cuts in the
##'      data and corresponding scores.}
##' }
##' @seealso \code{\link{reportcardData}}
"guidelines"

##' @title Fabricated report card data and guidelines (combined)
##' @docType data
##' @name reportCard
##' @format A data frame with 19 variables:
##' \describe{
##' \item{\code{Date}}{Sample date}
##' \item{\code{Site}}{A lower level spatial unit nested within Zone}
##' \item{\code{Measure}}{The measured/observed variable}
##' \item{\code{Value}}{The observed value}
##' \item{\code{Month}}{The sampling month}
##' \item{\code{ReportYear}}{Reporting year base on an Austral
##'     Tropical Water year (1st Oct - 30th Sept)}
##' \item{\code{Region}}{The top level of the spatial hierarchy}
##' \item{\code{Zone}}{A spatial unit nested within \code{Region}}
##' \item{\code{Component}}{The highest level of the variable
##'     hierarchy}
##' \item{\code{Indicator}}{Indicator level}
##' \item{\code{Subindicator}}{Subindicator level}
##' \item{\code{GL}}{The guideline/threshold value}
##' \item{\code{Lower, Upper}}{Lower and upper bounds for guidelines that apply
##'     to a range (for example pH of Dissolved oxygen)}
##' \item{\code{lBound, uBound}}{Lower and upper capping and scaling bounds}
##' \item{\code{DOF}}{Direction of Failure.  This will be either 'H'
##'     for higher (indicating that values higher than the guideline
##'     consistute an excedence of the guideline), 'L' for lower than
##'     the guideline or 'B' for a guideline range.}
##' \item{\code{Cuts}}{Break=Score pairs for defining the cuts in the
##'     data and corresponding scores.}
##' }
##' @seealso \code{\link{reportcardData}} \code{\link{indicator.hier}}, \code{\link{spatial.hier}}, \code{\link{guidelines}}
"reportCard"



