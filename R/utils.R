#' @title Determine Austral tropical Season (Wet/Dry)
#' @description This function uses a vector of valid \code{Dates} to
#'     create a new vector that indicates the corresponding Austral
#'     Tropical Season (Wet or Dry).
#' @param Date a vector of Dates
#' @return a vector of strings indicating either 'Wet' or 'Dry'
#' @details
#'     The Austral 'Wet' Season is defined as the months of November,
#'     December, January, February, March and April.  The Austral
#'     'Dry' Season is defined as the months of May, June, July,
#'     August, September and October.
#' @export
#' @seealso \code{\link{RC_waterYear}}
#' @examples
#' RC_makeWetDrySeason(.leap.seconds)
RC_makeWetDrySeason <- function(Date) {
    if ("POSIXct" %in% class(Date)) Date <- as.Date(Date)
    ifelse(format(Date, "%b") %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'Dry','Wet')
}

#' @title Determine the Austral tropical water year
#' @description This function uses a vector of valid \code{Dates} to
#'     create a new vector that indicates the corresponding Austral
#'     Tropical water year
#' @param Date a vector of Dates
#' @details
#'     The Austral Tropical water year is defined as between October
#'     1st and September 30.
#' @return a vector of integers indicating the water year
#' @export
#' @seealso \code{\link{RC_financialYear}}
#' @examples
#' RC_waterYear(.leap.seconds)
RC_waterYear <- function(Date) {
    as.numeric(as.character(format(Date+(as.Date("1970-12-31")-as.Date("1970-10-01")+1), format="%Y")))
}


#' @title Determine financial year
#' @description This function uses a vector of valid \code{Dates} to
#'     create a new vector that indicates the corresponding financial
#'     year. 
#' @param Date a vector of Dates
#' @return a vector of integers indicating the financial year
#' @export
#' @seealso \code{\link{RC_waterYear}}
#' @examples
#' RC_financialYear(.leap.seconds)
RC_financialYear <- function(Date) {
    as.numeric(as.character(format(Date+(as.Date("1970-12-31")-as.Date("1970-07-01")+1), format="%Y")))
}

##' @title Convert cut string into matrix
##' @description Convert a string of cuts (Break=Score pairs separated
##'     by ':') into a matrix in which each row represents a
##'     Break/Score pair 
##' @param x a string containing the ':' separated Break=Score cut pairs
##' @return a matrix of Breaks and corresponding Scores
##' @author Murray Logan
RC_cuts_matrix <- function(x) {
    t(sapply(strsplit(unlist(strsplit(x, ':')), '='), 'as.numeric'))
}

##' @title Extract the Break associated with the minimum Score
##' @description Extract the Break associated with the minimum Score
##'     from a sting of ':' separated Break=Score cut pairs
##' @param x a string containing the ':' separated Break=Score cut pairs
##' @return a numeric representing the Break associated with the
##'     minimum Score
##' @author Murray Logan
##' @export
RC_min_cut <- function(x) {
    x = RC_cuts_matrix(x)
    x[max(which(x[,2]==min(x[,2]))),1]
}

##' @title Extract the Break associated with the maximum Score
##' @description Extract the Break associated with the maximum Score
##'     from a sting of ':' separated Break=Score cut pairs
##' @param x a string containing the ':' separated Break=Score cut pairs
##' @return a numeric representing the Break associated with the
##'     maximum Score
##' @author Murray Logan
##' @export
RC_max_cut <- function(x) {
    x = RC_cuts_matrix(x)
    x[min(which(x[,2]==max(x[,2]))),1]
}


##' @title Report Card Traffic light colours
##' @description A vector of hex colour codes associated with the
##'     report card traffic light colour scheme
##' @return a vector of hex codes
##' @author Murray Logan
##' @export
RC_reportCardColors = c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24')

##' @title Report Card Traffic light colour palette
##' @description A colour palette associated with the
##'     report card traffic light colour scheme
##' @return a color palette
##' @author Murray Logan
##' @export
RC_reportCardColorPalette = colorRampPalette(RC_reportCardColors)
