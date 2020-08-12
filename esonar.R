#' eSonar Data
#'
#' @description Functions to read, manipulate and analyze eSonar trawl acoustic monitoring  probe data.
#'
#' @param x eSonar data file
#' @param year Numeric value specifying the survey or project year(s).
#' @param survey,project Character string specifying the survey or project name.
#'    The survey may refer to the September multi-species survey (\code{survey = "rv"},
#'    \code{survey = "sep"} or \code{survey = "September"}), the  Northumberland Strait
#'    survey (\code{= "ns"}), the mobile Sentinel survey  \code{= "sentinel"}),
#'    or the snow crab survey (\code{= "sc"} or \code{= "snow crab"}).
#' @param set.number,tow.id Numeric value or strings specifying the survey set number or tow ID.
#' @param set.card Data frame containing the survey tows which specify the Minilog data to be loaded.
#' @param path Logical value specifying whether to include the path in the Minilog files.
#'
#' @examples
#' # eSonar files for the 2000 September RV survey:
#' esonar.file(survey = "rv", year = 2000)
#'
#' # Use a tow ID to extract file names for the snow crab survey 2006-2012:
#' esonar.file("GP001", year = 2006:2012)
#'
#' # Use a set card extract for the first ten sets of 2012:
#' x <- read.gulf(year = 2012, survey = "sc")
#' minilog.file(x[1:10, ])
#'

#'
#' x <- read.minilog(tow.id = "GP001F", year = 2010)
#' x <- read.minilog(survey = "rv", year = 2010)
#'
#' # Plot Minilog data:
#' file <- system.file("extdata", "Minilog example.txt", package = "gulf.data")
#' x <- read.minilog(file)
#' plot(x)
#'
#' @export minilog
#'
#' @seealso \code{\link[gulf.data]{read.minilog}}
#' @seealso \code{\link[gulf.data]{header}}
#'

esonar <- function(x, ...){
   # ESONAR - Generic 'esonar' method.
   UseMethod("esonar")
}
