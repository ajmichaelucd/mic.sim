#' loadRData
#'
#' Borrowed from https://stackoverflow.com/a/25455968 to import Rdata files and assign them to a variable name
#'
#' @param fileName
#'
#' @return
#' @export
#'
#' @examples
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
