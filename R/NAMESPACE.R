#' The 'mpiBpred' package.
#'
#' @description A DESCRIPTION OF THE PACKAGE
#'
#' @docType package
#' @name mpiBpred
#' @aliases mpiBpred
#' @import shiny
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom dplyr bind_cols bind_rows filter group_by if_else mutate summarise ungroup
#' @importFrom ggplot2  aes aes_ aes_string geom_boxplot geom_density geom_errorbar geom_histogram ggplot geom_point ggtitle ylab xlab geom_line geom_ribbon labs theme element_text
#' @importFrom graphics lines
#' @importFrom jsonlite toJSON
#' @importFrom htmltools save_html
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom modules module
#' @importFrom rsync rsync getData sendObject listFiles
#' @importFrom shinyjs alert reset runjs useShinyjs
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom stats density median na.omit quantile rgamma rlnorm rnorm sd optim runif
#' @importFrom utils capture.output combn write.table
#' @importFrom shinyjs alert
#' @importFrom xlsx write.xlsx
#'
globalVariables(c("model", "dataObj", "formulasObj", "inputObj"))
NULL

#' Server and UI Functions for Shiny Module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param id namespace id
#' @param title title of tab in tabset panel
#'
#' @name shinyModule
NULL
