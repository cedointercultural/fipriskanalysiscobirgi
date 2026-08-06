#' Read life history Google sheet
#'
#' @param sheetname name of sheet read
#' @param dataname name of data
#'
#' @return rds files for every sheet
#' @export
#'
#' @examples read_googlesheet(sheetlink, sheetname = "datos_caracteristicas")
read_googlesheet <- function(sheetlink, sheetname, coltypes){

 googlesheets4::gs4_deauth() #indicates no need to ask for credentials
 this.data <- googlesheets4::read_sheet(sheetlink, sheet = sheetname, col_types= coltypes)

 return(this.data)
 }
