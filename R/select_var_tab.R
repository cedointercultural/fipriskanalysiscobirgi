#' Select variables
#'
#' @param datatable
#' @param sspscenario
#'
#' @return num.scale data with scaled total
#' @export
#'
#' @examples
select_var_tab <- function(namesdata, datatable, scenario, id.cols, expohist.bc){

  if(scenario=="historico"){

    selec.data <- datatable %>%
      dplyr::select(tidyr::any_of(namesdata)) %>%
      dplyr::select(dplyr::where(is.numeric)) %>%
      dplyr::mutate(escenario = scenario) %>%
      dplyr::bind_cols(id.cols,.)


  } else if(grepl("ssp",scenario)){

    selec.data <- datatable %>%
      dplyr::select(tidyr::any_of(namesdata)) %>%
      dplyr::select(dplyr::where(is.numeric)) %>%
      dplyr::mutate(escenario = scenario) %>%
      dplyr::bind_cols(id.cols,.)


  }

  return(selec.data)
}

