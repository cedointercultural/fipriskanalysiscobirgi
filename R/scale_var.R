#' Scale variable
#'
#' @param datatable
#' @param sspscenario
#'
#' @return num.scale data with scaled total
#' @export
#'
#' @examples scaled.exp <- scale_var(exp.data.set.sc, NA)
scale_var <- function(datatable, sspscenario){

if (is.character(sspscenario)){

  num.vars <- datatable %>%
    dplyr::filter(scenario==sspscenario)

} else if(is.na(sspscenario)){

  num.vars <- datatable

}
  
  process.num <- num.vars %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    caret::preProcess(., method=c("range"))
  
  num.scale <- num.vars %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    stats::predict(process.num, .)
  
  process.tot <- num.vars %>%
    dplyr::mutate(tot_score = rowSums(dplyr::select_if(., is.numeric), na.rm = TRUE)) %>%
    dplyr::select(tot_score) %>%
    caret::preProcess(., method=c("range"))
  
  num.tot <- num.scale %>%
    dplyr::mutate(tot_score = rowSums(dplyr::select_if(., is.numeric), na.rm = TRUE)) %>%
    dplyr::select(tot_score) %>%
    stats::predict(process.tot, .) %>%
    dplyr::bind_cols(num.scale, .)
  
  data.res <- num.vars %>%
    dplyr::select(dplyr::where(is.character)) %>%
    dplyr::bind_cols(., num.tot)
  
   return(data.res)
}



