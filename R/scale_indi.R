#' Scale variable
#'
#' @param datatable
#' @param sspscenario
#'
#' @return num.scale data with scaled total
#' @export
#'
#' @examples
scale_indi <- function(datatable, loc.fip.expo){

data.loc  <- loc.fip.expo %>%
    dplyr::left_join(datatable, by=c("COM_ID")) %>%
    dplyr::filter(!is.na(NOM_ENT)) %>%
    dplyr::select(-NOM_ENT, -NOM_MUN, -NOM_LOC) %>%
    dplyr::select(COM_ID, escenario, fip_name, fip_id_number, matches("."))

data.loc.wide <- data.loc %>%
    tidyr::pivot_longer(cols=5:ncol(data.loc), names_to="indicator",values_to="value") %>%
    dplyr::group_by(fip_name, fip_id_number, escenario, indicator) %>%
    dplyr::summarise(fip_value = mean(value)) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = fip_value) %>%
    dplyr::ungroup()

  process.num <- data.loc.wide %>%
    dplyr::select(-fip_id_number) %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    caret::preProcess(., method=c("range"))

  num.scale <- data.loc.wide %>%
    dplyr::select(-fip_id_number) %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    stats::predict(process.num, .)

  process.tot <- num.scale %>%
  dplyr::mutate(tot_score = rowSums(dplyr::select_if(., is.numeric), na.rm = TRUE)) %>%
  dplyr::select(tot_score) %>%
    caret::preProcess(., method=c("range"))

  num.tot <- num.scale %>%
    dplyr::mutate(tot_score = rowSums(dplyr::select_if(., is.numeric), na.rm = TRUE)) %>%
    dplyr::select(tot_score) %>%
    stats::predict(process.tot, .) %>%
    dplyr::bind_cols(num.scale, .)

  data.res <- data.loc.wide %>%
    dplyr::select(dplyr::where(is.character)) %>%
    dplyr::bind_cols(., num.tot)

   return(data.res)
}



