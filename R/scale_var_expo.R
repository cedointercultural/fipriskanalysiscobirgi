#' Scale variable
#'
#' @param datatable
#' @param sspscenario
#'
#' @return num.scale data with scaled total
#' @export
#'
#' @examples scaled.exp <- scale_var(exp.data.set.sc, NA)
scale_var_expo <- function(datatable, sspscenario, vars.expo, fiplocal.id){
  
  num.vars <- datatable %>% 
    dplyr::filter(CVE_LOC %in% fiplocal.id) 
  
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
  
  data.long <- data.res %>%
    tidyr::pivot_longer(cols=3:ncol(.), names_to="variable_id",values_to="value") 
  print(data.long)
  
  var.names <- unique(data.long$variable_id)
  
  if("Average_Chlorophyll" %in% var.names){
    
    data.long <- data.long %>% 
      dplyr::filter(!variable_id %in% c("Average_Chlorophyll"))
    
  }
  
 
  if("Average_Phytoplankton" %in% var.names){
    
    data.long <- data.long %>% 
      dplyr::filter(!variable_id %in% c("Average_Phytoplankton"))
    
  }
  
  if("tot_score" %in% var.names){
    
    data.long <- data.long %>% 
      dplyr::filter(!variable_id %in% c("tot_score"))
    
  }
  
  vars.set <- vars.expo %>% 
    dplyr::filter(scenario==sspscenario) %>% 
    dplyr::mutate(variable_id = var_nom) %>% 
    dplyr::select(variable_id, direction, scenario) 

  #higher value is negative  
  expo.dat <- data.long %>% 
    dplyr::left_join(vars.set, by="variable_id") %>% 
    dplyr::mutate(in_value=ifelse(direction=="higher_positive", 1-value, value)) %>%
    dplyr::select(-CVE_MUN,-direction, -scenario,-value) %>%
    tidyr::pivot_wider(names_from=variable_id, values_from=in_value) %>% 
    dplyr::mutate(scenario=sspscenario) %>% 
    dplyr::select(scenario, everything())
  
  print(expo.dat)
  return(expo.dat)
}



