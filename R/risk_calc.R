#' @title Estimate risk
#' @description  Estimate risk by scenario
#' @details INPUT: 1) scaled hazard, exposure, vulnerability
#' @details OUTPUT: 1) risk 
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com

risk_calc <- function(thisscenario, hazpca, vulpca, expopca,coops, sc.list){
  
  print(thisscenario)
  eachscenario <- sc.list[thisscenario]
  print(eachscenario)
  
  this.haz <- hazpca %>% 
    dplyr::filter(scenario==eachscenario) 
  
  risk.axes <- this.haz %>% 
    dplyr::left_join(vulpca, by=c("nom_coop")) %>%
    dplyr::left_join(expopca, by=c("nom_coop")) %>% 
    dplyr::select(-pc_val.x, -pc_val.y) %>% 
    dplyr::mutate(tot_risk = norm_pca_expo + norm_pca_vul + norm_pca_haz)
  
  process.coop <- risk.axes %>% 
    dplyr::select(tot_risk) %>%
    caret::preProcess(., method=c("range"))
  
  coops.frame <- coops %>% 
    dplyr::distinct(nom_coop, cooperativa, entidad)
  
  
  risk.data.set.coop <- risk.axes %>% 
    dplyr::select(tot_risk) %>%
    stats::predict(process.coop, .) %>% 
    dplyr::bind_cols(nom_coop=risk.axes$nom_coop) %>% 
    dplyr::left_join(coops.frame, by = "nom_coop") %>% 
    dplyr::mutate(scenario = eachscenario)
  
  return(risk.data.set.coop)
  
  
}
