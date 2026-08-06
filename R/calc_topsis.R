#' Topsis multicriteria
#'
#' @param eachscenario
#' @param risk.data
#' @param risk.data.hist
#'
#' @return
#' @export
#'
#' @examples
calc_topsis <- function(eachno, hazard.sc, risk.data, risk.data.hist){

  print(eachno)
  eachscenario <- hazard.sc[eachno]

  print(eachscenario)

  if(eachscenario=="historical"){

    topsis.data <- risk.data %>%
      dplyr::filter(scenario=="historical") %>%
     dplyr::select(-componente, -indicador_categoria, -eje, -scenario) %>%
      group_by(cooperativa, indicator) %>%
      dplyr::summarise(mean_value = mean(mean_value)) %>% #this summarises across species
      tidyr::pivot_wider(names_from = indicator, values_from = mean_value)%>%
      dplyr::ungroup()

  } else {


    topsis.data <- risk.data %>%
      dplyr::filter(scenario==eachscenario) %>%
      dplyr::select(-componente, -indicador_categoria, -eje, -scenario) %>%
      group_by(cooperativa, indicator) %>%
      dplyr::summarise(mean_value = mean(mean_value)) %>% #this summarises across species
      tidyr::pivot_wider(names_from = indicator, values_from = mean_value) %>%
      dplyr::left_join(risk.data.hist, by ="cooperativa") %>%
      dplyr::ungroup()

    }

  print(topsis.data)


  topsis.data.rev <-  topsis.data %>%
    dplyr::select(where(~ !any(is.na(.)))) %>%
    dplyr::select(-cooperativa)

  topsis.data.min <- topsis.data.rev[,!(colSums( topsis.data.rev) == 0)]
  topsis.data.max <- topsis.data.min[,!(colSums( topsis.data.min) == 6 )]


  #calculate topsis
  risk.matrix <- topsis.data.max %>%
     as.matrix()

  topsis.w <- rep(1,ncol(risk.matrix)) #
  topsis.i <- rep("-",ncol(risk.matrix))
  topsis.res <- topsis::topsis(risk.matrix, topsis.w, topsis.i)

  topsis.res.sc <- topsis.data %>%
    dplyr::select(cooperativa) %>%
    dplyr::bind_cols(topsis.res) %>%
    dplyr::mutate(escenario = eachscenario)

  return(topsis.res.sc)
  #d <- matrix(rpois(12, 5), nrow = 4)
  #w <- c(1, 1, 2)
  #i <- c("+", "-", "+")
  #topsis(d, w, i)

}
