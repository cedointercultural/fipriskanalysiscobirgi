#' @title PCA factor environmental sensitivity
#' @description  Normalize from 0-1
#' @details INPUT: 1) scaled data
#' @details OUTPUT: 1) pca 
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


pca_factor_expo <- function(expo.coop.data){
  
 
  data.set.wide <- expo.coop.data 
  print(head(data.set.wide))
  
  data.set.frame <- data.set.wide %>%
    dplyr::select(-nom_coop) %>% 
    as.data.frame
  
  data.set.id <- data.set.wide %>%
    dplyr::select(nom_coop)
  
  items.scale  <- ncol(data.set.frame) # number of items in the scale
  #obtain correlation matrix
  hozdatamatrix.impact <- cor(data.set.frame, use="complete.obs")
  #print correlation plot
  #pairs(data.set)
  
  # print out the correlation matrix but ask for numbers to 3 decimal places
  corrmat.impact  <- round(hozdatamatrix.impact,3)
  corrmat.impact
  # bartlett test - want a small p value here to indicate c0rrelation matrix not zeros
  psych::cortest.bartlett(data.set.frame)
  
  #principal components analysis
  
  # Determine Number of Factors to Extract
  
  #This code can be use to determine the optimal number of factors, however Jacob et al (2012)
  #recommends using a single factor solution
  # Generally, if the goal is simple and parsimonious description of a correlation
  # or covariance matrix, the first k principal components
  # will do a better job than any other k-dimensional solution
  ev <- eigen(hozdatamatrix.impact) # get eigenvalues
  ap <- nFactors::parallel(subject=nrow(data.set.frame),var=ncol(data.set.frame),
                           rep=100,cent=.05)
  try(nS <- nFactors::nScree(ev$values, ap$eigen$qevpea))
  #plotnScree(nS)
  
  #run single factor solution
  model1.impact<- psych::principal(data.set.frame, nfactors = 1, rotate = "none",scores=TRUE) #can specify rotate="varimax"
  model1.impact
  #SS loading is the eigenvalue
  # h2is called the communality estimate. Measures the % of variance
  # in an observed variable accounted for by the retained components
  
  #plot variables that continue most to the PCA
  res.pca <- prcomp(data.set.frame, scale = TRUE)
  
  var.cont <- factoextra::fviz_pca_var(res.pca,
                                       col.var = "contrib", # Color by contributions to the PC
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                       repel = TRUE     # Avoid text overlapping
  )
  
  ggplot2::ggsave(here::here("outputs","figures",paste0("varcont_pca_",this.scenario,".png")), var.cont, device="png", width = 8, height = 5)
  
  
  variable.plot <-  factoextra::fviz_cos2(res.pca, choice="var", axes = 1 )
  
  ggplot2::ggsave(here::here("outputs","figures","variables_pca_exposure.png"), variable.plot, device="png", width = 8, height = 5)
  
  
  # can find the reproduced correlations and the communalities (the diagonals)
  psych::factor.model(model1.impact$loadings)
  
  model.loadings <- unclass(model1.impact$loadings) %>%
    tidyr::as_tibble(rownames="var")
  #  dplyr::rename(loadings = PC1) 
  
  #  write_csv(model.loadings,here("outputs","analysis",paste0("model_loadings_",datasetname,".csv")))
  
  
  # the diagonals represent the uniqueness values (1- R squared):
  residuals.impact <- psych::factor.residuals(hozdatamatrix.impact, model1.impact$loadings)
  residuals.impact
  # nice to plot the residuals to check there are normally distributed
  #hist(residuals.impact)
  
  # to save the above values we need to add them to a dataframe
  factorscores.impact <- model1.impact$scores %>%
    tibble::as_tibble(rownames="coop") %>% 
    dplyr::bind_cols(data.set.id)
  
  g <- ggbiplot::ggbiplot(res.pca,
                          obs.scale = 1,
                          var.scale = 1,
                          groups = data.set.wide$nom_coop,
                          ellipse = TRUE,
                          circle = TRUE,
                          ellipse.prob = 0.68)
  g <- g + ggplot2::scale_color_discrete(name = '')
  g <- g + ggplot2::theme(legend.direction = 'horizontal',
                          legend.position = 'top')
  
  
  #obtain largest eigenvalue
  larg.eigen  <- max(model1.impact$values)
  #Armor's Theta tests for internal consistency of a factor scale
  Theta  <- (items.scale/(items.scale-1)) * (1-(1/larg.eigen))
  
  #shows the summary of the loadings table
  
  p <- print(model1.impact)
  
  model.summary <-  round(p$Vaccounted,2)  %>%
    tidyr::as_tibble(rownames = "Var") %>%
    dplyr::bind_rows(tidyr::tibble(Var = c("Eigenvalue","Theta"),PC1=c(larg.eigen, Theta))) %>%
    dplyr::filter(Var!="SS loadings")
  
  # write_csv(model.summary,here("outputs","analysis",paste0("model_summary_", datasetname,".csv")))
  
  
  
  this.pca <- factorscores.impact %>% 
    dplyr::group_by(nom_coop) %>% 
    dplyr::summarise(pc_val=mean(PC1)) 
  
  #scale pca  
  maxs <- max(this.pca$pc_val)
  mins <- min(this.pca$pc_val)
  
  
  scaled.pca <- scale(this.pca$pc_val, center = mins, scale = maxs - mins) %>% 
    as.data.frame() %>%
    dplyr::bind_cols(this.pca[,1:2]) %>% 
    dplyr::rename(norm_pca_expo = V1) 
  
  print(scaled.pca)
  
  readr::write_csv(scaled.pca,here::here("outputs","analysis",paste0("expo_factor_scores.csv")))
  
  
  return(scaled.pca)
  
  
}



  


  

 
 


