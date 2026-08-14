#' @title Normalize function
#' @description  Normalize from 0-1
#' @details INPUT: 1) data
#' @details OUTPUT: 1) normalized data
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


pca_factor <- function(thisdata, datasetname, namesdata, index){

  #standarize by index

  #test for na values

  data.set.nan <-  thisdata %>%
    tidyr::pivot_longer(cols = 2:ncol(.), names_to="index", values_to = "value") %>%
    dplyr::filter(is.nan(value) | is.na(value))

  print("index values that equal NA or NaN because all values are the same or all 0s, these indices will be eliminated")
  print(unique(data.set.nan$index))

  data.set <-  thisdata %>%
    tidyr::pivot_longer(cols = 2:ncol(.), names_to="index", values_to = "value") %>%
    dplyr::filter(!is.nan(value)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(index, by="index") %>%
    dplyr::mutate(value=dplyr::if_else(direction=="alto_negativo", 1-value, value)) %>%
    dplyr::select(-direction) %>%
    tidyr::pivot_wider(id_cols = name, names_from = index) %>%
    dplyr::select(-name)

 items.scale  <- ncol(data.set)
  #obtain correlation matrix
  hozdatamatrix.impact <- cor(data.set, use="complete.obs")
  #print correlation plot
  #pairs(data.set)

  # print out the correlation matrix but ask for numbers to 3 decimal places
  corrmat.impact  <- round(hozdatamatrix.impact,3)
  corrmat.impact
  # bartlett test - want a small p value here to indicate c0rrelation matrix not zeros
  psych::cortest.bartlett(data.set)

  #principal components analysis

  # Determine Number of Factors to Extract

  #This code can be use to determine the optimal number of factors, however Jacob et al (2012)
  #recommends using a single factor solution
  # Generally, if the goal is simple and parsimonious description of a correlation
  # or covariance matrix, the first k principal components
  # will do a better job than any other k-dimensional solution
  ev <- eigen(hozdatamatrix.impact) # get eigenvalues
  ap <- nFactors::parallel(subject=nrow(data.set),var=ncol(data.set),
                 rep=100,cent=.05)
  try(nS <- nFactors::nScree(ev$values, ap$eigen$qevpea))
  #plotnScree(nS)

  #run single factor solution
  model1.impact<- psych::principal(data.set, nfactors = 1, rotate = "none",scores=TRUE) #can specify rotate="varimax"
  model1.impact
  #SS loading is the eigenvalue
  # h2is called the communality estimate. Measures the % of variance
  # in an observed variable accounted for by the retained components

  # prcomp library factoextra
  #plot variables that continue most to the PCA
  data.set.named <- data.set %>%
    as.matrix()

  rownames(data.set.named) <- namesdata$name

  res.pca <- stats::prcomp(data.set.named, scale = TRUE)

  factoextra::fviz_pca_var(res.pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )

  variable.plot <-  factoextra::fviz_cos2(res.pca, choice="var", axes = 1 )

  ggplot2::ggsave(here::here("outputs","figures",paste0("variables_pca_",datasetname,".png")), variable.plot, device="png", width = 8, height = 5)

  # Biplot: Shows both variables and observations together
  # Extract scores and loadings for custom biplot
  scores <- as.data.frame(res.pca$x[, 1:2])
  scores$name <- rownames(res.pca$x)

  loadings <- as.data.frame(res.pca$rotation[, 1:2] * sqrt(res.pca$sdev[1:2]))
  loadings$var <- rownames(loadings)

  # Create biplot
  biplot <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = scores,
      ggplot2::aes(x = PC1, y = PC2),
      color = "#1565C0",
      size = 4,
      alpha = 0.7
    ) +
    ggrepel::geom_text_repel(
      data = scores,
      ggplot2::aes(x = PC1, y = PC2, label = name),
      size = 3,
      color = "#1565C0",
      fontface = "bold",
      segment.color = NA,
      max.overlaps = Inf
    ) +
    ggplot2::geom_segment(
      data = loadings,
      ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")),
      color = "#2E7D32",
      linewidth = 0.6,
      alpha = 0.8
    ) +
    ggrepel::geom_text_repel(
      data = loadings,
      ggplot2::aes(x = PC1, y = PC2, label = var),
      size = 3.5,
      color = "#2E7D32",
      fontface = "bold",
      segment.color = NA
    ) +
    ggplot2::labs(
      title = "",
      x = paste0("PC1 (", round(summary(res.pca)$importance[2, 1] * 100, 1), "%)"),
      y = paste0("PC2 (", round(summary(res.pca)$importance[2, 2] * 100, 1), "%)")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  ggplot2::ggsave(here::here("outputs","figures",paste0("biplot_pca_",datasetname,".png")), biplot, device="png", width = 10, height = 8)

  # Heatmap: Shows row-variable interaction in scaled data
  heatmap.data <- thisdata %>%
    tidyr::pivot_longer(cols = 2:ncol(.), names_to="variable", values_to = "value")

  # Calculate order for rows and columns
  name_order <- heatmap.data %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
    dplyr::arrange(mean_value) %>%
    dplyr::pull(name)

  var_order <- heatmap.data %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
    dplyr::arrange(mean_value) %>%
    dplyr::pull(variable)

  heatmap.plot <- heatmap.data %>%
    dplyr::mutate(name = factor(name, levels = name_order)) %>%
    dplyr::mutate(variable = factor(variable, levels = var_order)) %>%
    ggplot2::ggplot(ggplot2::aes(x = variable, y = name, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "#F7F7F7",
      high = "#B35806",
      midpoint = mean(heatmap.data$value, na.rm = TRUE),
      name = "Scaled Value"
    ) +
    ggplot2::labs(
      title = paste("Variable Contributions -", datasetname),
      x = "Variables",
      y = "Observations/Communities"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 9)
    )

  ggplot2::ggsave(here::here("outputs","figures",paste0("heatmap_pca_",datasetname,".png")), heatmap.plot, device="png", width = 8, height = 6)


  # can find the reproduced correlations and the communalities (the diagonals)
  psych::factor.model(model1.impact$loadings)

  model.loadings <- unclass(model1.impact$loadings) %>%
    tidyr::as_tibble(rownames="var") %>%
    dplyr::rename(loadings = PC1) %>%
    dplyr::mutate(data_name = datasetname)

  readr::write_csv(model.loadings,here::here("outputs","analysis",paste0("model_loadings_",datasetname,".csv")))


  # the diagonals represent the uniqueness values (1- R squared):
  residuals.impact <- psych::factor.residuals(hozdatamatrix.impact, model1.impact$loadings)
  residuals.impact
  # nice to plot the residuals to check there are normally distributed
  #hist(residuals.impact)

  # to save the above values we need to add them to a dataframe
  factorscores.impact <- model1.impact$scores %>%
    tidyr::as_tibble() %>%
    dplyr::bind_cols(namesdata, .) %>%
    dplyr::arrange(dplyr::desc(PC1)) %>%
    dplyr::mutate(component=datasetname)

  readr::write_csv(factorscores.impact,here::here("outputs","analysis",paste0("factor_scores_",datasetname,".csv")))

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

  readr::write_csv(model.summary,here::here("outputs","analysis",paste0("model_summary_", datasetname,".csv")))

  return(factorscores.impact)
}

