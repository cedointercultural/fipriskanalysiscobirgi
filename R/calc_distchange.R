
#' Calculate distance change in future distribution
#'
#' @param eachspecies 
#' @param sdm.species 
#' @param crs.eqap 
#' @param sdm.names 
#' @param sdm.files 
#' @param present.name 
#' @param sdm.scenarios 
#'
#' @return
#' @export
#'
#' @examples
calc_distchange <- function(eachspecies, sdm.species, crs.eqap, sdm.names, sdm.files, present.name, sdm.scenarios){

  this.species <- sdm.species[eachspecies]
  
  print(eachspecies)
  print(this.species)
  sp.name <- gsub(" ","_",this.species)
    
  test.names <- grepl(paste0(sp.name, "_avg.asc", collapse="|"), sdm.names) 
  
  index.test <- which(test.names, arr.ind = FALSE, useNames = TRUE)
  
  name.hist <- sdm.files[index.test]
  
  if(length(name.hist)>1) print("Mas de un raster historico")
    
  #read historical raster
  raster.present <- terra::rast(here::here("data-raw","sdm",name.hist))
  
  raster.present.proj <- terra::project(raster.present, crs.eqap)
  
  dist.list <- list()
  
  length.scenarios <- 1:length(sdm.scenarios)
  
  for(eachsc in length.scenarios){
 
    eachfuturesc <- sdm.scenarios[eachsc]
    print(eachfuturesc)
    
    future.names <- grepl(paste0(sp.name,"_",eachfuturesc,".*"), sdm.names)
    
    index.future <- which(future.names, arr.ind = FALSE, useNames = TRUE)
    
    name.sc <- sdm.files[index.future]
    
    print(name.sc)
    if(length(name.sc)>1) print(paste0("Mas de un raster futuro ", sp.name))
    
    raster.future <- terra::rast(here::here("data-raw","sdm", name.sc))
    raster::plot(raster.future)
    raster.future.proj <- terra::project(raster.future, crs.eqap)
    
    delta <- raster.future.proj - raster.present.proj
  #  terra::plot(delta, main = 'Change in Suitability')
    
    ## calculate biotic velocity
    #############################
    #https://github.com/adamlilith/enmSdmX/blob/master/man/examples/bioticVelocity_examples.r
    
    series <- c(raster.present.proj, raster.future.proj)
    
    
    names(series) <- c('present', eachfuturesc)
  #  terra::plot(series)
    
    times <- c(2022, 2050)
    quants <- c(0.10, 0.90)
    
    bv <- enmSdmX::bioticVelocity(
      x = series,
      times = times,
      quants = quants,
      cores = 2
    )
    
    dist.change <- tidyr::tibble(species = this.species, scenario=eachfuturesc,
                                 centroid_velocity = bv$centroidVelocity, # positive value shows Northward shift
                                 suitability = bv$simpleMeanDiff, # average change in suitability from one time period to next
                                 rmsq = bv$rmsd, # root-mean square difference from one time period to the next
                                 godsoeESp = 1 - bv$godsoeEsp, # similarity
                                 schoenerD = 1 - bv$schoenerD, # similarity
                                 warrenI = 1 - bv$warrenI, # similarity
                                 cor = 1 - bv$cor, #similarity
                                 rankCor = 1 - bv$rankCor) #similarity
  
    print(dist.change)
    
    dist.list[[eachsc]] <- dist.change  
  }
    
  dist.res <- dplyr::bind_rows(dist.list)
  
  return(dist.res)

  }




