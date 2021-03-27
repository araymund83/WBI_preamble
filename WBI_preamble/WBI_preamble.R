## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "WBI_preamble",
  description = paste("this module prepares the following objects: ",
                      "1. study areas and corresponding rasterToMatch(as well as large versions)",
                      "2. species equivalencies tables and the sppEquiv column",
                      "3. vegetation map, biomassMap, standAge Map, firePoints, flammable map",
                      "rawBiomassMap,vegMap, and ecoregion map."),
  keywords = "",
  authors = c(
    person("Alex M", "Chubaty", email= "achubaty@for-cast.ca", role = "aut"),
    person("Ana", "Raymundo", email= "angeles-ana-paula.raymundo-sanchez.1@ulaval.ca", role = "aut")
    ),
  version = list(SpaDES.core = "1.0.6.9020", WBI_preamble = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "WBI_preamble.Rmd")),
  reqdPkgs = list("magrittr", "raster", "sf", "sp",
                  "PredictiveEcology/reproducible@development (>= 1.2.6.9008)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9014)",
                  "PredictiveEcology/LandR@development"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    paste("Distance (m) to buffer studyArea and rasterToMatch when",
                          "creating large versions.")),
        defineParameter("studyAreaName", "character", "BC", NA, NA,
                    paste("study area name for WB project, options are: BC, AB",
                          "SK,YK, NWT,MB"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(firePoints, objectClass = "RasterLayer",
                  desc = "fire points for XXXXX"), 
#TODO: DESCRIPTION 
    createsOutput(rasterToMatch, objectClass = "RasterLayer",
                  desc = "template raster"),
    createsOutput(rasterToMatchLarge, objectClass = "RasterLayer",
                  desc = "template raster for larger area"),
    createsOutput(rasterToMatchReporting, objectClass = "RasterLayer",
                  desc = "template raster for reporting area"),
    createsOutput(sppColorVect, objectClass = "character",
                  desc = "species colours for plotting"),
    createsOutput(sppEquiv, objectClass = "character",
                  desc = "table of LandR species names equivalencies"),
    createsOutput(sppEquivCol, objectClass = "character",
                  desc = "name of column to use in sppEquiv"),
    createsOutput(standAgeMap2011, objectClass = "RasterLayer",
                  desc = "time since disturbance raster for year 2011"),
    createsOutput(studyArea, objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for simulation (buffered to mitigage edge effects"),
    createsOutput(studyAreaLarge, objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for module parametrization(buffered)"),
    createsOutput(studyAreaReporting, objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for reporting")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.WBI_preamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "WBI_preamble", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "WBI_preamble", "save")
    },
      warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  dPath <- file.path("modules", currentModule(sim), "data")
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))
  
  #### Prep study-area specific objects ####
  ## when adding study areas, add relevant climate urls, rtm and sa, and don't forget R script prepSppEquiv
  allowedStudyAreas <- c("AB", "BC", "MB", "NT", "NU", "SK", "YT") ## prov/terr x BCR intersections
                        
  provs <- c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")
  terrs <- c("Yukon", "Northwest Territories", "Nunavut")
  WB <- c(provs, terrs)
  
  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"
  
  bcrshp <- Cache(prepInputs,
                  url = bcrzip,
                  destinationPath = dPath,
                  targetCRS = targetCRS,
                  useCache = P(sim)$.useCache,
                  fun = "sf::st_read")

  canProvs <- Cache(prepInputs,
                    "GADM",
                    fun = "base::readRDS",
                    dlFun = "raster::getData",
                    country = "CAN", level = 1, path = paths1$inputPath,
                    #targetCRS = targetCRS, ## TODO: fails on Windows
                    targetFile = "gadm36_CAN_1_sp.rds",
                    cacheRepo = paths1$cachePath,
                    destinationPath = paths1$inputPath
  )%>% 
    st_as_sf(.) %>%
    st_transform (., targetCRS)
  
  #################################################################################
  ## BCR for Western Boreal
  ################################################################################# 
  
  bcrWB <- bcrshp[bcrshp$BCR %in% c(4, 6:8), ]
  provsWB <- canProvs[canProvs$NAME_1 %in% WB, ]
  
  WBstudyArea <- Cache(postProcess, 
                       provsWB, 
                       studyArea = bcrWB, 
                       useSAcrs = TRUE,
                       cacheRepo = paths1$cachePath,
                       filename2 = NULL) %>%
    as_Spatial(.)

  
  #################################################################################
  ## BCR6 subdivision
  #################################################################################
  AB <- c("Alberta")
  BC <- c("British Columbia")
  SK <- c("Saskatchewan")
  MB <- c("Manitoba")
  NWT <- C("Northwest Territories")
  
  bcr6 <- bcr_sf[bcr_sf$BCR %in% c(6), ]
  
  provsBCR6 <- canProvs[canProvs$NAME_1 %in% provBCR6, ]
  AB <- canProvs[canProvs$NAME_1 %in% AB, ]
  BC <- canProvs[canProvs$NAME_1 %in% BC, ]
  SK <- canProvs[canProvs$NAME_1 %in% SK,]
  MB <- canProvs[canProvs$NAME_1 %in% MB,]
  NWT <- canProvs[canProvs$NAME_1 %in% NWT, ]
  
  bcr6SA <- reproducible::Cache(postProcess,
                                provsBCR6,
                                studyArea = bcr6,
                                useSAcrs = TRUE,
                                cacheRepo = asPath(Paths$cachePath),
                                destinationPath = asPath(Paths$inputPath),
                                filename2 = "bcr6_studyArea")
  
  ## in order to be able to rasterize, we need to create a numeric column to ID each of the provinces
  ## for BCR6
  bcr6SA$ID <- as.numeric(as.factor(bcr6SA$NAME_1))
  
  ## In addition, this object has problems when rasterize, that is why geometry is being
  ## homogenize by using st_cast
  #bcr6SA <- st_cast(bcr6SA, "MULTIPOLYGON") %>% as_Spatial(bcr6SA) ## TODO:
if (grepl("bcr6_AB", P(sim)$studyAreaName)){
    ## BCR6 Alberta 
  bcr6AB <- reproducible::Cache(postProcess,
                                  AB,
                                  studyArea = bcr6SA,
                                  useSAcrs =  TRUE,
                                  filename2 = NULL,
                                  cacheRepo = Paths$cachePath)
  sim$studyArea <- bcr6AB
} else if (grepl("bcr6BC", P(sim)$studyAreaName)){
  ## BCR6 British Columbia
  bcr6BC <- reproducible::Cache(postProcess,
                                  BC,
                                  studyArea = bcr6SA,
                                  useSAcrs =  TRUE,
                                  filename2 = NULL,
                                  cacheRepo = Paths$cachePath)
  sim$studyArea <- bcr6BC
} else if (grepl("bcr6NWT", P(sim)$studyAreaName)){
  ## BCR6 North West Territories
  bcr6NWT <- reproducible::Cache(postProcess,
                                 NWT,
                                 studyArea = bcr6SA,
                                 useSAcrs = TRUE,
                                 filename2 = NULL,
                                 cacheRepo = Paths$cachePath)
  sim$studyArea <- bcr6NWT
  
}else if (grepl("bcr6SK", P(sim)$studyAreaName)){
  bcr6SK <- reproducible::Cache(postProcess,
                                SK,
                                studyArea = studyArea,
                                filename2 = NULL,
                                cacheRepo = Paths$cachePath)

  sim$studyArea <- bcr6SK
  
}else if (grepl("bcr6MB", P(sim)$studyAreaName)){
  bcr6MB <- reproducible::Cache(postProcess,
                                MB,
                                studyArea = studyArea,
                                filename2 = NULL,
                                cacheRepo = Paths$cachePath)

  sim$studyArea <- bcr6MB
}
  sim$studyArea <- spTransform(sim$studyArea, targetCRS)
  sim$studyArea$studyAreaName <- P(sim)$studyAreaName
  sim$studyAreaReporting <- sim$studyArea
  sim$studyArea <- buffer(sim$studyArea, P(sim)$bufferDist)
  sim$studyAreaReporting <- sim$studyArea
  
  #################################################################################
  ## LCC 2005
  #################################################################################
  sim$rasterToMatch <- reproducible::Cache(LandR::prepInputsLCC,
                                           destinationPath = asPath(Paths$inputPath),
                                           studyArea = sim$studyArea,
                                           year = 2005,
                                           filename2 = paste0(P(sim)$studyAreaName, "_rtm.tif"))
  sim$rasterToMatchLarge <- sim$rasterToMatch

  
   ## saving shapefiles
  st_write(bcr6SA, file.path(Paths$inputPath, "BCR6.shp"), overwrite = TRUE)
  st_write(bcr6ABBC, file.path(Paths$inputPath, "BCR6_ABBC.shp"), overwrite = TRUE)
  st_write(bcr6SKMB, file.path(Paths$inputPath, "BCR6_SKMB.shp"), overwrite = TRUE)
  st_write(bcr6NWT, file.path(Paths$inputPath, "BCR6_NWT.shp"), overwrite = TRUE)
  

  #################################################################################
  ## Age
  #################################################################################
  standAgeMapURL <- paste0(
    "ftp://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
    "canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/",
    "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"
  )
  
  
  sim$standAgeMap2011 <- Cache(LandR::prepInputsStandAgeMap,
                           destinationPath = asPath(Paths$inputPath),
                           ageUrl = standAgeMapURL,
                           ageFun = "raster::raster",
                           studyArea = studyAreaLarge,
                           rasterToMatch = rasterToMatchLarge,
                           # maskWithRTM = TRUE,
                           method = "bilinear",
                           useCache = TRUE,
                           datatype = "INT2U",
                           filename2 = "standAgeMap.tif",
                           startTime = 2011)
  
  
  #################################################################################
  ## Wetlands
  #################################################################################
  # wetlandzip <- "https://drive.google.com/file/d/1R1AkkD06E-x36cCHWL4U5450mSDu_vD0/view?usp=sharing"
  # wetlandWB <- Cache(prepInputs,
  #                 url = wetlandzip,
  #                 destinationPath = getPaths()$inputPath,
  #                 studyArea = studyArea,
  #                 destinationPath = Paths$inputPath,
  #                 rasterToMatch = LCC05Ras,
  #                 targetFile = "CA_wetlands_post2000.tif",
  #                 userTags = c("wetlandWB")
  #                 )
  # wetland6 <-  reproducible::Cache(postProcess,
  #                                  wetlandWB,
  #                                  studyArea = bcr6SA,
  #                                  useSAcrs = TRUE)
  
  sim$flammableMap <- LandR::defineFlammable(LandCoverClassifiedMap = rstLCC,
                                         nonFlammClasses = c(33, 36:39),
                                         mask = rasterToMatchLarge)
  
  firePointsURL <- "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip"
  sim$firePoints <- Cache(fireSenseUtils::getFirePoints_NFDB,
                      url = firePointsURL,
                      studyArea = studyAreaLarge,
                      rasterToMatch = rasterToMatchLarge,
                      redownloadIn = 1,
                      years = 1991:2017, ## TODO: @araymund83 these are default years; do you want others?
                      fireSizeColName = "SIZE_HA",
                      NFDB_pointPath = asPath(Paths$inputPath)) %>%
    st_as_sf(.)
  
  biomassMapURL <- paste0(
    "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
    "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/",
    "NFI_MODIS250m_2001_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif"
  )
  
  with_config(config = config(ssl_verifypeer = 0L), {
    sim$rawbiomassMap2001 <- Cache(prepInputs,
                               destinationPath = asPath(Paths$inputPath),
                               url = biomassMapURL,
                               fun = "raster::raster",
                               studyArea = studyAreaLarge,
                               rasterToMatch = rasterToMatchLarge,
                               maskWithRTM = TRUE,
                               method = "bilinear",
                               datatype = "INT2U",
                               filename2 = "rawBiomassMap2001")
  })
  
  sim$vegMap <- Cache(LandR::prepInputsLCC,
                  year = 2005,
                  destinationPath = asPath(Paths$inputPath),
                  studyArea = studyAreaLarge,
                  rasterToMatch = rasterToMatchLarge,
                  filename2 = "vegMap.tif"
  )
  
  ecoregionsURL <- paste0("https://drive.google.com/file/d/",
                          "1y2-mM7NBxEvdeoDENkwbQDa-rdQjiutx/view?usp=sharing")
  sim$ecoregionsMap <- Cache(prepInputs,
                         destinationPath = Paths$inputPath,
                         url = ecoregionsURL,
                         fun = "read_sf",
                         studyArea = studyAreaLarge,
                         rasterToMatch = rasterToMatchLarge,
                         maskWithRTM = TRUE,
                         method = "bilinear",
                         datatype = "INT2U",
                         filename2 = "ecoregionsMap"
  )
  
##all species considered in WB (will be subset later for each study area)
  data("sppEquivalencies_CA", package = "LandR")
  sppEquiv <- sppEquivalencies_CA
  
  allWBspp <- c("Abie_bal", "Abie_las", "Betu_pap", "Lari_lar",
                "Pice_eng", "Pice_gla", "Pice_mar", "Pinu_ban",
                "Pinu_con", "Popu_tre")
  sppEquiv <- sppEquivalencies_CA[LandR %in% allWBspp]
  SASppToUse <- data.table::data.table(
    LandR = sppEquiv[, LandR],
    BC = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
    AB = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    SK = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE),
    MB = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE),
    YT = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    NT = c(FALSE, FALSE, TRUE, TRUE, FALSE,TRUE, TRUE, TRUE, FALSE, TRUE),
    NU = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  sAN <- studyAreaName
  
  sim$sppEquiv <- sppEquiv[which(SASppToUse[, ..sAN][[1]]), ] ##subset per SA
  sim$sppEquivCol <- "LandR"
  rm(sppEquivalencies_CA)
  
  #Assign colour
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sppEquiv,
                                   sppEquivCol = sppEquivCol,
                                   palette = "Paired")
  mixed <- structure("#D0FB84", names = "Mixed")
  sim$sppColorVect[length(sim$sppColorVect) + 1] <- mixed
  attributes(sim$sppColorVect)$names[length(sim$sppColorVect)] <- "Mixed"
  
 
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### add additional events as needed by copy/pasting from above
