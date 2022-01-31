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
  keywords = "WBI",
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
    defineParameter("bcr", "character", NA, NA, NA,
                    paste("BCR region used for subdivision of the WB study area when",
                          "using reclassification module WBI_vegReclass.Options are:",
                          "bcr4", "bcr6", "bcr7", "bcr8",
                          "bcr6BC", "bcr6AB","bcr6SK", "bcr6MB", "bcr4MB", "bcr8SK",
                          "bcr8MB","bcr7MB", "bcr7SK")),
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    paste("Distance (m) to buffer studyArea and rasterToMatch when",
                          "creating large versions.")),
    defineParameter("studyAreaName", "character", "AB", NA, NA,
                    paste("study area name for WB project, options are:"
  ))),
  inputObjects = bindrows(

  ),
  outputObjects = bindrows(
    createsOutput("firePoints", objectClass = "SimpleFeatureCollection",
                  desc = "fire points for the study area"),
    createsOutput("rasterToMatch", objectClass = "RasterLayer",
                  desc = "template raster"),
    createsOutput("rasterToMatchLarge", objectClass = "RasterLayer",
                  desc = "template raster for larger area"),
    createsOutput("rasterToMatchReporting", objectClass = "RasterLayer",
                  desc = "template raster for reporting area"),
    createsOutput("sppColorVect", objectClass = "character",
                  desc = "species colours for plotting"),
    createsOutput("sppEquiv", objectClass = "character",
                  desc = "table of LandR species names equivalencies"),
    createsOutput("sppEquivCol", objectClass = "character",
                  desc = "name of column to use in sppEquiv"),
    createsOutput("standAgeMap2011", objectClass = "RasterLayer",
                  desc = "time since disturbance raster for year 2011"),
    createsOutput("studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for simulation (buffered to mitigage edge effects"),
    createsOutput("studyAreaLarge", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for module parametrization(buffered)"),
    createsOutput("studyAreaReporting", objectClass = "SpatialPolygonsDataFrame",
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
  #### Prep study-area specific objects ####
  #allowedStudyAreas <- c("bcr6AB", "bcr6BC", "bcr6MB", "bcr6NWT", "NU", "bcr6SK", "YT") ## prov/terr x BCR intersections
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

  if (packageVersion("reproducible") >= "1.2.5") {
    fn1 <- function(x) {
      x <- readRDS(x)
      x <- st_as_sf(x)
      st_transform(x, targetCRS)
    }
  } else {
    fn1 <- "readRDS"
  }

  canProvs <- Cache(prepInputs,
                    "GADM",
                    fun = fn1,
                    dlFun = "raster::getData",
                    country = "CAN", level = 1, path = paths1$inputPath,
                    #targetCRS = targetCRS, ## TODO: fails on Windows
                    targetFile = "gadm36_CAN_1_sp.rds",
                    cacheRepo = paths1$cachePath,
                    destinationPath = paths1$inputPath
  )

  if (packageVersion("reproducible") < "1.2.5") {
    canProvs <- st_as_sf(canProvs) %>%
      st_transform(., targetCRS)
  }

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
  ## BCR subdivision
  #################################################################################
  AB <- c("Alberta")
  BC <- c("British Columbia")
  MB <- c("Manitoba")
  SK <- c("Saskatchewan")
  NT <- c("Northwest Territories")
  NU <- c("Nunavut")
  YK <- c("Yukon")
##BCR in WB
  bcr4 <- bcrshp[bcrshp$BCR %in% c(4), ]
  bcr6 <- bcrshp[bcrshp$BCR %in% c(6), ]
  bcr7 <- bcrshp[bcrshp$BCR %in% c(7), ]
  bcr8 <- bcrshp[bcrshp$BCR %in% c(8), ]

##provinces and territories in WB
  AB <- canProvs[canProvs$NAME_1 %in% AB, ]
  BC <- canProvs[canProvs$NAME_1 %in% BC, ]
  MB <- canProvs[canProvs$NAME_1 %in% MB,]
  SK <- canProvs[canProvs$NAME_1 %in% SK,]
  NT <- canProvs[canProvs$NAME_1 %in% NT, ]
  NU <- canProvs[canProvs$NAME_1 %in% NU, ]
  YK <- canProvs[canProvs$NAME_1 %in% YK, ]

  # bcr4SA <- reproducible::Cache(postProcess,
  #                               bcr4,
  #                               studyArea = WBstudyArea,
  #                               useSAcrs = TRUE,
  #                               cacheRepo = asPath(Paths$cachePath),
  #                               destinationPath = asPath(Paths$inputPath),
  #                               filename2 = NULL)
  # bcr6SA <- reproducible::Cache(postProcess,
  #                               bcr6,
  #                               studyArea = WBstudyArea,
  #                               useSAcrs = TRUE,
  #                               cacheRepo = asPath(Paths$cachePath),
  #                               destinationPath = asPath(Paths$inputPath),
  #                               filename2 = NULL)
  # bcr7SA <- reproducible::Cache(postProcess,
  #                               bcr7,
  #                               studyArea = WBstudyArea,
  #                               useSAcrs = TRUE,
  #                               cacheRepo = asPath(Paths$cachePath),
  #                               destinationPath = asPath(Paths$inputPath),
  #                               filename2 = NULL)
  # bcr8SA <- reproducible::Cache(postProcess,
  #                               bcr8,
  #                               studyArea = WBstudyArea,
  #                               useSAcrs = TRUE,
  #                               cacheRepo = asPath(Paths$cachePath),
  #                               destinationPath = asPath(Paths$inputPath),
  #                               filename2 = NULL)
   #bcr6SA <- as_Spatial(bcr6SA)
#browser()
if (grepl("AB", P(sim)$studyAreaName)){
  bcrAB <- st_intersection(bcrWB, AB)
  sim$studyArea <- bcrAB
}
if (grepl("BC", P(sim)$studyAreaName)){
  bcrBC <- st_intersection(bcrWB, BC)
  bcr6BC <- st_intersection(bcrBC, bcr6)
  sim$studyArea <- bcr6BC
}
if (grepl("MB", P(sim)$studyAreaName)){
  bcrMB <- st_intersection(bcrWB, MB)
  sim$studyArea <- bcrMB
}
if (grepl("SK", P(sim)$studyAreaName)){
  bcrSK <- st_intersection(bcrWB, SK)
  sim$studyArea <- bcrSK
}

if (grepl("NT", P(sim)$studyAreaName)){
  bcrNT <- st_intersection(bcrWB, NT)
  sim$studyArea <- bcrNT
}
if (grepl("NU", P(sim)$studyAreaName)){
  bcrNU <- st_intersection(bcrWB, NU)
  sim$studyArea <- bcrNU
}
if (grepl("YK", P(sim)$studyAreaName)){
  bcrYK <- st_intersection(bcrWB, YK)
  sim$studyArea <- bcrYK
}

  # bcr6SA <- reproducible::Cache(postProcess,
  #                               provsBCR6,
  #                               studyArea = bcr6,
  #                               useSAcrs = TRUE,
  #                               cacheRepo = asPath(Paths$cachePath),
  #                               destinationPath = asPath(Paths$inputPath),
  #                               filename2 = "bcr6_studyArea")

  ## in order to be able to rasterize, we need to create a numeric column to ID each of the provinces
  ## for BCR6
  #bcr6SA$ID <- as.numeric(as.factor(bcr6SA$NAME_1))


  ## In addition, this object has problems when rasterize, that is why geometry is being
  ## homogenize by using st_cast
#   #bcr6SA <- st_cast(bcr6SA, "MULTIPOLYGON") %>% as_Spatial(bcr6SA) ## TODO:
# if (grepl("AB", P(sim)$studyAreaName)){
#   if(grepl("bcr6", P(sim)$bcr)){
#     ## BCR6 Alberta
#   bcr6AB <- reproducible::Cache(postProcess,
#                                 bcr6SA,
#                                 studyArea = AB,
#                                 useSAcrs =  TRUE,
#                                 filename2 = NULL,
#                                 cacheRepo = Paths$cachePath)
#   sim$studyArea <- bcr6AB
#   }
# } else if (grepl("BC", P(sim)$studyAreaName)){
#   if(grepl("bcr6", P(sim)$bcr)){
#   ## BCR6 British Columbia
#   bcr6BC <- reproducible::Cache(postProcess,
#                                   bcr6SA,
#                                   studyArea = BC,
#                                   useSAcrs =  TRUE,
#                                   filename2 = NULL,
#                                   cacheRepo = Paths$cachePath)
#   sim$studyArea <- bcr6BC
#   } else if(grepl("bcr4", P(sim)$bcr)){
#     bcr4BC <- reproducible::Cache(postProcess,
#                                   bcr4SA,
#                                   studyArea = BC,
#                                   useSAcrs =  TRUE,
#                                   filename2 = NULL,
#                                   cacheRepo = Paths$cachePath)
#     sim$studyArea <- bcr4BC
#
#   }
# } else if (grepl("NT", P(sim)$studyAreaName)){
#   if(grepl("bcr4",P(sim)$bcr)){
#     bcr4NT <- reproducible::Cache(postProcess,
#                                   bcr4SA,
#                                   studyArea = NT,
#                                   useSAcrs = TRUE,
#                                   filename2 = NULL,
#                                   cacheRepo = Paths$cachePath)
#     sim$studyArea <- bcr4NT
#
#   }else if (grepl("bcr6", P(sim)$bcr)){
#     ## BCR6 North West Territories
#     bcr6NT <- reproducible::Cache(postProcess,
#                                   bcr6SA,
#                                   studyArea = NT,
#                                   useSAcrs = TRUE,
#                                   filename2 = NULL,
#                                   cacheRepo = Paths$cachePath)
#     sim$studyArea <- bcr6NT
#   }else if (grepl("bcr7", P(sim)$bcr)){
#     bcr7NT <- reproducible::Cache(postProcess,
#                                   bcr7SA,
#                                   studyArea = NT,
#                                   useSAcrs = TRUE,
#                                   filename2 = NULL,
#                                   cacheRepo = Paths$cachePath)
#     sim$studyArea <- bcr7NT
# }
# } else if (grepl("SK", P(sim)$studyAreaName)){
#   if(grepl("bcr6", P(sim)$bcr)){
#   bcr6SK <- reproducible::Cache(postProcess,
#                                 bcr6SA,
#                                 studyArea = SK,
#                                 filename2 = NULL,
#                                 cacheRepo = Paths$cachePath)
#   sim$studyArea <- bcr6SK
#   } else if (grepl("bcr7", P(sim)$bcr)){
#     bcr7SK <- reproducible::Cache(postProcess,
#                                   bcr7SA,
#                                   studyArea = SK,
#                                   filename2 = NULL,
#                                   cacheRepo = Paths$cachePath)
#     sim$studyArea <- bcr7SK
#   }else if (grepl("bcr8", P(sim)$bcr)){
#     bcr8SK <- reproducible::Cache(postProcess,
#                                   bcr8SA,
#                                   studyArea = SK,
#                                   filename2 = NULL,
#                                   cacheRepo = Paths$cachePath)
#     sim$studyArea <- bcr8SK
#   }
# } else if (grepl("MB", P(sim)$studyAreaName)){
#   if(grepl("bcr6", P(sim)$bcr)){
#   bcr6MB <- reproducible::Cache(postProcess,
#                                 MB,
#                                 studyArea = bcr6SA,
#                                 filename2 = NULL,
#                                 cacheRepo = Paths$cachePath)
#   sim$studyArea <- bcr6MB
#    } else if(grepl("bcr7", P(sim)$bcr)){
#      bcr8MB <- reproducible::Cache(postProcess,
#                                    MB,
#                                    studyArea = bcr7SA,
#                                    filename2 = NULL,
#                                    cacheRepo = Paths$cachePath)
#      sim$studyArea <- bcr7MB
#    } else if(grepl("bcr8", P(sim)$bcr)){
#      bcr8MB <- reproducible::Cache(postProcess,
#                                    MB,
#                                    studyArea = bcr8SA,
#                                    filename2 = NULL,
#                                    cacheRepo = Paths$cachePath)
#      sim$studyArea <- bcr8MB
#   }
# } else if(grepl("YK", P(sim)$studyArea)){
#     bcr4YK <- reproducible::Cache(postProcess,
#                                 YK,
#                                 studyArea = bcr4SA,
#                                 filename2 = NULL,
#                                 cacheRepo = Paths$cachePath)
#   sim$studyArea <- bcr4YK
# } else if(grepl("NU", P(sim)$studyArea)){
#   bcr7NU <- reproducible::Cache(postProcess,
#                                 NU,
#                                 studyArea = bcr7SA,
#                                 filename2 = NULL,
#                                 cacheRepo = Paths$cachePath)
#   sim$studyArea <- bcr7NU
# }
  #sim$studyArea <- as_Spatial(sim$studyArea)
  #sim$studyArea <- spTransform(sim$studyArea, targetCRS)

  #sim$studyArea <- as_Spatial(sim$studyArea)
  sim$studyArea <- st_cast(sim$studyArea, 'POLYGON')  ## line added because masking RTM fails later in the script
  sim$studyArea$studyAreaName <- P(sim)$studyAreaName
  sim$studyAreaReporting <- sim$studyArea

  #studyArea and studyAreaLarge are the same buffered area
  sim$studyArea <- st_buffer(sim$studyArea, P(sim)$bufferDist)
  sim$studyAreaLarge <- sim$studyArea
#browser()
  #################################################################################
  ## LCC 2010
  #################################################################################
##create a RTM for lcc2010 have a 250 m resolution
  RTM <- reproducible::Cache(LandR::prepInputsLCC,
                             destinationPath = asPath(Paths$inputPath),
                             studyArea = sim$studyArea,
                             year = 2005,
                             useCache = P(sim)$.useCache,
                             filename2 = paste0(P(sim)$studyAreaName, "_250_RTM.tif"))


   # sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
   #                            destinationPath = asPath(Paths$inputPath),
   #                            rasterToMatch = RTM,
   #                            year = 2010,
   #                            filename2 = paste0(P(sim)$studyAreaName, "_rtm.tif"))
  sim$rasterToMatch <- RTM
browser()
  sim$rasterToMatchReporting <- Cache(maskInputs, sim$rasterToMatch, sim$studyAreaReporting)
  sim$rasterToMatchLarge <- sim$rasterToMatch



  #################################################################################
  ## Age
  #################################################################################
  standAgeMapURL <- paste0(
    "ftp://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
    "canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/",
    "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"
  )


  sim$standAgeMap2011 <-  LandR::prepInputsStandAgeMap(
                           destinationPath = asPath(Paths$inputPath),
                           ageUrl = standAgeMapURL,
                           ageFun = "raster::raster",
                           studyArea = sim$studyArea,
                           rasterToMatch = sim$rasterToMatch,
                           # maskWithRTM = TRUE,
                           method = "bilinear",
                           useCache = TRUE,
                           datatype = "INT2U",
                           filename2 = paste0("standAgeMap_", P(sim)$studyAreaName, ".tif"),
                           startTime = 2011)


  sim$rstLCC <- reproducible::Cache(LandR::prepInputsLCC,
                                    destinationPath = asPath(Paths$inputPath),
                                    studyArea = sim$studyArea,
                                    rasterToMatch = sim$rasterToMatch,
                                    year = 2005,
                                    filename2 = paste0(P(sim)$studyAreaName, "_rstLCC.tif"))

  # sim$rstLCC <- reproducible::Cache(LandR::prepInputsLCC,
  #                                   destinationPath = asPath(Paths$inputPath),
  #                                   studyArea = sim$studyArea,
  #                                   rasterToMatch = sim$rasterToMatch,
  #                                   year = 2010,
  #                                   filename2 = paste0(P(sim)$studyAreaName, "_rstLCC.tif"))

  sim$flammableMap <- LandR::defineFlammable(LandCoverClassifiedMap = RTM,
                                         nonFlammClasses = c(33, 36:39), # for LCC 2005
                                         #nonFlammClasses = c(13, 16, 17, 18, 19),
                                         mask = sim$rasterToMatch)

  # firePointsURL <- "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip"
  # sim$firePoints <- Cache(fireSenseUtils::getFirePoints_NFDB,
  #                     url = firePointsURL,
  #                     studyArea = sim$studyArea,
  #                     rasterToMatch = sim$rasterToMatch,
  #                     redownloadIn = 1,
  #                     years = 1991:2017, ## TODO: @araymund83 these are default years; do you want others?
  #                     fireSizeColName = "SIZE_HA",
  #                     NFDB_pointPath = asPath(Paths$inputPath)) %>%
  #   st_as_sf(.)

  biomassMapURL <- paste0(
    "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
    "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/",
    "NFI_MODIS250m_2001_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif"
  )

  # # with_config(config = config(ssl_verifypeer = 0L), {
    sim$rawbiomassMap2001 <- Cache(prepInputs,
                               destinationPath = asPath(Paths$inputPath),
                               url = biomassMapURL,
                               fun = "raster::raster",
                               studyArea = sim$studyArea,
                               rasterToMatch = sim$rasterToMatch,
                               maskWithRTM = TRUE,
                               method = "bilinear",
                               datatype = "INT2U",
                               filename2 = paste0("rawBiomassMap2001_", P(sim)$studyAreaName, ".tif"))
  #})

  sim$vegMap <- Cache(LandR::prepInputsLCC,
                  year = 2005,
                  destinationPath = asPath(Paths$inputPath),
                  studyArea = sim$studyArea,
                  rasterToMatch = sim$rasterToMatch,
                  filename2 = paste0("vegMap_",P(sim)$studyAreaName,".tif")
  )

  # ecoregionsURL <- paste0("https://drive.google.com/file/d/",
  #                         "1y2-mM7NBxEvdeoDENkwbQDa-rdQjiutx/view?usp=sharing")
  # sim$ecoregionsMap <- Cache(prepInputs,
  #                        destinationPath = Paths$inputPath,
  #                        url = ecoregionsURL,
  #                        #fun = "read_sf",
  #                        studyArea = sim$studyArea,
  #                        rasterToMatch = sim$rasterToMatch,
  #                        maskWithRTM = TRUE,
  #                        method = "bilinear",
  #                        datatype = "INT2U",
  #                        filename2 = paste0("ecoregionsMap_",P(sim)$studyAreaName,".tif")
  # )

##all species considered in WB (will be subset later for each study area)
  data("sppEquivalencies_CA", package = "LandR", envir = environment())
  allWBspp <- c("Abie_Bal", "Abie_Las", "Betu_Pap", "Lari_Lar",
                "Pice_Eng", "Pice_Gla", "Pice_Mar", "Pinu_Ban",
                "Pinu_Con", "Popu_Tre")
  sppEquiv <- sppEquivalencies_CA[KNN %in% allWBspp]
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
  sAN <- studyarea

  sim$sppEquiv <- sppEquiv[which(SASppToUse[, ..sAN][[1]]), ] ##subset per SA
  sim$sppEquivCol <- "LandR"
  rm(sppEquivalencies_CA)

  #Assign colour
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sim$sppEquiv,
                                   sppEquivCol = sim$sppEquivCol,
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
