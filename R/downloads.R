#' Internal dataset wrappers used by NATDB
#'
#' Each of these functions downloads a dataset from a published
#' dataset that (to the best of the authors' knowledge) has been
#' released for public use. Please note that it is *impossible* for
#' NATDB to download a dataset that has not been released to the user;
#' NATDB is Not A DataBase; these functions download data to which the
#' user already has access.
#' 
#' @param ... Optional arguments that can be specified to a function
#'     at run-time. Currently, all arguments passed to any of these
#'     functions are ignored.
#' @return \code{\link{natdb}} object
#' @author William D. Pearse, Maxwell J.Farrell, Konrad C. Hafen,
#'     Mallory A. Hagadorn, Spencer B. Hudson, Sylvia P. Kinosian,
#'     Ryan McCleary, Anne E. McManis, Alexandre Rego, & Kathryn M
#'     Welglarz
#' @rdname datasets
#' @name datasets
#' @examples
#' # You almost certainly *don't* want to use these functions on their own
#' # - use the function "natdb" to build a database instead
#' .pearse.2014()
#' @export
#' @importFrom gdata ls.funs
#' @importFrom suppdata suppdata
#' @importFrom readxl read_excel read_xls read_xlsx
#' @importFrom utils read.csv read.csv2 read.delim read.table unzip
#' @importFrom testdat sanitize_text
#' @importFrom tidyr unite
#' @importFrom caper pgls
#' @importFrom utils type.convert data download.file unzip
#' @importFrom reshape2 dcast
#' @importFrom httr GET
.albouy.2015 <- function(...){
    data <- read.csv(unzip(suppdata("E096-203","Functional_data.zip", "esa_archives"), "Functional_data.csv"), sep=";")
    for(i in 11:17)
        data[,i] <- as.logical(data[,i])
    names(data)[18:19] <- c("minimum_depth", "maximum_depth")
    metadata <- data[,c("id","Super_class","Order","Family","Genus")]
    data <- data[,!names(data) %in% names(metadata)]
    data$Species <- tolower(gsub("_", " ", data$Species))
    return(.df.melt(data, "Species", c(NA,"cm","cm",rep(NA,8),"m","m",rep(NA,9)), metadata))
}

#' @export
.ameztegui.2016 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.12b0h/2","FunctionalTraits_Dryad.xlsx")))
    data <- data[,-c(1,7,8)]
    names(data)[4:14] <- c("Phylum","leaf_habit","specific_leaf_area","photosynthetic_capacitity_per_unit_leaf_mass","N_content_per_unit_mass","P_content_per_unit_mass","leaf_lifespan","leaf_length","seed_mass","wood_density","max_tree_height")
    units <- c(rep(NA,3),"deciduous/evergreen","m/kg","mmol CO2/g s","%","%","month","mm","mg/seed","g cm^3","m")
    metadata <- data[,2:4]
    data <- .df.melt(data, "SpName", units=units, metadata=metadata)
    return(data)
}

#' @export
.anderson.2015 <- function (...){
    data<-read.csv(suppdata("10.1371/journal.pone.0166714",2))
    metadata<-data[,c(2:4,44:46)]
    data<-data[,-c(2:4,44:46)]
    units<-c("cm","g","cm^2",rep("?",4),rep("cm^2",7),"?",rep("g",7),rep("mg/g",7),rep("cm^2/g",7),rep("g",4))
    data<-.df.melt(data,"Species",units=units,metadata=metadata)
    return(data)
}

#' @export
.arnold.2016 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.t3d52", "Arnold_etal_2016_functecol_dataset.xlsx"), skip = 3))
    species <- rep(c("Tribolium_castaneum"), nrow(data))
    data <- data.frame(species, data)
    metadata <- data[,c(2:7,24:25)]
    data <- data[,-c(2:7,23:25)]
    units <- c("uL/h", "#/h", "mg", rep("mm",7), rep("mm/s",2), rep("#",3), NA, NA, NA, NA, "days", NA, NA, NA)
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.artacho.2015 <- function(...){
    data <- read.csv2(suppdata("10.5061/dryad.qg062", "phenotypictraits.csv"), sep=';')
    metadata <- data[,c(2:3,5)]
    data <- data[,-c(1:5,10)]
    data$species <- 'zootoca_vivipara'
    units <- c('mm', 'mm', 'g','C', 'J/h', 'cm/sec','cm/sec')
    data$SVL <- as.numeric(data$SVL)
    data$TTL <- as.numeric(data$TTL)
    data$weight <- as.numeric(data$weight)
    data$PBT <- as.numeric(data$PBT)
    data$RMR <- as.numeric(data$RMR)
    colnames(data) <- c('snout_vent_length', 'total_length','body_mass', 'prefered_body_temperature','resting_metabolic_rate','maximum_sprint_speed_intercept','maximum_sprint_speed_slope','species')
    data$maximum_sprint_speed_intercept <- as.numeric(data$maximum_sprint_speed_intercept)
    data$maximum_sprint_speed_slope <- as.numeric(data$maximum_sprint_speed_slope)
    return(.df.melt(data, "species", units=units, metadata))
}

#' @export
.aubret.2012a <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), sheet=1))
    data$species <- "notechis_scutatus"
    metadata <- data.frame(data$POPULATION, data$DATE)
    data$POPULATION <- NULL
    data$DATE <- NULL
    names(data) <- c("sex", "body_mass", "body_mass", "snout_vent_length", "snout_vent_length", "species")
    units <- c(NA, "g", "log10(g)", "cm", "log10(cm)")
    return(.df.melt(data, "species", units, metadata))
}
.aubret.2012b <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), sheet=2))
    data$X <- data$X.1 <- data$X.2 <- data$X.3 <- data$BCI <- NULL
    data$species <- "notechis_scutatus"
    metadata <- data[,c("POPULATION", "MOTHER", "BIRTH DATE")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data) <- c("snout_vent_length", "body_mass_pre", "body_mass_pre", "body_mass_post", "body_mass_post", "bm_body_mass_post", "rcm", "total_litter_body_mass", "total_litter_body_mass", "total_litter_snout_vent_length", "total_litter_snout_vent_length", "total_litter_jaw_length", "total_litter_jaw_length","species")
    units <- c("log10(cm)", "g", "log10(g)", "log10(mean(cm))", "g", "log10(g)", "?", "?", "mean(g)", "log10(mean(g))", "mean(cm)", "log10(mean(cm))", "mean(cm)", "log10(mean(cm))", NA)
    return(.df.melt(data, "species", units, metadata))
}
#' @export
.aubret.2012c <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), sheet=3))
    names(data) <- c("genus_species", "body_mass", "max_circumference")
    units <- c("g", "cm")
    return(.df.melt(data, "genus_species", units))
}

#' @export
.augspurger.2016a<-function(...){
    data <- read.csv(suppdata("10.5061/dryad.56cn4","Data File 1. Diaspore traits.csv"), as.is=TRUE)
    units <- c("mg","cm^2","mg/cm^2","cm/s")
    return(.df.melt(data,"Species",units=units))
}
#' @export
.augspurger.2016b<-function(...){
    data <- read.csv(suppdata("10.5061/dryad.56cn4","Data File 3. Parent tree information.csv"))
    metadata <- data[,2,drop=FALSE]
    data <- data[,-2]
    units <- c("cm","m","m","m^2",NA,"m^2")
    return(.df.melt(data,"Species",units=units,metadata=metadata))
}

#' @export
.bello_bedoy.2015a <- function(...){
    data <- read.csv(
        suppdata("10.6084/m9.figshare.1190766.v2","Mating success H americana.csv")
    )
    data$species <- "hetaerina_americana"
    metadata <- data.frame(id=data$id)
    data$id <- NULL
    data <- .df.melt(data, "species", c(NA, "#", "mm^2 ", "mm^2", "mm"), metadata)
    data$numeric$variable <- gsub("aspot", "wing_spot_area", data$numeric$variable, fixed=TRUE)
    data$numeric$variable <- gsub("awing", "wing_area", data$numeric$variable, fixed=TRUE)
    data$numeric$variable <- gsub("matings", "mating_events", data$numeric$variable, fixed=TRUE)
    data$numeric$variable <- gsub("lwing", "wing_length", data$numeric$variable, fixed=TRUE)
    return(data)
}
.bello_bedoy.2015b <- function(...){
    data <- as.data.frame(read_excel(
        suppdata("10.6084/m9.figshare.1190766.v2","mating occurrencePzoe_2010.xls")
    ))
    data$species <- "paraphlebia_zoe"
    metadata <- data.frame(data$Individual)
    data$Individual <- data$Estatus <- NULL
    levels(data$Status) <- c("satellite.male", "territorial.male")
    return(.df.melt(data, "species", c(NA, NA, "mm", "mm", "?", "?", "log10(?)"), metadata))
}

# Out of commission until https://github.com/willpearse/suppdata/issues/22 is closed
### #' @export
#.benesh.2017 <- function(...){
#    data <- read.csv(unzip(suppdata("10.1002/ecy.1680", 1), "CLC_database_lifehistory.csv"))
#    metadata <- data[,c("Parasite.genus", "Parasite.group", "Development.remarks", "Size.reported.as", "Size.remarks", "Author", "Year", "Journal", "Volume", "Pages")]
#    data <- data[,!names(data) %in% names(metadata)]
#    return(.df.melt(data, "Parasite.species", c(NA, "#", NA, NA, "days", "degC", "mm", "mm", "mm", "mm", "n", NA, NA, NA), metadata))
#}

#' @export
.bengtsson.2016 <- function(...){
    data <- read.csv(suppdata('10.5061/dryad.62054', 'bengtsson_etal_2016_traits.csv'), as.is=TRUE)
    data$species.name <- tolower(data$species.name)
    metadata <- data[,c(1:4,6:10)]
    data <- data[,-c(1:4,6:10)]
    units <- c(NA, rep("%",4), "g", "g/cm^2", "g", "g/cm^2", "g", "g/cm^2", "mm", "mm", "cm^-2", "mg/cm^3", "mm", "cm^-2", "mg/cm^3", "mg cm-^2 h^-1", "mg g^-1 h^-1", "mg h^-1", "%", "%", "C/N", "cm^-2", "mg/cm^3", "mm", NA, NA, "latitude", "longitude", NA, NA, NA, NA, NA)
    data <- .df.melt(data, "species.name", units, metadata)
    return(data)
}

#' @export
.broeckhoven.2016 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.k186f", "Data%20for%20Dryad.xlsx"), skip=1))
    data <- data[,-c(23:102)]
    data$Species <- tolower(gsub(" ", "_", (data$Species)))
    names(data) <- tolower(gsub("\\.", "_", names(data)))
    units <- c(rep('mm',21))
    return(.df.melt(data, "species", units=units))
}

#' @export
.brown.2015 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.m3d4d/1", "BrownGrahamTraitsData.xlsx")))
    data <- data[,-c(4,16)]
    units <- c(rep(NA,12),"g")
    metadata <- data[,1,drop=FALSE]
    return(.df.melt(data, "Latin Name", units, metadata))
}

#' @export
.buzzard.2015 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.s8f38", "FEBuzzardSpTraits.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
    data$species <- with(data, tolower(paste(genus, species, sep="_")))
    metadata <- data[,c(1:3,5:8,15:17)]
    data <- data[,-c(1:3,5:8,15:17)]
    units <- c("cm^2/g", "mg/g", rep("?",4))
    names(data)[2:3] <- c("specific_leaf_area","leaf_dry_matter_content")
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.cariveau.2016 <- function(...){
    data <- as.data.frame(read_xlsx(suppdata("10.1371/journal.pone.0151482", 3), sheet="TableS1_v2"))
    metadata <- data[,c(1:3,6)]
    data$species <- with(data, tolower(paste(genus, species, sep="_")))
    data <- data[-c(1:4,6)]
    data <- .df.melt(data, "species", units=c(rep(NA, 9), "mm","mm","mm","mm", NA, NA), metadata)
    return(data)
}

#' @export
.carmona.2014 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.53550", "Traits%20per%20species%20and%20quadrat.csv"), sep = ";", as.is=TRUE)
    names.data <- read.csv(suppdata("10.5061/dryad.53550", "Species%20key.csv"), sep = ";", as.is=TRUE)
    data$binomial <- names.data[,2][match(data$Species, names.data[,1])]
    metadata <- data[,1:3]
    data <- data[,-1:-3]
    units <- c(NA, rep("cm",10), rep("mm^2",10), rep("mm^2/mg",10))
    data <- .df.melt(data, "binomial", units, metadata)
    data$numeric$variable <- gsub("Height[0-9]*", "height", data$numeric$variable)
    data$numeric$variable <- gsub("LeafArea[0-9]*", "leaf_area", data$numeric$variable)
    data$numeric$variable <- gsub("SLA[0-9]*", "specific_leaf_area", data$numeric$variable)
    return(data)
}

#' @export
.case.2016 <- function(...){
    data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.103099/Traits.2Feb2015.csv?sequence=1", sep = ",", header = TRUE)
    #units <- length(names(data))
    data <- .df.melt(data, "species")
    return(data)
}

#' @export
.castillo.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.gm465","Castillo and Delph isofemale data.csv"))
    names(data) <- tolower(names(data))
    data$species <- 'caenorhabditis_remanei'
    metadata <- data[,c(1:2,8)]
    data <- data[,-c(1:2,8)]
    units <- c('um','um',rep('sec',3))
    return(.df.melt(data, "species", units,metadata))
}

#' @export
.cavender_bares.2015a <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.855pg","Freezing.vulnerability.csv"), fileEncoding="UTF-8")
    data <- data[1:758,]
    metadata <- data[,c(2,3)]
    data <- data[,-c(2,3)]
    units <- c('C', 'C', '%', '%')
    return(.df.melt(data,"Species",units=units, metadata))
}
#' @export
.cavender_bares.2015b <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.855pg","Tree.height.csv"))
    data <- data[1:109,1:8]
    metadata <- data[,2:6]
    data <- data[,-c(2:6)]
    units <- c("m", 'cm')
    return(.df.melt(data,"Species",units=units, metadata))
}
#' @export
.cavender_bares.2015c <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.855pg","PCA_All_Virentes.csv"))
    metadata <- data[,c(1,2,4:9,11:18)]
    data <- data[,-c(1,2,4:9,11:18)]
    names <- data['Species']
    for(i in 1:17){
        p <- c('BR', 'FU', 'GE', 'MN', 'OL', 'SA','VI','FU2', 'OL2', 'SA2', 'VI2', 'VI3', 'MN2', 'VI4', 'GE1', 'GE3', 'HY')
        r <- c("q_brandegeei","q_fusiformis", "q_geminata", "q_minima", "q_oleoides", "q_sagraena", "q_virginiana", "q_fusiformis2", "q_oleoides2", "q_sagraena2", "q_virginiana2", "q_virginiana3","q_minima2", "q_virginiana4", "q_geminata1", "q_geminata3", "hybrid")
        names[,1] <- gsub(p[i], r[i], names[,1])
    }
    units <- c("mm^2", 'mm^2','C','C','%','%',rep('C',7),rep('mm',3),'NA',rep('mm',4), 'C', 'C', '%', 'NA', rep('C',7), rep('mm',3), 'NA', rep('mm',4))
    return(.df.melt(data,"Species",units=units, metadata))
}

#' @export
.comeault.2013 <- function(...){
    data <- read.table(suppdata("10.5061/dryad.ck2cm","Tcris_FHA_phenotypes.txt"),header=TRUE)
    data <- data[,-c(1,4)]
    colnames(data) <- c('sex','morph','hue_green_color_chip', 'saturation_green_color_chip', 'brightness_green_color_chip', 'lateral_hue_average', 'lateral_saturation_average', 'lateral_brightness_average', 'midsaggital_hue_average', 'midsaggital_saturation_average', 'midsaggital_brightness_average','body_length','body_width','head_width','proportion_striped')
    data$species <- 'timema_cristinae'
    metadata <- data[,c(1:2)]
    data <- data[,-c(1:2)]
    units <- c(rep(NA,8),rep('cm',4),rep(NA,2))
    return(.df.melt(data, "species",units,metadata))
}

#' @export
.delaRiva.2015 <- function(...){
    data <- as.data.frame(read_excel(suppdata('10.5061/dryad.dr275.2', 'Dryad_database.xls'), sheet='Traits'))
    names(data) <- c("species", "country", "province", "zone", "plot", "latitude", "longitude", "height", "plant_cover", "leaf_size", "leaf_dry_matter_content", "species_leaf_area","LChl", "LNC", "delta_13C", "SDMC", "WD", "RDMC", "SRL")
    data$species <- tolower(gsub(" ", "_", data$species, ignore.case = TRUE))
    metadata <- data[,c(2:7)]
    data <- data[,-c(2:7)]
    units <- c("m", "m^2", "cm^2", "g^-1", "m^2 Kg^-1", "ug g^-1", "%", "%", "g^-1", "g cm^-3", "g g^-1", "m g^-1", rep("NA", 4), "Latitude", "Longitude")
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.deraison.2014 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.5q33h","Plant traits.xls"), sheet = 2))
    name.data <- as.data.frame(read_excel(suppdata("10.5061/dryad.5q33h","Plant traits.xls"), sheet = 1))
    data$Plant.species <- name.data$Species.name[1:22]
    names(data) <- c("species","leaf_dry_matter", "leaf_nitrogen_content", "leaf_carbon_content", "leaf_carbon_nitrogen_ratio", "leaf_thickness", "leaf_area", "perimeter_leaf_length_ratio")
    data <- data[,1:8]
    units <- c("%", rep("% g",2), NA, "mm", "cm^2", NA)
    data <- .df.melt(data, "species", units=units)
    return(data)
}

#' @export
.dmitriew.2014 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.6g2f8","DataDmitriew&BlanckenhornJEB12488 2014.csv"),sep=';')
    data$species <- 'sepsis_punctum'
    data <- data[,-c(4)]
    colnames(data) <- c('population', 'food', 'sex', 'family', "sire",'dam','development_time','fore_femur_width','mittibial_length', 'species')
    metadata <- data[,c(1:6)]
    data <- data[,-c(1:6)]
    metadata$population <- tolower(metadata$population)
    metadata$food <- tolower(metadata$food)
    metadata$sex <- tolower(gsub('Female', 'f', metadata$sex))
    metadata$sex <- tolower(gsub('Male', 'm', metadata$sex))
    units <- c('days', 'mm','mm')
    data <- .df.melt(data, "species", units,metadata)
    return(data)
}

#' @export
.edwards.2015a <- function(...){
    data <- read.csv(suppdata("E096-202", "Table1.csv", "esa_archives"))
    metadata <- data[,c("isolate","taxon","synonym", "c_citation")]
    data <- data[,!names(data) %in% names(metadata)]
    units <- c(NA, "degC", "umol photons m-2 s-1", "hr", "um^3", "um^3", "umol/cell", "day^-1", "day^-1", "umol L-1", "umol L-1", "umol N cell^-1 day^-1", "umol N umol C-1 day -1", "umol N cell^-1", "umol N umol C-1", "umol N cell^-1", "umol N umol C-1", "day^-1", "day^-1", "umol L-1", "umol L-1", "umol N cell^-1 day^-1", "umol N umol C-1 day^-1", "umol N cell^-1", "umol N umol C-1", "umol N cell^-1", "umol N umol C-1", "day^-1", "day^-1", "umol L-1", "umol L-1", "umol P cell^-1 day^-1", "umol P umol C-1 day^-1", "umol P cell^-1", "umol P umol C-1", "umol P cell^-1", "umol P umol C-1", "citation")
    return(.df.melt(data, "species", units, metadata))
}
#' @export
.edwards.2015b <- function(...){
    data <- read.csv(suppdata("E096-202", "Table3.csv", "esa_archives"))
    metadata <- data[,c("isolate","volume_citation")]
    data$isolate <- data$volume_citation <- NULL
    data$species <- tolower(gsub(" ", "_", data$species))
    return(.df.melt(data, "species", "um3", metadata))
}

# Out of commission until https://github.com/willpearse/suppdata/issues/22 is closed
# #' @export
if(FALSE)
    .engemann.2016 <- function(...){
    data <- read.delim(unzip(suppdata("10.1002/ecy.1569", 1), "DataS1/GrowthForm_Final.txt"))
    metadata <- data[,c("FAMILY_STD", "CONSENSUS", "ID", "SOURCES")]
    data <- data[,!names(data) %in% names(metadata)]
    data$SPECIES_STD <- tolower(gsub(" ", "_", data$SPECIES_STD))
    return(.df.melt(data, "SPECIES_STD", rep(NA,2), metadata))
}

#' @export
.enriquezUrzelai.2015 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.d3v78","WM_anurans_traits.csv"), as.is=T)
    data <- subset(data, select=-c("SP","COLLECTION_N","ENTRANCE_N", "COLLECTION", "T.F"))
    data$LOC <- as.factor(data$LOC)
    colnames(data) <- c("species","locomotor_mode","snout_vent_length","tibiofibula_length","femur_length")
    units <- c(NA,"mm","mm","mm")
    data$species <- tolower(gsub(" ", "_", (data$species)))
    data <- .df.melt(data, "species", units=units)
    return(data)
}

#' @export
.falster.2015 <- function(...){
    data <- read.csv(unzip(suppdata("E096-128","baad_data.zip", "esa_archives"), "baad_data/baad_data.csv"))
    # Note: units "m^2/m^2 are taken from the meta-data
    units <- c(NA,"mm","deg",NA,"m^2/m^2",NA,NA,NA,NA,"year","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m","m","m","m","m","m","m","kg","kg","kg","kg","kg","kg","kg","kg","kg","kg","kg","m^2","kg/m^2","kg/m^3","kg/m^3","kg/m^3","kg/m^3","kg/kg","kg/kg","kg/kg","kg/kg","kg/kg","kg/kg")
    metadata <- data[,c("studyName","location","latitude","longitude","species","family")]
    data$speciesMatched <- tolower(gsub(" ", "_", data$speciesMatched))
    data <- data[,!names(data) %in% names(metadata)]
    return(.df.melt(data, "speciesMatched", units, metadata))
}

#' @export
.fargevieille.2017 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.b1q08","ColorTraitValuesECE3-2822.csv"),sep=';')
    data <- data[,-c(3,4)]
    levels(data$pop)<- c('d_muro','e_muro','e_pirio','d_rouviere')
    colnames(data) <- c('year','population','male_blue_brightness','female_blue_brightness','male_blue_hue','female_blue_hue','male_blue_uv_chroma','female_blue_uv_chroma','male_yellow_brightness','female_yellow_brightness','male_yellow_contrast','female_yellow_contrast')
    data$species <- 'cyanistes_caeruleus'
    metadata <- data[,1:2]
    data <- data[,-c(1:2)]
    units <- c(rep(NA,10))
    return(.df.melt(data, "species", units,metadata))
}

#' @export
.friedman.2014 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.489c7","NILs_rawdata.xls")))
    metadata <- data[,c(1:6,18,19)]
    data <- data[,-c(1:6,18,19)]
    units <- c('days',NA,'mm', 'mm',NA,rep('mm',6))
    data$Species <- 'M_guttatus'
    return(.df.melt(data,"Species",units=units, metadata))
}

#' @export
.fitzgerald.2017 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.67140", "traits.csv"), sep = ",", as.is = TRUE)
    species <- tolower(gsub(".", "_", data$species, fixed = TRUE))
    species <- gsub("sp_", "sp.", species)
    data <- data.frame(species, data[,-1])
    data <- data[,-c(2, 9:12)]
    units <- c(rep("log(mm)", 5), "sqrt(mm)", "cubert(mm)", "mm", "sqrt(mm)", "mm", "sqrt(mm)", "mm", "inverse(mm)", "log(mm)", "mm", "mm", "log(mm)", rep("mm", 3), "log_mm", rep("mm", 4), "inverse(mm)", "log(mm)", "cubert(mm)", rep("sqrt(mm)",2), "cubert(mm)", rep("mm",2), "inverse(mm)", rep("sqrt(mm)",2), "mm", "sqrt(mm)", rep("fourthroot(mm)",2), "mm", "log(mm)", rep("mm",3))
    data <- .df.melt(data, "species", units)
    return(data)
}

#' @export
.gossner.2015 <- function(...){
    data <- read.delim(suppdata("10.5061/dryad.53ds2", "ArthropodSpeciesTraits.txt"))
    metadata <- data[,c(1:3)]
    data <- data[,-c(1:3,5,17)]
    units <- c("mm",rep(NA,10))
    data <- .df.melt(data,"SpeciesID",units=units, metadata)
}

#' @export
.grootemaat.2015 <- function(...){
    data <- as.data.frame(read_excel(suppdata('10.5061/dryad.m41f1', 'Grootemaat%202015_FE_Dryad.xlsx'), sheet ='RAWdata'))
    # Ugh this is so kludgy...
    names <- names(data)
    data <- as.data.frame(as.matrix(data)[-1,], col.names=names, stringsAsFactors=FALSE)
    for(i in seq_along(names(data)))
        data[,i] <- type.convert(data[,i])
    names(data) <- names
    metadata <- data[,c(1,3)]
    data <- data[,-c(1,3)]
    units <- c(rep('mm',4), rep('g',2), rep('cm^2',2), 'cm^3', '1/cm', rep('g/cm^3',2), "%", "cm^2/g", rep("%",4), 'NA', rep('s',4), 'NA', 'NA')
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}

#' @export
.grutters.2017<-function(...){
    data<-as.data.frame(read_excel(suppdata("10.5061/dryad.d4k51","FE-2016-00091-Data-plant-traits-and-consumption.xlsx"), na=c(""," ","NA")))
    vars <- c("species", "native", "clade", "latitude_origin", "acquisition", "shared_pomacea", "consumption_rate_lymnaea", "shared_lymnaea",
              "consumption_rate_pomacea","c","n","p","dmc","total_phenolics","cn","cp","np","n_total_pheno")
    data <- data[-c(1),] #remove row with unit names
    data$plantScientificName = gsub(" ","_",data$plantScientificName) #replace spaces with underscore
    data$plantScientificName = gsub("-","_",data$plantScientificName) #replace hyphens with underscore
    data$plantScientificName <- sapply(strsplit(data$plantScientificName, "_"), function(x) paste(x[1:2], collapse="_"))
    data$plantScientificName = tolower(data$plantScientificName)
    colnames(data)<-vars
    metadata <- data[,c(2:6,8)]
    data <- data[,-c(2:6,8)]
    units <- c("mg g^-1 d^-1","mg g^-1 d^-1","percent_dry_weight", "percent_dry_weight",
               "percent_dry_weight", "g/g", "mg/g", "mol/mol","mol/mol","mol/mol", "mg g^-1 (mg g^-1)^-1")
    for(i in 2:ncol(data)){
        data[,i] = as.numeric(data[,i])
    }
    data <- .df.melt(data, "species",units=units, metadata = metadata)
    return(data)
}

# Out of commission until https://github.com/willpearse/suppdata/issues/22 is closed
# #' @export
if(FALSE) .hebert.2016 <- function(...){
    data <- read.csv(unzip(suppdata("10.1890/15-1275.1",1), "zooplankton_traits.csv"), sep=";", as.is=TRUE, dec=",")
    data$binomial <- tolower(paste(data$Genus, data$Species, sep="_"))
    metadata <- data[,c("Genus","Species","Replicate.number","Group","Ref.tg","Ref.bl","Ref.dm","Ref.C","Ref.N","Ref.P","Ref.NP","Ref.prot","Ref.lip","Ref.resp","Ref.N.ex","Ref.P.ex","Ref.NPex")]
    data <- data[,!names(data) %in% names(metadata)]
    data$Min.lip <- gsub(",",".",gsub(" ", "", data$Min.lip))
    data$Temp.Pex.rt <- gsub(",",".",gsub(" ", "", data$Temp.Pex.rt))
    data$Min.dm <- gsub(",",".",gsub(" ", "", data$Min.dm))
    data$Temp.resp.rt <- gsub(",",".",gsub(" ", "", data$Temp.resp.rt))
    data$Min.Nex.rt.ug.dm <- gsub(",",".",gsub(" ", "", data$Min.Nex.rt.ug.dm))
    data$Temp.Nex.rt <- gsub(",",".",gsub(" ", "", data$Temp.Nex.rt))
    data$P.phosphate.ex.rt.ug.ind <- gsub(",",".",gsub(" ", "", data$P.phosphate.ex.rt.ug.ind))
    data$Min.NP.ex <- gsub(",",".",gsub(" ", "", data$Min.NP.ex))
    units <- c(NA, NA, "mm", "mm", "mm","mg", "mg", "mg", "%", "%", "%", "mg", "%", "%", "%", "mg", "%","%", "%", "mg", ":", ":", ":", "%", "%", "%", "%", "%", "%", "ul O2 ind-1 h-1", "ul O2 mgDM -1 h-1","ul O2 mgDM -1 h-1", "ul O2 mgDM -1 h-1", "degC", "degC", "degC", "ug N-NH4+ ind-1 h-1", "ug N-NH4+ mgDM-1 h-1", "ug N-NH4+ mgDM-1 h-1", "ug N-NH4+ mgDM-1 h-1", "nmol N-NH4+ mg DM-1 h-1", "nmol N-NH4+ ind-1 h-1", "nmol N-NH4+ ind-1 h-1", "nmol N-NH4+ ind-1 h-1", "degC", "degC", "degC", "ug P-PO43- ind-1 h-1", "ug P-PO43- mg DM-1 h-1", "ug P-PO43- mg DM-1 h-1", "ug P-PO43- mg DM-1 h-1", "nmol P-PO43- mg DM-1 h-1", "nmol P-PO43- ind-1 h-1", "nmol P-PO43- ind-1 h-1", "nmol P-PO43- ind-1 h-1", "degC", "degC", "degC", ":", ":", ":")
    return(.df.melt(data, "binomial", units, metadata))
}

#' @export
.hintze.2013 <- function(...){
    data <- read.csv("http://www.sciencedirect.com/science/MiamiMultiMediaURL/1-s2.0-S1433831913000218/1-s2.0-S1433831913000218-mmc1.txt/273233/html/S1433831913000218/6bd947d6c0ccb7edd11cd8bf73648447/mmc1.txt", sep=";", as.is=TRUE, dec=",")
    data$metadata <- seq_len(nrow(data))
    data$name <- sapply(strsplit(tolower(sanitize_text(data$name)),split=" "), function(x) paste(x[1:2], collapse="_"))
    data <- data[,!names(data) %in% c("comment","family","citation_total","citation_prop_ane","citation_prop_dyso","citation_prop_endo","citation_prop_epi","citation_prop_hem","citation_prop_hydro","citation_prop_other")]
    return(.df.melt(data, "name"))
}

#' @export
.husak.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.2d960","HusakFergusonLovern_Anolis_training_diet_alldata.csv"))
    data <- data[,-1]
    data$species <- 'anolis_carolinensis'
    names(data) <- c('sex','training','diet','reproductive_state_binary','pre_snout_vent_length','post_snout_vent_length','snout_vent_length_difference','snout_vent_length_growth','pre_mass','post_mass','mass_difference','growth_mass','pre_endurance','post_endurance','phytohaemagglutinin_response','bacterial_killing_ability','liver_mass','ventricle_mass','atria_mass','heart_mass','bite_force','testosterone_levels','corticosterone_levels','haematocrit','fat_mass','species')
    metadata <- data[,1:4]
    data <- data[,-c(1:4)]
    units <- c(rep('mm',4), rep('g',4),rep('sec',2),'mm','%',rep('mg',4),'N',rep('ng/mL',2),'NA','mg')
    return(.df.melt(data, "species", units,metadata))
}

#' @export
.ingram.2016 <- function(...){
    color <- read.csv(suppdata("10.5061/dryad.9vr0c", "Dewlap_data_archive.csv"))
    metadata <- color[,c(3)]
    metadata <- as.data.frame(metadata)
    colnames(metadata) <- 'sample_size'
    color <- color[,-c(1,3,4,7:11)]
    units <- c('cm', 'cm^2')
    color$Species <- paste0("anolis_",color$Species)
    colnames(color) <- tolower(colnames(color))
    return(.df.melt(color, "species", units, metadata))
}

#' @export
.jennings.2015 <- function(...){
    data <- read.csv(suppdata("E096-226", "SIA_N_C_Atlantic_marine_fishes_squids_20150105_v1.csv", "esa_archives"))
    metadata <- data[,c("record", "year", "DOY", "latitude", "longitude", "sea")]
    data <- data[,!names(data) %in% names(metadata)]
    units <- c(NA, "g", "%", "%", "?", "?")
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.jennings.2016a <- function(...){
    data = read.csv(suppdata("10.5061/dryad.m23g6","spiders.csv"))
    data$species = rep("sosippus_floridanus", nrow(data))
    vars = c("web_area","web_height","diff_trich","diff_trap","sum_trap", "species")
    units = c("cm^2","cm","#/cm^2","cm^2","cm^2")
    metadata = data[,c(1:7,10)]
    data = data[,-c(1:7,10)]
    colnames(data) = vars
    data = .df.melt(data, "species", units, metadata)
}
#' @export
.jennings.2016b <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.m23g6", "sundews.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
    species <- rep(c("drosera_capillaris"), nrow(data))
    data <- data.frame(species, data)
    metadata <- data[,c(2:6)]
    data <- data[,-c(2:6)]
    units <- c("#", "sqrt(#)", "cm^2", "cm", "#", "#/cm^2", "cm^2","cm^2", NA, NA, "#", NA, NA)
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}
.jennings.2016c <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.m23g6", "toads.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
    species <- rep(c("anaxyrus_quercicus"), nrow(data))
    data <- data.frame(species, data)
    metadata <- data[,c(2:6)]
    data <- data[,-c(2:6)]
    units <- c("cm^2", "cm", "#", "sqrt(#)", "g",  "g", "#", "#/cm^2", "cm^2","cm^2", NA, NA, "#", NA, NA)
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}

#' @export
.jones.2009 <- function(...){
    data <- read.delim(suppdata("E090-184", "PanTHERIA_1-0_WR05_Aug2008.txt", "esa_archives"))
    for(i in 1:ncol(data))
        data[data[,i]==-999 | data[,i]=="-999",i] <- NA
    metadata <- data[,c("MSW05_Order","MSW05_Family","MSW05_Genus","MSW05_Species","References")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data) <- c("MSW05_Binomial", "X1.activity_cycle", "mass", "forearm_length", "head_body_length", "age_eye_open", "age_birth", "basal_metabolic_rate", "basal_metabolic_rate", "diet_breadth", "dispersal_age", "gestation_length", "habitat_breadth", "home_range", "individual_home_range", "interbirth_interval", "litter_size", "litters_per_year", "max_longevity", "neonate_mass", "neonate_body_length", "population_denisty", "population_group_size", "sexual_maturity_age", "social_group_size", "teat_number", "terrestrial", "trophic_level", "weaning_age", "weaning_mass", "weaning_body_length", "mass", "litters_per_year", "neonate_mass", "weaning_mass", "global_range_area", "max_latitude", "min_latitude", "mid_latitude", "max_longitude", "min_longitude", "mid_longitude", "min_human_population_density", "mean_human_population_density", "human_population_5p_n", "human_population_density_change", "mean_precipitation", "mean_temperature", "mean_actual_evapotranspiration", "mean_potential_evapostranspiration")
    units <- c(NA, "g", "mm", "mm", "days", "days", "mL/hour", "g", NA, "days", "days", NA, "km^2", "km^2", "days", "#", "#/year", "months", "g", "mm", "km^2", "#", "days", "#", "#", NA, NA, "days", "g", "mm", "g", "#", "g", "g", "km^2", "?", "?", "", "?", "?", "?", "n/km^2", "#/km^2", "#/km^2", ":", "mm", "0.1degC", "mm", "mm")
    data <- .df.melt(data, "MSW05_Binomial", units=units)
    
    data$character$species <- tolower(gsub(" ","_", data$character$species))
    data$numeric$species <- tolower(gsub(" ","_", data$numeric$species))
    return(data)
}

#' @export
.kamath.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.9vk07","KamathLososEvol_AnolissagreiMorphAvg.csv"))
    data <- data[,-c(1)]
    data$species <- 'anolis_sagrei'
    metadata <- data[,c(1)]
    metadata <- as.data.frame(metadata)
    colnames(metadata) <- 'sex'
    data <- data[,-c(1)]
    colnames(data) <- c('snout_vent_length','average_lamellae_count','toepad_area','limb_length','species')
    units <- c('mm','NA','mm^2','mm',NA)
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.kamilar.2015 <- function(...){
    data <- as.data.frame(read_excel(
        suppdata("10.5061/dryad.pb74r","Kamilar&Tecot-AppendixS1.xlsx"),
        skip=1, col_names=c("species","order","body_mass","brain_mass",
                            "anterior_lobe_volume","pituitary_volume",
                            "posterior_lobe_volume","fetal_growth_rate",
                            "postnatal_growth_rate","gestation_length",
                            "neonate_body_mass","litter_size","weaning_age",
                            "weaning_body_mass","max_longevity")
    ))
    meta <- data[,"order",drop=FALSE]
    data <- subset(data, select=-c(order))
    units <- c("g","g","mm^3","mm^3","mm^3","g/day","g/day","day","g","individuals","day","g","years")
    data$species <- tolower(gsub(" ", "_", (data$species)))
    data <- .df.melt(data, "species", units=units, metadata=meta)
    return(data)
}

#' @export
.kefi.2016 <- function(...){
    data <- as.data.frame(read_excel(
        suppdata("10.5061/dryad.b4vg0","chilean_metadata.xls"),
        col_names=c("id", "species", "body_mass", "sessile_mobile", "cluster",
                    "shore_height_conservative", "shore_height_C_ordinal",
                    "shore_height_C_breadth", "shore_height_2_restrictive",
                    "shore_height_R_ordinal", "shore_height_r_breadth",
                    "phyllum", "subphyllum", "trophic")
    ))
    data$species <- tolower(gsub(" ","_", data$species))
    metadata <- data[,c(1, 6, 9, 12:13)]
    data <- data[,-c(1, 6, 9, 12:13)]
    units <- c("?",NA,"?", "?","?","?","?",NA)
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}

#' @export
.kelt.2015 <- function(...){
    data <- read.delim(suppdata("E096-155","Mammal_Home_Ranges.txt", "esa_archives"))
    names(data) <- c("taxon","order","family","trophic_group","body_mass","home_range","references","notes")
    data$taxon <- tolower(gsub(" ", "_", data$taxon))
    metadata <- data[,c("order","family","references","notes")]
    data <- data[,!names(data) %in% names(metadata)]
    return(.df.melt(data, "taxon", c(NA,"log10(g)","log10(ha)"), metadata))
}

#' @export
.kissling.2014 <- function(...){
    data <- read.delim(suppdata("10.5061/dryad.6cd0v","MammalDIET_v1.0.txt"), as.is=T)
    data$species <- tolower(paste(data$Genus,data$Species,sep="_"))
    # Subsetting to only include non-interpolated data (estimates generated from taxonomy higher than species)
    data <- data[data$FillCode=="0",]
    data <- unique(data[which(!is.na(data$species)),])
    meta <- data[,c("Order","Family","Genus","TaxonomicNote","DataSource")]
    data <- data[,!names(data) %in% c("TaxonID","Order","Family","Genus","Species","TaxonomicNote","FillCode","DataSource")]
    units <- rep(NA,ncol(data)-1)
    data <- .df.melt(data, "species", metadata=meta)
    return(data)
}

#' @export
.klomp.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.0g3d5","draco_comparative data.csv"))
    names(data) <- tolower(gsub("\\.", "_", names(data)))
    names(data) <- c('species', 'female_dewlap_area', 'male_dewlap_area', "sexual_dimorphism_in_dewlap_area", "sexual_size_dimorphism_svl", "sexual_dichromatism_chromatic_contrast",'sexual_dichromatism_achromatic_contrast', 'female_dewlap_chromatic_contrast', 'male_dewlap_chromatic_contrast','female_dewlap_achromatic_contrast', 'male_dewlap_achromatic_contrast', 'light_level_auc','predation_category')
    data$species <- tolower(gsub(" ", "_", (data$species)))
    data$species <- gsub("\\(", "", (data$species))
    data$species <- gsub("\\)", "", (data$species))
    units <- c(rep('cm^2',2), 'cm^2 (nl)', 'cm', rep('JND', 6), 'nl', NA)
    data <- .df.melt(data, "species", units=units)
    return(data)
}

#' @export
.kolbe.2011 <- function(...){
    data <- read.table(suppdata("10.5061/dryad.1d24c","21%20species%20means.txt"),header=TRUE,sep = '\t')
    names(data) <- tolower(gsub("\\.", "_", names(data)))
    data$species <- tolower(gsub('\\. ', 'nolis_', data$species))
    names(data)[14] <- "femur_length"
    metadata <- data[,2,drop=FALSE]
    names(metadata) <- 'ecomorph'
    data <- data[,-2]
    units <- c(rep('mm',ncol(data)))
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.kraft.2015a <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.69ph0", "BCI%20data.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
    data$genus_species <- tolower(data$genus_species)
    metadata <- data[,c(2:4)]
    data <- data[,-c(2:4)]
    units <- c(rep("J/m^2",4), "g", "um", "um", "#", "m", "density", "#", NA, NA, NA)
    data <- .df.melt(data, "genus_species", units, metadata)
    return(data)
}
#' @export
.kraft.2015b <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.69ph0", "Kraft_et_al%20data.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
    data <-  data[1:166,-19]
    data$Species <- tolower(gsub(" ", "_", data$Species, ignore.case = TRUE))
    metadata <- data[,c(2:3)]
    data <- data[,-c(2:3)]
    units <- c(rep("mm",3), "mm^3", rep("mm",3), rep("J/m^2",2), "%", "%", "#", "m", "#", "density", NA, NA)
    data <- .df.melt(data, "Species", units, metadata)
    return(data)
}
.kuo.2014 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.p8740","personality_date1.csv"))
    metadata <- data[,5]
    metadata <- as.data.frame(metadata)
    colnames(metadata) <- 'diet'
    data <- data[,-c(1:5,16)]
    colnames(data) <- c('snout_vent_length','latency_to_explore','perch_inspected','percent_time_newzone','percent_time_onperch','bite_force','tail_diameter', 'personality_principal_component_1','personality_principal_component_2','transformed_principal_component_1')
    data$species <- 'anolis_sagrei'
    units <- c('cm','min',NA,rep('%',2), 'N','cm',rep(NA,3))
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.lagisz.2013 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.kf490","Lagisz_DATATABLE.csv"), stringsAsFactors=FALSE)
    meta <- data[,c("Refseq","NCBI_taxID","class","size_sources")]
    data <- data[,!names(data) %in% c("ord","Refseq","NCBI_taxID","genus","species","class","log_volume_male", "log_volume_female","size_sources")]
    names(data)[1] <- "species"
    data$species <- tolower(gsub(" ","_", data$species))
    units <- c(rep(NA,3),rep("mm",4),rep("mm^3",2), rep(NA,16))
    data <- .df.melt(data, "species", units=units, metadata=meta)
    return(data)
}

#' @export
.lawson.2015<-function(...){
    data<-read.csv(suppdata("10.5061/dryad.72h45","riparian wood density data.csv"))
    metadata<-data[,c(1,3,5,7,9)]
    data<-data[,-c(1,3,5,7,9)]
    units<-rep("g/cm^3",3)
    data<-.df.melt(data,"species",units=units,metadata=metadata)
}

#' @export
.lessard.2016 <- function(...){
    data <- read.delim(suppdata("10.5061/dryad.t897q", "hummer_traits_Lessard.txt"), sep = " ", row.names = NULL)
    species <- tolower(data[,1])
    data <- data.frame(species, data[,-c(1)])
    units <- c("g", "mm", "degC", "?", "range")
    return(.df.melt(data, "species", units))
}

#' @export
.limpens.2014a <- function(...){
    data <- as.data.frame(read_excel(suppdata("doi:10.5061/dryad.926nd", "Motherfile.xls")))
    species <- gsub("gla", "picea_glauca", data$Species)
    species <- gsub("mar", "picea_mariana", species)
    species <- gsub("rub", "picea_rubens", species)
    species <- gsub("sit", "picea_sitchensis", species)
    species <- gsub("ban", "pinus_banksiana", species)
    species <- gsub("nig", "pinus_nigra", species)
    species <- gsub("syl", "pinus_slyvestris", species)
    data <- data.frame(species, data[,-3])
    metadata <- data[,c(2:4)]
    data <- data[,-c(2:4)]
    units <- c(rep("cm",2), "ln(mm/day)", "mg", rep("cm",2), "ln(mm/day)", "mg", "cm/cm", "mg/mg", "mg", rep(NA,3))
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}
.limpens.2014b <- function(...){
    data <- as.data.frame(read_excel(suppdata("doi:10.5061/dryad.926nd", "traitsandsurvival.xlsx"), na = c("","NA")))
    species <- gsub("gla", "picea_glauca", data$Species)
    species <- gsub("mar", "picea_mariana", species)
    species <- gsub("rub", "picea_rubens", species)
    species <- gsub("sit", "picea_sitchensis", species)
    species <- gsub("ban", "pinus_banksiana", species)
    species <- gsub("nig", "pinus_nigra", species)
    species <- gsub("syl", "pinus_slyvestris", species)
    data <- data.frame(species, data[,c(-1,-3)])
    metadata <- data[,c(2,4)]
    data <- data[,-c(2,4)]
    
    units <- c(rep("cm",2), "mm/day", "mg", rep("cm",2), "mm/day", "mg", "cm/cm", "mg/mg", "mg", rep(NA,3))
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.lislevand.2006 <- function(...){
    data("shorebird", package="caper", envir=environment())
    names(shorebird.data) <- c("species","body_mass_male","body_mass_female","egg_mass","clutch_size","mating_system")
    units <- c("g","g","f",NA,NA)
    shorebird.data$species <- tolower(gsub(" ", "_", (shorebird.data$species)))
    return(.df.melt(shorebird.data, "species", units=units))
}

# Out of commission until https://github.com/willpearse/suppdata/issues/22 is closed
# #' @export
if(FALSE){
    .lu.2016a <- function(...){
    data <- read.csv(unzip(suppdata("10.1002/ecy.1600", 2), "GCReW_Allom_General_Data.csv"))
    data$binomial <- data$Spp_Orig
    data$binomial <- gsub("PHAU", "phragmites_australis", data$binomial)
    data$binomial <- gsub("SCAM", "schoenoplectus_americanus", data$binomial)
    data$binomial <- gsub("SPAL", "spartina_alterniflora", data$binomial)
    data$binomial <- gsub("SPCY", "spartina_cynosuroides", data$binomial)
    data$binomial <- gsub("TYAN", "typha_angustifolia", data$binomial)
    data$binomial <- gsub("AMCA", "amaranthus_cannabinus", data$binomial)
    data$binomial <- gsub("ATPA", "atriplex_patula", data$binomial)
    data$binomial <- gsub("KOVI", "kosteletzkya_virginica", data$binomial)
    data$binomial <- gsub("POHY", "polygonum_hydropiper", data$binomial)
    data$binomial <- gsub("SOSE", "solidago_sempervirens", data$binomial)
    data$binomial <- gsub("IVFR", "iva_frutescens", data$binomial)
    data$Width[data$Width==-99] <- NA
    metadata <- data[,c("Species","Spp_Orig","Decile","ID","Year")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data)[1] <- "height"
    return(.df.melt(data, "binomial", c("cm","mm","g"), metadata))
}
.lu.2016b <- function(...){
    data <- read.csv(unzip(suppdata("10.1002/ecy.1600", 2), "GCReW_Allom_Other_Data.csv"))
    data$binomial <- data$Species
    data$binomial <- gsub("PHAU", "phragmites_australis", data$binomial)
    data$binomial <- gsub("SCAM", "schoenoplectus_americanus", data$binomial)
    data$binomial <- gsub("SPAL", "spartina_alterniflora", data$binomial)
    data$binomial <- gsub("SPCY", "spartina_cynosuroides", data$binomial)
    data$binomial <- gsub("TYAN", "typha_angustifolia", data$binomial)
    data$binomial <- gsub("AMCA", "amaranthus_cannabinus", data$binomial)
    data$binomial <- gsub("ATPA", "atriplex_patula", data$binomial)
    data$binomial <- gsub("KOVI", "kosteletzkya_virginica", data$binomial)
    data$binomial <- gsub("POHY", "polygonum_hydropiper", data$binomial)
    data$binomial <- gsub("SOSE", "solidago_sempervirens", data$binomial)
    data$binomial <- gsub("IVFR", "iva_frutescens", data$binomial)
    data$Width[data$Width==-99] <- NA
    metadata <- data[,c("Species","ID","Year")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data)[1] <- "height"
    return(.df.melt(data, "binomial", c("cm","mm","g"), metadata))
}
.lu.2016c <- function(...){
    data <- read.csv(unzip(suppdata("10.1002/ecy.1600", 2, cache=FALSE), "GCReW_Allom_SCAM_Data.csv"))
    data$binomial <- "schoenoplectus_americanus "
    data$Width[data$Width==-99] <- NA
    metadata <- data[,c("Species","ID","Expt","ExptAge","Year","Community","Chamber","Quadrat","InOut","CO2","N","Treatment")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data)[1] <- "height"
    data$Cut_Stem <- as.logical(data$Cut_Stem)
    return(.df.melt(data, "binomial", c("cm","cm","mm","g","g","g",NA), metadata))
}
    }

#' @export
.lupold.2013 <- function(...){
    data <- read.delim(suppdata("10.5061/dryad.qj811","dataset.txt"), as.is=T)
    data$Species[data$Species=="Aepiceros_melampus"] <- "Aepyceros_melampus"
    data$species <- tolower(gsub(" ","_", data$Species))
    meta <- data[,c("Order","Family","N","ReferenceEjaculateData","ReferenceBodyMass.TestisMass","RererenceBasalMetabolicRate")]
    data <- data[,!names(data) %in% c("Species","Order","Family","N","ReferenceEjaculateData","ReferenceBodyMass.TestisMass","RererenceBasalMetabolicRate", "BodyMass_forBMR_.in_g")]
    colnames(data)[6:8] <- c("body_mass","CombinedTestesMass","BasalMetabolicRate")
    units <- c(NA, NA, NA, NA, NA, "g","g","mL/h")
    data <- .df.melt(data, "species", units=units, metadata=meta)
    return(data)  
}

#' @export
.madin.2016 <- function(...){
    # Currently performs aggregation to species means as original data is in long format with no unique identifiers per row
    data <- read.csv(unzip(suppdata("10.6084/m9.figshare.2067414",1))[1], stringsAsFactors=TRUE)
    data$species <- tolower(gsub(" ","_", data$specie_name))

    # Aggregation Code from https://coraltraits.org/procedures#reshaping-data-downloads
    my_aggregate_rules <- function(x) {
        if (length(x) > 1) {               # Does a species by trait combination have more than 1 value?
            x <- type.convert(x, as.is=TRUE)
            if (is.character(x)) {
                return(x[1])                   # If values are strings (characters), then return the first value
            } else {
                return(as.character(mean(x)))  # If values are numbers, then return the mean (converted back to numeric)
            }
        } else {
            return(x)                        # If a species by trait combination has 1 value, then just return that value 
        }
    }
    
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)
    
    traits <- data[,c("species","trait_name","value", "standard_unit")]
    traits$standard_unit[traits$standard_unit%in%c("cat","dimensionless","text","bin","id","units")] <- NA  
    units <- unique(traits[,c("trait_name","standard_unit")])
    units <- units[with(units, order(trait_name)),]
    traits_with_multiple_units <- unique(units$trait_name[duplicated(units$trait_name)])
    # Some traits are reported in different units per species.
    # Removing for now - until unit lookup table is available
    # Some of these are important traits (ex. growth rate), so it would be best to keep them in
    traits <- traits[!traits$trait_name%in%traits_with_multiple_units,]
    units <- units$standard_unit[!units$trait_name%in%traits_with_multiple_units]
    data <- dcast(traits, species ~ trait_name, value.var="value", fun.aggregate=my_aggregate_rules, fill="")
    data[data == ""] <- NA
    # dcast is turning numerics into characters
    for(i in seq_len(ncol(data)))
        data[,i] <- type.convert(data[,i], as.is=TRUE)
    data <- .df.melt(data, "species", units=units)
    return(data)
}

#' @export
.martin.2014 <- function(...){
    data <- read.table(suppdata("10.5061/dryad.j4n82","Sexually%20antagonistic%20association%20between%20paternal%20phenotype%20and%20offspring%20viability%20reinforces%20total%20selection%20on%20a%20sexually%20selected%20trait%20-%20id%20data.txt"),sep='\t',header=TRUE)
    metadata <- data[,c(1:3,7:9)]
    data <- data[,-c(1:3,7:9)]
    units <- c('cm','kg','kg')
    data$Species <- 'O_canadensis'
    return(.df.melt(data,"Species",units=units, metadata))
}

#' @export
.martin.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.4t3r6","Martin et al. Functional Ecology.txt"), sep = "\t")
    data$species <- rep("Coffea_arabica", nrow(data))
    metadata <- data[,c(1:10,12)]
    data <- data[-c(1:10,12)]
    units <- c("#","cm","cm","mm","mm","#","#","#","m^2","mm","m","mg","g","#","#","#",
               "#","#","#","#","#","#","#","#","g/m^2","#","g/cm^3","#","#","g/cm^2",
               "#","#","#","#","#", NA)
    data <- .df.melt(data, "species", units = units, metadata)
    return(data)
}

#' @export
.marx.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.m88g7","DRYAD2_SJtraits.csv"))
    names(data)[3:7] <- c("Seed.Mass","Maximum.Height","","Leaf.Size","Leaf.Nitrogen")
    units <- c(NA,"mg","m","cm^2/g","cm^2","specific_leaf_area")
    metadata <- data[,2,drop=FALSE]
    return(.df.melt(data, "Species", units=units, metadata=metadata))
}

#' @export
.mccullough.2015<-function(...){
    file<-tempfile()
    download.file("http://www.sciencedirect.com/science/MiamiMultiMediaURL/1-s2.0-S0003347215003103/1-s2.0-S0003347215003103-mmc1.xlsx/272524/html/S0003347215003103/0bb76368c8bbec26cf11858140abe3e8/mmc1.xlsx",file, quiet=TRUE)
    data<-as.data.frame(read_xlsx(file))
    units<-c("mm","mm")
    data<-.df.melt(data,"Species",units=units)
}

#' @export
.mesquita.2016 <- function(...){
    data <- read.delim(suppdata("10.5061/dryad.55610", "Data%20%28revised%29.txt"), header=TRUE)
    data <- data[,-c(2:4,7,9:10,12:13,17,25)]
    data$Species <- tolower(gsub(" ", "_", (data$Species)))
    colnames(data) <- c('species','longitude','latitude','average_female_adult_weight','mean_female_svl_adults','female_svl_at_maturity','offspring_svl','mean_clutch_size','mode_of_reproduction','clutch_per_year','clutch_frequency','relative_clutch_mass','foraging_mode','distribution','prefered_habitat_type')
    metadata <- data[,c(2:3,14:15)]
    data <- data[,-c(2:3,14:15)]
    data$mode_of_reproduction <- tolower(data$mode_of_reproduction)
    data$foraging_mode <- tolower(gsub(' ', '_', data$foraging_mode))
    units <- c('g', rep('mm',3),rep(NA,3),'g',rep(NA,2))
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.molinari.2014<-function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.50hd2","Dryad_Final.xlsx")))
    vars = c("species", "native", "life_form","abundance_2007","abundance_2008", "peak_flowering", "height", "seed_mass", "specific_leaf_area")
    colnames(data) = vars
    data$species = tolower(gsub(" ","_",data$species))
    metadata = data[,c(4:5)]
    data = data[,-c(4:5)]
    units = c(NA,NA,"month","m","mg","cm^2/g")
    data = .df.melt(data, "species",units,metadata)
    return(data)
}

#' @export
.myhrvold.2015 <- function(...){
    data <- read.csv(suppdata("E096-269","Data_Files/Amniote_Database_Aug_2015.csv", "esa_archives", cache=FALSE))
    for(i in seq_len(ncol(data)))
        data[data[,i]==-999 | data[,i]=="-999",i] <- NA
    metadata <- data[,c("class","order","family","genus","species","subspecies","common_name")]
    species <- ifelse(is.na(data$subspecies), "", data$subspecies)
    data$binomial <- tolower(paste(data$genus, data$species, data$subspecies, sep="_"))
    data <- data[,!names(data) %in% names(metadata)]
    units <- c("days", "#", "#/year", "g", "years", "days", "days", "g", "g", "g", "days", "days", "years","days", "years", "g", "g", "g", "mm", "mm", "g", "cm", "cm", "cm", "cm", "cm", "g", "cm", "days")
    return(.df.melt(data, "binomial", units, metadata))
}

#' @export
.munoz.2014 <- function(...){
    data <- read.table(suppdata("10.5061/dryad.q39h2", "Munoz_2014_AmNat_Dryad.txt"), header=T)
    names(data) <- c('species', 'clade', 'island','latitude','longitude','elevation','snout_vent_length')
    data$island <- tolower(data$island)
    metadata <- data[,c(3:6)]
    data <- data[,-c(2:6)]
    data$species <- gsub('A.','anolis_', data$species)
    units <- 'mm'
    return(.df.melt(data, "species", units,metadata))
}

#' @export
.nandy.2013 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.r54m2","Nandy et al. 13-0268.R2_data.xlsx")))
    colnames(data) <- c('selection_regime', 'block', 'total_mass_dry')
    names(data) <- tolower(names(data))
    data$species <- 'drosophila_melanogaster'
    metadata <- data[,c(1:2)]
    data <- data[,-c(1:2)]
    units <- c('mg')
    return(.df.melt(data, "species", units,metadata))
}

#' @export
.olli.2015 <- function(...){
    data <- read.table(suppdata("10.5061/dryad.d0826", "fd.txt"), header = T, sep = '\t')
    for(i in c(1:9,11))
        data[,i] <- as.logical(data[,i])
    units <- c(rep('NA',9), "micrometer", NA)
    data$species <- rownames(data)
    data <- .df.melt(data, "species", units=units)
    return(data)
}

#' @export
.paquette.2015 <- function(...){
    # They zipped the zip when they transferred the SI to the new system,
    # - hence unzip(unzip(...))
    data <- as.data.frame(read_xls(unzip(unzip(suppdata("10.1002/ece3.1456", 1)))[2], sheet=2, na=c("","NA")))
    #WHAT IS WRONG HERE
    data <- data[7:nrow(data),2:8]
    colnames(data) <- c("species","occurrence","average_maximum_height","wood_density","seed_mass","shade_tolerance","nitrogen_per_leaf_mass_unit")
    data <- data[, names(data)!="occurrence"]
    units <- c("m","g/cm^3","mg",NA,"%")
    data$species <- tolower(gsub(" ","_", data$species))
    for(i in 2:6)
        data[,i] <- as.numeric(data[,i])
    allNAs <- apply(data[2:6], 2,is.na)
    data <- data[rowSums(allNAs)<5,]
    return(.df.melt(data, "species", units=units))
}

#' @export
.pearse.2014 <- function(...){
    data <- read.csv(suppdata("10.6084/m9.figshare.979288", 4), sep = ",", na.strings = c("","NA"))
    species <- rep(c("Carcinus_maenas"), nrow(data))
    data <- data.frame(species, data)
    metadata <- data[,c(2:3,8:15)]
    data <- data[,-c(2:3,8:15)]
    units <- c(NA, "mm","#","#")
    data <- .df.melt(data, "species", units=units, metadata)
    return(data)
}

#' @export
.perez.2014 <- function(...){
    data <- as.data.frame(read_excel(suppdata('10.5061/dryad.d61jk/1', 'leaf%20traits%2c%20foliar%20freezing%20resistance%2c%20climatic%20niche.xlsx'), sheet=1))
    data$Species <- tolower(gsub(" ", "_", data$Species, ignore.case = TRUE))
    metadata <- data[,c(2:4)]
    data <- data[,-c(2:4)]
    units <- c("cm^2", "gr/m^2", rep("N/mm^2",2), "NA", "latitude", "longitude")
    return(.df.melt(data, "Species", units, metadata))
}

#' @export
.petry.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.1cf8p","PollenMovement.csv"))
    data$species <- rep("valeriana_edulis", nrow(data))
    metadata <- data[,1,drop=FALSE]
    data <- data[,-1]
    units <- c("#","#","#","#","#","#","#","#","#","#","m","m","m","m","m","m")
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.philipson.2014 <- function(...){
    data <- read.delim(suppdata("10.5061/dryad.qn814", "EcologyEvolution_IntensivePlotsData_forDryad.txt"), sep = ",", as.is = TRUE)
    species <- tolower(gsub(" ", "_", data$Species, ignore.case = TRUE))
    data <- data.frame(species, data[,-1])
    metadata <- data[,c(2, 9:12)]
    data <- data[,-c(2, 9:12)]
    units <- c("%", "mm", "mm", "cm", "days", rep(NA, 6))
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}

#' @export
.philipson.2016<-function(...){
    data<-read.delim(suppdata("10.5061/dryad.qn814","EcologyEvolution_IntensivePlotsData_forDryad.txt"),sep = ",")
    metadata<-data[,c(2,7:12)]
    data<-data[,c(1,3:6)]
    units<-c("%","mm","mm","cm")
    data<-.df.melt(data,"Species",units=units,metadata=metadata)
}

# Out of commission until https://github.com/willpearse/suppdata/issues/22 is closed 
# #' @export
if(FALSE) .pfautsch.2016 <- function(...){
    first <- read.csv(unzip(suppdata("10.1890/16-0147.1", 1), "Eucalyptus_vessel_anatomy_800cm.csv"), as.is=TRUE)
    first$measurement_height <- "800cm"
    names(first)[19] <- "diameter_at_breast_height"
    second <- read.csv(unzip(suppdata("10.1890/16-0147.1", 1), "Eucalyptus_vessel_anatomy_130cm.csv"), as.is=TRUE)
    second$measurement_height <- "130cm"
    names(second)[19] <- "diameter_at_breast_height"
    data <- rbind(first, second)
    data$vessel_area_per_image <- as.numeric(data$vessel_area_per_image)
    names(data)[5:6] <- c("latitude", "longitude")
    metadata <- data[,c("location","country","state","latitude","longitude","elevation","tree","image","measurement_height")]
    data <- data[,!names(data) %in% names(metadata)]
    units <- c("#", "#", "um^2", "um^2", "um^2", "# cm^-2", "%", "um", "um", "cm", "cm", "cm", "g/cm^3", "g/cm^3", "degC", "degC", "mm", "mm", "mm", "mm", "mm", "mm", "?")
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.pigot.2015 <- function(...){
    # Will says: Need to leave the URI in there; potentially fulltext problem?
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.fd986","Database%20S1%20Pigot%2c%20Trisos%20and%20Tobias.xls")))
    data <- data[,-c(26:28)]
    for(i in 12:15)
        data[,i] <- as.logical(data[,i])
    names(data)[3:11] <- c("Min.Elevation","Max.Elevation","log.Bill.Length","log.Bill.Width","log.Bill.Depth","log.Tarsus.Length","log.Kipps.Distance","log.Wing.Length","log.Tail.Length")
    names(data)[26] <- "Museum.Institution.codes"
    units <- c(NA,"m","m",rep('mm',7),rep('NA',4),rep('%',10),NA)
    metadata <- data[,c(1,3:4,26)]
    data <- .df.melt(data, "Binomial", units=units, metadata=metadata)
    return(data)
}

#' @export
.price.2014 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.r3n45", "Price%20et%20al%20Data%20for%20Dryad.xlsx")))
    names(data)[7:18] <- c("growth_form","specific_leaf_area","leaf_mass_area","leaf_life_span","N_content_per_unit_mass","N_content_per_unit_area","P_content_per_unit_mass","P_content_per_unit_area","phototsynthetic_capacity_per_mass","phototsynthetic_capacity_per_area","leaf_size","height") 
    units <- c(rep(NA,6), "mm^2/mg ", "g/m^2", "month", "%", "g/m^2", "%", "g/m^2", "nmol/g s", "micromol/m^2 s", "cm^2", "m")
    metadata <- data[,c(1:2,4:7)]
    data <- data[,!names(data) %in% names(metadata)]
    data$leaf_size <- as.numeric(as.character(data$leaf_size))
    return(.df.melt(data, "genus species", units, metadata))
}

#' @export
.plourde.2015 <- function(...){
    data <- read.delim(suppdata("10.5061/dryad.sv181", "complete.individual.data.txt"))
    data$species <- with(data, tolower(paste(genus, species, sep="_")))
    data$genus <- NULL
    metadata <- data[,c(1:2,13,15,30:34,38)]
    data <- data[,-c(1:2,13,15,30:34,38)]
    units <- c("g", "cm^3", rep("g",2), "cm^3", rep("g",2), "cm^3", "g", "cm", rep("cm", 7), rep("g/cm^3",3), rep("cm^2", 4),  "?", "?", "%", rep(NA, 6), "r^2", rep(NA, 3))
    return(.df.melt(data, "species", units, metadata))
}

#' @export
.rahman.2013 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.2v68d","Data%20for%20Dryad.xls")))
    data$Species <- "P_reticulata"
    metadata <- data[,c(1:7,11,12,15,16,19,20)]
    data <- data[,-c(1:7,11,12,15,16,19,20)]
    units <- c('log10','log10','log10',NA,'mm^2',NA,'mm^2','log10','um')
    return(.df.melt(data,"Species",units=units, metadata))
}

#' @export
.rojas.2013 <- function(...){
    traits.morph <- read.delim(suppdata("10.5061/dryad.f435k","morphological_variables.txt"), as.is=T)
    traits.diet <- read.delim(suppdata("10.5061/dryad.f435k","ecological_variables.txt"), as.is=T)
    data <- merge(traits.morph,traits.diet, by.x="taxon",by.y="taxon")
    colnames(data)[1:4] <- c("species","n_individuals_sampled","body_mass","cranial_volume")
    meta <- data[,"n_individuals_sampled",drop=FALSE]
    data <- data[,names(data)!="n_individuals_sampled"]
    data$species <- tolower(gsub(" ","_", data$species))
    units <- c("g","mm^3", rep(NA,6))
    data <- .df.melt(data, "species", units=units, metadata=meta)
    return(data)
}

#' @export
.rutschmann.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.qd5gj", "Rutschmann_AdaptationofPhenologyacrossPopulations_dataset.csv"),sep=';')
    names(data) <- tolower(gsub("X\\.", "", names(data)))
    data <- data[,-c(1:3)]
    data$species <- 'zootoca_vivipara'
    colnames(data) <- c('altitude','snout_vent_length','total_mass','parturition_year', 'parturition_day', 'mid_gestation_temperatures', 'forest_cover_coefficient','mountain_chain','species')
    data$mountain_chain <- tolower(data$mountain_chain)
    metadata <- data[,c(1,4:8)]
    data <- data[,-c(1,4:8)]
    units <- c('mm','g')
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}

#' @export
.sherratt.2013<-function(...){
    data <- read.csv(suppdata("10.5061/dryad.hk2v3","Ontogenetic allometry data.csv"))
    metadata <- data[,c(3,5)]
    data <- data[,-c(2,3,5)]
    colnames(data) <- c('species', 'snout_vent_length', 'face_length', 'body_length')
    data$species <- tolower(gsub('A. ', 'anolis_', data$species))
    colnames(metadata) <- c('age_class','sex')
    metadata$sex <- tolower(gsub('Male', 'm', metadata$sex))
    metadata$sex <- tolower(gsub('Female', 'f', metadata$sex))
    units <- c("mm","mm","mm")
    return(.df.melt(data,'species', units, metadata))
}

#' @export
.shibata.2016a <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.rj480","FEShibataDataForAnalyses.xls")))
    metadata <- data[,14,drop=FALSE]
    data <- data[,-14]
    units <- c(rep('%',6), NA, NA, 'cm^2', 'g m^-2', 'MN/m^2', '%', 'g/cm^3')
    return(.df.melt(data,"Species",units=units, metadata))
}
#' @export
.shibata.2016b <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.rj480","FEShibataDataForAnalyses.xls"), sheet=2))
    metadata <- data[,15,drop=FALSE]
    data <- data[,-15]
    units <- c(NA, NA, rep('cm^2',3),'g/cm^3', 'g m^-2', 'MN/m^2', rep('%',5), NA)
    return(.df.melt(data,"Species",units=units, metadata))
}

#' @export
.simmons.2014 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.42pg7", "Simmons%20%26%20Buzatto%202013.xlsx")))
    species <- rep(c("Onthophagus_taurus"), nrow(data))
    data <- data.frame(species, data)
    metadata <- data[,11,drop=FALSE]
    data <- data[,-c(10:11)]
    units <- c(rep(NA,2), "mg", "mm", "mg", rep(NA, 5))
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}

#' @export
.simpson.2015 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.2c506", "Plant trait data.csv"))
    metadata <- data[,c(2:3)]
    data <- data[-c(2:3)]
    data$Species <- gsub(" ","_",data$Species)
    data <- .df.melt(data, "Species", units = c(NA, NA, "m", "m", "g", "g", "g/g", "g/cm", "SA/vol", "kJ/g"), metadata)
    return(data)
}

# Out of commission until https://github.com/willpearse/suppdata/issues/22 is closed
# #' @export
if(FALSE)
    .stephens.2017 <- function(...){  
    data <- read.csv(unzip(suppdata("10.1002/ecy.1799", 1))[4], as.is=TRUE)
    colnames(data)[1] <- "species"
    meta <- data[,"ParasiteTraitsCitation",drop=FALSE]
    data <- data[,names(data)!="ParasiteTraitsCitation"]
    units <- rep(NA,4)
    data$species <- tolower(gsub(" ","_", sanitize_text(data$species)))
    return(.df.melt(data, "species", units=units, metadata=meta))
}

#' @export
.vanier.2013 <- function(...){
    data <- read.delim(suppdata("E094-246", "Mass_volume_data.txt", "esa_archives"), sep = "", as.is = TRUE, na.strings = c("","NA"))
    data$Individual_Species <- tolower(data$Individual_Species)
    data$Species_Groups <- tolower(data$Species_Groups)
    metadata <- data[,c(1:2,4:5)]
    data <- data[,-c(1:2,4:5)]
    units <- c("g", "m^3", rep(NA, 3), "?", rep(NA, 3))
    return(.df.melt(data, "Individual_Species", units, metadata))
}

#' @export
.visser.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.fm654","final.csv"))
    data <- data[,2:8]
    metadata <- data[,c(1,2,3,5,7)]
    data <- data[,-c(2,3,5,7)]
    units <- c('cm','cm')
    return(.df.melt(data,"sp",units=units, metadata))
}

#' @export
.wilman.2014a  <- function(...){
    data <- read.delim(suppdata("E095-178", "BirdFuncDat.txt", "esa_archives"))
    data <- data[,-c(1,23,34)]
    data <- .df.melt(data, "Scientific")
    data$numeric$species <- tolower(gsub(" ", "_", data$numeric$species))
    data$character$species <- tolower(gsub(" ", "_", data$character$species))
    data$numeric$variable <- gsub("BodyMass.SpecLevel", "total_mass", data$numeric$variable, fixed=TRUE)
    data$numeric$variable <- gsub("BodyMass.Value", "total_mass.Value", data$numeric$variable, fixed=TRUE)
    return(data)
}
.wilman.2014b  <- function(...){
    data <- read.delim(suppdata("E095-178", "MamFuncDat.txt", "esa_archives"))
    data <- data[,-1,drop=FALSE]
    data <- .df.melt(data, "Scientific")
    data$numeric$species <- tolower(gsub(" ", "_", data$numeric$species))
    data$character$species <- tolower(gsub(" ", "_", data$character$species))
    data$numeric$variable <- gsub("BodyMass.SpecLevel", "total_mass", data$numeric$variable, fixed=TRUE)
    data$numeric$variable <- gsub("BodyMass.Value", "total_mass.Value", data$numeric$variable, fixed=TRUE)
    return(data)
}

#' @export
.winchell.2016 <- function(...){
    data <- read.csv(suppdata("10.5061/dryad.h234n","winchell_evol_phenshifts.csv"))
    metadata <- data[,c(3:5,7:11)]
    colnames(metadata) <- c('site','context','perch','perch_temperature','ambient_temperature','humidity_percent','perch_height','perch_diameter')
    metadata$site <- tolower(gsub(' ','_', metadata$site))
    data <- data[,-c(1:5,7:11,15:16)]
    colnames(data) <- c('body_temperature', 'mass', 'head_height', 'snout_vent_length', 'average_jaw_length', 'average_jaw_width','average_metacarpals','average_radius','average_ulna','average_humerus', 'average_femur', 'average_tibia','average_fibula','average_metatarsals_i','average_metatarsals_ii','total_forelimb_length','total_hindlimb_length')
    data$species <- 'anolis_cristatellus'
    metadata$perch_diameter <- as.numeric(metadata$perch_diameter)
    units <- c('C','g',rep('mm',15))
    return(.df.melt(data, "species", units,metadata))
}

#' @export
.yin.2015 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.76ph0","Species-XylemAnatomy.xlsx")))
    metadata <- data.frame(data[,2])
    data <- data[,-c(2,13)]
    units <- c(NA, NA, rep('um',6), NA, NA, NA)
    return(.df.melt(data,"Species",units=units, metadata))
}

#' @export
.zhang.2014 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.9tk7t","data_traits.xlsx")))
    data <- data[,-c(1:2,5:6,8:11,16)]
    metadata <- data[,c(1:3)]
    data <- data[,-c(1:3)]
    data$species <- 'sterna_hirundo'
    colnames(metadata) <- c('age', 'sex', 'lifespan')
    colnames(data) <- c('egg_volume', 'clutch_size', 'brood_size', 'no_fledglings', 'species')
    units <- c(rep(NA,4))
    data <- .df.melt(data, "species", units, metadata)
    return(data)
}

#############################################
# The following functions need to be ########
# checked by their original authors. ########
# I will not add units to papers I haven't ##
# read... ###################################
#' @export
.tian.2016 <- function(...){
    file<-tempfile()
    download.file("http://www.nature.com/article-assets/npg/srep/2016/160122/srep19703/extref/srep19703-s2.xls",file)
    data <- as.data.frame(read_xls(file, skip=8,
                                   col_names=c("site","species","family","important_value_index",
                                               "leaf_area","leaf_dry_weight","specific_leaf_area",
                                               "blank","stomatal_length","stomatal_density","spi",
                                               "blank2","leaf_thickness",
                                               "palisade-leaf_mesophyll_thickness_ratio",
                                               "spingy-leaf_mesophyll_thickness_ratio","blank3",
                                               "evergreen_deciduous","blank4","leaf_type")
    ))
    data$species <- gsub("\xa0", " ", data$species)
    data$species <- sanitize_text(data$species)
    data$species <- sapply(strsplit(data$species, " "), function(x) paste(x[1:2], collapse="_"))
    data <- data[,!grepl("blank", names(data))]
    metadata <- data.frame(site=data$site, family=data$family)
    data <- data[,-c(1,3)]
    return(.df.melt(data, "species", metadata=metadata))
}

#' @export
.maire.2016 <- function(...){
    data <- as.data.frame(read_excel(suppdata("10.5061/dryad.j42m7.2","globamax_data_160609 (for GEB ms).xlsx"), sheet = "Data"))
    data <- data[,order(names(data))]
    units <- c("umol m^-2 s^-1","%","nmol g^-1 s^-1","mm m^-1","kg dm^-3","g  kg^-1","cmol+ kg^-1","cmolc kg^-1",NA,"%wt","gC gN^-1",NA,"gC kg^-1",NA,NA,"cm","m",NA,NA,NA,NA,"%wt","mmol m^-2 s^-1",NA,NA,NA,NA,"mm mm^-1","mm mm^-1","gN m^-2","%","gN kg^-1",NA,"W m^-2","gP m^-2","mgP2O5 kg^-1","mm month^-1","mm month^-1",NA,"%","mm","mm","mm","mm",NA,"km",NA,NA,NA,"W m-2","%","dS m^-1","%wt","%","%wt",NA,"cm2 g^-1","% of ECEC",NA,"%","%","%","%",NA,"cmol kg^-1","degC",NA,"degC","degC","degC","#","degC")
    
    metadata = data[,c("Country","Latitude","Longitude","Cite", "Continent", "Dataset", "Expt_Remark", "Family","Genus","Location", "P.Method","P.retention.class", "P.Source", "Seeding_Sapling","SITECODE")]
    units <- units[!names(data) %in% c("Genus.spp",names(metadata))]
    data <- data[,!names(data) %in% names(metadata)]
    data$Genus.spp <- tolower(gsub(" ","_",data$Genus.spp))
    return(.df.melt(data, "Genus.spp",units,metadata))
}

# Out of commission until https://github.com/willpearse/suppdata/issues/22 is closed
#' @export
if(FALSE) .neuheimer.2016 <- function(...){
    data <- read.csv(unzip(suppdata("10.1890/15-1261.1", 1), "15-1261_Neuheimer_SizeDatabase.csv"), as.is=TRUE)
    value <- with(data, c(AdultSize1,AdultSize2,ProgenySize1,ProgenySize2))
    variable <- rep(c("size","size","progeny_size","progeny_size"),each=nrow(data))
    unit <- with(data, c(AdultSizeUnit1,AdultSizeUnit2,ProgenySizeUnit1,ProgenySizeUnit2))
    species <- with(data, rep(tolower(paste(Genus,Species,sep="_")),4))
    metadata <- rep(NA, length(species))
    output <- list(numeric=data.frame(species=species,metadata=metadata,variable=variable,value=value,units=unit),character=NULL)
    class(output) <- "natdb"
    return(output)
}

#'@export
.peel.2016 <- function(...) { 
dp_data <- read.csv(suppdata(x="10.5061/dryad.2fp34",si = "Eidolon data 2007_2014_Openaccess.csv"),as.is=TRUE)
###Create meta data object------------------------------------------------------------------------------------------------------
my_metadata <- as.data.frame(dp_data[c(1:12, 23:24, 26, 31, 33)])
colnames(my_metadata) <- c("Sample", "SamplingEventID", "Samplers", "Samplingdate", "Birthdate", "MthsSinceBirthdate", "Cont_Island", 
                           "Country", "Region", "Location","Latitude", "Longitude", "MotherID", "OffspringID", "BandNo", "GeneticsID",
                           "GenBankAccession")
### Create data object------------------------------------------------------------------------------------------------------
my_data <- dp_data[c(13:22, 25, 27:30, 32, 34:69)] 
### Rename columns ------------------------------------------------------------------------------------------------------
colnames(my_data) <- c("BatWt","Sex","Age", "AgeSpecific", "AgeTooth", "TeethCertaintyScore", "TeethAgeRange", "TeethAgeMths", "AgeMths", 
                       "ReproStatus", "Forearm", "LagosBatVirusScore", "HenipavirusResults", "AchimotaVirus1", "AchimotaVirus2", 
                       "CytochromeB", "T1AlleleSize", "T2AlleleSize", "S1AlleleSize", "S2AlleleSize", "F1AlleleSize", "F2AlleleSize", 
                       "W1AlleleSize", "W2AlleleSize", "N1AlleleSize", "N2AlleleSize", "Q1AlleleSize", "Q2AlleleSize","X1AlleleSize", 
                       "X2AlleleSize", "P1AlleleSize", "P2AlleleSize", "K1AlleleSize", "K2AlleleSize", "Ac1AlleleSize", "Ac2AlleleSize", 
                       "Af1AlleleSize", "Af2AlleleSize", "Ai1AlleleSize", "Ai2AlleleSize", "Ad1AlleleSize", "Ad2AlleleSize", 
                       "Y1AlleleSize", "Y2AlleleSize", "Ag1AlleleSize", "Ag2AlleleSize", "Ah1AlleleSize", "Ah2AlleleSize", 
                       "B1AlleleSize","B2AlleleSize", "M1AlleleSize", "M2AlleleSize")
### Add species name column------------------------------------------------------------------------------------------------------
my_data$species <- "eidolon_helvum" # species names
### Add units column ------------------------------------------------------------------------------------------------------
my_units <- c("g", NA, NA, NA, NA, "mo", "mo", NA, "mm", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

### df.melt ------------------------------------------------------------------
to_return_data <- .df.melt(x = my_data, spp = "species", units = my_units, metadata = my_metadata)
return(to_return_data)
}

#'@export
.sherratt.2018d <- function(...){
  downl_data <- read.csv(
    suppdata(
      x = "10.5061/dryad.48r5h.2",
      si = "trait data.csv")
    ,as.is=TRUE, header = TRUE, na.strings = c("")) #Read file from internet
  our_metadata <- data.frame(downl_data[, 2]) #Our metadata object
  our_data <- downl_data[, c(1, 3:8)] #Our data object
  colnames(our_data) <- c("species", "number_specimens_in_girth" , "max_total_length", "relative_girth_at_0.75SVL", "SD_of_relative_girth", "proportion_burrowing", "prey_type") #Column names for our data
  our_units <- c("", "mm","mm", "", "", "") #Units for our data
  our_final_data <- .df.melt(
    x = our_data,
    spp = "species",
    units = our_units,
    metadata = our_metadata)
  return(our_final_data)
}

#'@export
.ord.2015b <- function(...){
  downl_data <- read.delim(
    suppdata(
      x = "10.5061/dryad.f01t1",
      si = "ORD_TJ-JEB-2015-00382R1_hyoid_data.txt")
    ,as.is=TRUE) #Read file from internet
  our_data <- downl_data[, c(1:3)] #Our data object
  colnames(our_data) <- c("species", "fulcrum_ratio_of_hyoid", "hypohyal_angle_to_ceratobranchial") #Column names for our data
  our_units <- c("", "degrees") #Units for our data
  our_final_data <- .df.melt(
    x = our_data,
    spp = "species",
    units = our_units)
  return(our_final_data)
}