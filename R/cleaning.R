#' Cleaning MADtraits objects
#'
#' A very useful, and very much recommended, function for 'cleaning'
#' MADtraits data before serious use! It provides three kinds of
#' potential cleaning: of the trait names (e.g., harmonising datasets
#' so that "sla" and "specific_leaf_area" are recognised as the same
#' trait), of species' names (correcting some typos and name changes
#' using 'taxize'), and of trait units (e.g., harmonising across
#' datasets such that masses are all in the same unit). It is
#' *strongly recommended* that you perform some kind of cleaning of
#' MADtrait data before using it. The logic of the MADworld is to make
#' it easy for you to get data, and then transparent how that data has
#' been cleaned and managed downstream. We make no guarantee that the
#' decisions we have made in terms of cleaning are the "best" - please
#' feel free to use this code as a starting point, and improve from
#' there!
#' 
#' @param x \code{\link{MADtraits}} object
#' @param option What cleaning to perform: focusing on trait variable
#'     naes (DEFAULT; "traits"), species' taxonomic names ("species"),
#'     data units ("units"), or all three at once ("everything").
#' @param taxon.cache Whether to use MADtraits' internal cache of
#'     taxonomic lookup information for cleaning species' names
#'     (default: \code{TRUE}) or to build one from scratch at run-time
#'     using \code{taxize} (set as \code{FALSE}). Building from
#'     scratch is a very slow process! You can also pass a 'lookup'
#'     character vector that contains species' current (messy) names
#'     as the \code{names} element, and the clean (correct) names as
#'     the main entries. If you look at the code (which is short),
#'     this allows \code{clean.MADtraits} to run the equivalent of
#'     \code{lookup[raw_names]} to get the new, 'clean' names.
#' @param taxon.thresh Threshold of certainty to be used as a minimum
#'     when assigning new names to a species when building a lookup
#'     from scratch (see \code{taxon.cache}). The default, of 0.8, has
#'     not been chosen with any particular intelligence.
#' @param unit.choices Named vector of units, where the names are
#'     variables and the values are the units you would like that unit
#'     in. See examples - this isn't as confusing as it sounds. Units
#'     should be given in standard scientific notation - see
#'     \code{\link[convertr]{convert}} for more details.
#' @return \code{\link{MADtraits}} object
#' @author Will Pearse
#' @seealso convertunits.MADtraits taxonlookup.MADtraits
#' @examples
#' # Grab some example data
#' # - note that you should work with the output from the MADtraits function
#' # - since "cleaning" a single dataset doesn't achieve very much!
#' demo <- .cavenderbares.2015a()
#' MADtraits(datasets=c)
#' # Clean trait names (the default)
#' clean.MADtraits(demo)
#' # Clean species' names
#' clean.MADtraits(demo, "species")
#' # Clean units
#' clean.MADtraits(demo, "units")
#' # Clean it all!
#' clean.MADtraits(demo, "everything")
#' @export
clean.MADtraits <- function(x, option=c("traits","species","units","everything"), taxon.cache=TRUE, taxon.thresh=0.8, unit.choices=NA){
    # Argument handling
    if(!inherits(x, "MADtraits"))
        stop("'", deparse(substitute(x)), "' must be of type 'MADtraits'")
    option <- match.arg(option)

    # Do work
    if(option=="traits" | option=="everything")
        x <- .clean.trait.names(x)
    if(option=="species" | option=="everything")
        x <- .clean.taxonomy(x, taxon.thresh, taxon.cache)
    if(option=="units" | option=="everything")
        x <- .unit.conversion(x, unit.choices)
    
    return(x)
}

.clean.trait.names <- function(x){
    if(!is.null(x$numeric)){
        x$numeric <- x$numeric[x$numeric$variable != "",]
        x$numeric$variable <- tolower(x$numeric$variable)
        x$numeric$variable <- gsub(".", "_", x$numeric$variable, fixed=TRUE)
        x$numeric$variable <- gsub("^leaf[0-9]$", "leaf", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^puncture_[0-3]$", "puncture", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        
        x$numeric$variable <- gsub("^area_cm2$", "area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^area_m2$", "area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^av_female_length$", "female_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^av_male_width$", "male_width", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^ash_g_dry_weight_1$", "ash_g_dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^ash_g_dry_weight_2$", "ash_g_dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^ash_g_dry_weight_3$", "ash_g_dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_ash$", "ash", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_female_adult_weight$", "female_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_dry_weight$", "dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_indiv__leaf_area$", "leaf_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_ldmc$", "leaf_dry_matter_content", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_puncture$", "leaf_puncturability", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_sla$", "specific_leaf_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^bamax__cm2_$", "bamax", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^bill_length1$", "bill_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^area_cm2$", "area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^area$", "m2", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^birth_or_hatching_svl_cm$", "birth_snout_vent_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^birth_or_hatching_weight_g$", "birth_weight", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^body_wt$", "mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^bodymass_speclevel$", "mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^bodymass_value$", "mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^bodytemp_c$", "body_temperature", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^dbh$", "diameter_at_breast_height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^dbh__cm_$", "diameter_at_breast_height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^diam$", "diameter", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^dry_weight$", "dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^egg_mass_g$", "egg_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^dry_wgt$", "dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^foliar_area$", "foliar_area_mm2", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^foliar_area_mm2_1$", "", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^growth_svl$", "growth_snout_vent_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^head_height_mm$", "head_height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^head_l$", "head_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^height__m_$", "height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^height_apex$", "height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^ldmc$", "leaf_dry_matter_content", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_6_sla$", "specific_leaf_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_density_g_cm3$", "leaf_density", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_life_span$", "leaf_lifespan", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_nitrogen_content$", "leaf_nitrogen", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_toughness__mn_m_2_$", "leaf_toughness", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf1_area$", "leaf_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_dry_weight$", "leaf_dry_weight", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_ldmc$", "leaf_dry_matter_content", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_sla$", "species_leaf_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_dry_weight$", "leaf_dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_c$", "leaf_carbon", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_n$", "leaf_nitrogen", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaflength$", "leaf_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leafcn$", "leaf_carbon_nitrogen", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leafthck_avd$", "leaf_thickness", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leafthck_avi$", "leaf_thickness", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leafthickness$", "leaf_thickness", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leafwidth$", "leaf_width", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^length__um_$", "length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^lma__g_m_2_$", "leaf_mass_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^lma_g_m2$", "leaf_mass_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^mass_g$", "mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^male_svl_cm$", "male_snout_vent_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^maximum_longevity_y$", "maximum_longevity", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^mean_clutch_size$", "clutch_size", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^mean_female_svl_adults$", "female_snout_vent_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^mean_head_length$", "head_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^mean_glossa_length__mm_$", "glossa_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^perch_diam_cm$", "perch_diam", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^perch_height$", "", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^perch_temp_c$", "perch_temp", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^photosynthetic_capacitity_per_unit_leaf_mass$", "photosynthetic_capacitity", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^phototsynthetic_capacity_per_area$", "phototsynthetic_capacity", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^phototsynthetic_capacity_per_mass$", "phototsynthetic_capacity", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^plant_height__m_$", "height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^plant_width__m_$", "width", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^respiration_rt_ind$", "respiration_rate", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^respiration_rt_mg$", "respiration_rate", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^rjuv__cm2_$", "rjuv", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^rmax__cm2_$", "rmax", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^rootbiomasst8$", "root_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^rootlengtht0$", "root_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^rootlengtht8$", "root_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^rootrgr$", "root_relative_growth_rate", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^seedlength$", "seed_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^seedmaxwidth$", "max_seed_width", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^seedthick$", "seed_thickness", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^shootbiomasst8$", "shoot_biomass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^shootdens12$", "shoot_density", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^shootdens13$", "shoot_density", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^shootheightt0$", "shoot_height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^soil_n$", "soil_nitrogen", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^soil_c$", "soil_carbon", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^soil_p$$", "soil_phosphorous", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^soil_ph$", "soil_pH", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^solitary_vessels____$", "solitary_vessels", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^sproutd_mm$", "sproutd", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^sqrt_spider_prey_consumed$", "spider_prey_consumed", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^sqrt_sundew_prey_consumed$", "sundew_prey_consumed", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^sqrt_toad_prey_consumed$", "toad_prey_consumed", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^stumpd_mm$", "stumpd", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^svl$", "snout_vent_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^svl_mm$", "snout_vent_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^thick_m$", "thickness", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^thick_mm$", "thickness", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^total_dry_mass$", "dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^total_fresh_weight__g_$", "fresh_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^total_mass$", "mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^total_mass_dry$", "dry_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^total_mass_value$", "mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^phenolics$", "phenolics", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^total_sundews$", "sundews", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^totalspermnumber_in_millions$", "sperm", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^tree_height$", "height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^volume_m3$", "volume", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^volume_male$", "male_volume", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^volume_female$", "female_volume", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^weaning_body_mass$", "weaning_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^weight_g$", "weight", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^wet_wgt$", "wet_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^wood_density__g_cm_3_$", "wood_density", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^body_mass$", "mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^body_mass_female$", "female_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^body_mass_male$", "female_mass", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^body_size$", "size", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^foliar_m2_mm2$", "foliar", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^foliar_m2_mm2_1$", "", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^foliar_nitrogen_content____$", "foliar_nitrogen", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^head_lengthength$", "head_length", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_carbonc$", "leaf_carbon", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_carboncn$", "leaf_carbon_nitrogen", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^sla$", "sla", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf[0-9]_", "leaf_", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^seedmass$", "^seed_mass$", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^leaf_ldmc$", "leaf_dry_matter_content", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^max_tree_height$", "height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^average_maximum_height$", "height", x$numeric$variable, perl=TRUE, useBytes=TRUE)
        x$numeric$variable <- gsub("^total_leaf_area$", "leaf_area", x$numeric$variable, perl=TRUE, useBytes=TRUE)

        x$numeric$species <- tolower(gsub(" ", "_", x$numeric$species))
    }
    
    if(!is.null(x$categorical)){
        x$categorical$variable <- tolower(x$categorical$variable)
        x$categorical$variable <- gsub(" |\\.", "_", x$categorical$variable)
        x$categorical$species <- tolower(gsub(" ", "_", .sanitize.text(x$categorical$species), perl=TRUE, useBytes=TRUE))
    }
    
    return(x)
}

.clean.taxonomy <- function(x, thresh=0.8, use.cache=TRUE){
    # Download / use cache
    if(is.character(use.cache)){
        if(is.null(names(use.cache)))
            stop("'", deparse(substitute(use.cache)), "' must be a named vector for lookup (consider just using the internal cache or no pre-cached lookup)")
        lookup <- use.cache
    } else {
        if(use.cache==TRUE){
            lookup <- MADtraits_taxonomy
        } else  {
        warning("It can take an *exceedingly* long time to download taxonomic data for the entire dataset, and a server error in the middle of this may mess everything up. Please read the help file for advice on this!")
        spp <- unique(c(unique(x$numeric$species), unique(x$categorical$species)))
        dwn.spp <- gnr_resolve(spp, resolve_once=TRUE, best_match_only=TRUE)
        dwn.spp <- dwn.spp[!duplicated(dwn.spp$user_supplied_name),]
        dwn.spp$matched_name <- tolower(sapply(strsplit(dwn.spp$matched_name, " "), function(x) paste(x[1:2],collapse="_")))
        
        dwn.spp <- dwn.spp[dwn.spp$score >= thresh,]
        lookup <- with(dwn.spp, setNames(matched_name, user_supplied_name))
        }
    }
    
    # Lookup and return
    x$numeric$species <- lookup[x$numeric$species]
    x$categorical$species <- lookup[x$categorical$species]
    return(x)
}

#' @importFrom convertr convert
.unit.conversion <- function(x, choices=NA){
    if(is.na(choices)){
        choices <- tapply(x$numeric$units, x$numeric$variable, function(y) names(sort(table(y),decreasing=TRUE)[1]))
        choices <- Filter(Negate(is.null), choices)
    }
    
    for(i in seq_along(choices)){
        old.unit <- unique(x$numeric$units[x$numeric$variable==names(choices)[i]])
        for(j in seq_along(old.unit)){
            converted <- with(x$numeric, tryCatch(
                     convert(value[variable==names(choices)[i] & units==old.unit[j]], old.unit, choices[i]),
                 error=function(y) NA
                 ))
            if(any(Negate(is.na)(converted))){
                x$numeric$units[x$numeric$variable==names(choices)[i] & x$numeric$units==old.unit[j]] <- choices[i]
                x$numeric$value[x$numeric$variable==names(choices)[i] & x$numeric$units==old.unit[j]] <- converted
            }
        }
    }   
    
    return(x)
}
