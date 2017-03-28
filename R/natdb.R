#' Builds a trait database
#'
#' The key function of the natdb package. When run with defaults, it
#' will download and build a database of species' traits from all the
#' manuscript sources in the package. This totals XXX
#' manuscripts/databases, XXX species, and XXX traits. Please note
#' that all parameters are interactive; thus specifying \code{species}
#' and \code{traits} constraints will constraint according to both,
#' for example. Please also note that specifying any kind of
#' constraints makes use of the package's built-in cache of what
#' species and traits information are available in each database;
#' making use of this on the GitHub (developer) build of this package
#' is not advisable, and (further) it is impossible for us to verify
#' whether the datasets NATDB searches have been updated since the
#' package was last built.
#' 
#' @param datasets Character vector of datasets to be searched for
#'     trait data. If not specified (the default) all trait datasets
#'     will be downloaded and returned.
#' @param species Character vector of species to be searched for trait
#'     data. If not specified (the default) data for all species will
#'     be downloaded and returned.
#' @param traits Character vector of traits to be searched for
#'     data. If not specified (the default) data for all traits will
#'     be downloaded and returned.
#' @return natdb.data object. XXX
#' @author Will Pearse; USU Biology Nerd Group (XXX)
#' #@examples
#' # Limit the scope of these as they have to work online on servers!...
#' #@seealso 
#' @export

natdb <- function(cache, datasets, delay=5){
    #Check datasets
    if(missing(datasets)){
        datasets <- Filter(Negate(is.function), ls(pattern="^\\.[a-z]*\\.[0-9]+", name="package:natdb", all.names=TRUE))
    } else {
        datasets <- paste0(".", tolower(datasets))
        datasets <- gsub("..", ".", datasets, fixed=TRUE)
    }
    if(!all(datasets %in% datasets)){
        missing <- setdiff(datasets, ls.funs())
        stop("Error: ", paste(missing, collapse=", "), "not in natdb")
    }
    
    #Do work and return
    output <- vector("list", length(datasets))
    for(i in seq_along(datasets)){
        prog.bar(i, length(datasets))
        if(!missing(cache)){
            path <- file.path(cache,paste0(datasets[i], ".RDS"))
            if(file.exists(path)){
                output[[i]] <- readRDS(path)
            } else {
                output[[i]] <- eval(as.name(datasets[i]))()
                saveRDS(output[[i]], path)
                Sys.sleep(delay)
            }
        } else {
            output[[i]] <- eval(as.name(datasets[i]))()
            Sys.sleep(delay)
        }
        if(!is.null(output[[i]]$numeric))
            output[[i]]$numeric$dataset <- datasets[i]
        if(!is.null(output[[i]]$character))
            output[[i]]$character$dataset <- datasets[i]
    }
    
    numeric     <- do.call(rbind,
                           lapply(Filter(function(y) !is.null(y[[1]]), output), function(x) x[[1]])
                           )
    categorical <- do.call(rbind,
                           lapply(Filter(function(y) !is.null(y[[2]]), output), function(x) x[[2]])
                           )
    output <- list(numeric=numeric, categorical=categorical)
    class(output) <- "natdb"
    return(output)
}

print.natdb <- function(x, ...){
    # Argument handling
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    
    # Do main summary
    output <- matrix(0, 3, 3, dimnames=list(c("Numeric","Categorical","Total"),c("Species","Traits","Data-points:")))
    try(output[1,] <- c(length(unique(x$numeric$species)), length(unique(x$numeric$variable)), nrow(x$numeric)), silent=TRUE)
    try(output[2,] <- c(length(unique(x$categorical$species)), length(unique(x$categorical$variable)), nrow(x$categorical)), silent=TRUE)
    output[3,] <- colSums(output)
    cat("A Trait DataBase containing:\n")
    print(output)

    # Supplemental summaries
    printer <- FALSE
    if(!all(is.na(c(x$categorical$metadata,x$numeric$metadata)))){
        printed <- TRUE
        cat("Meta-data present. ")
    }
    if(!all(is.na(c(x$categorical$units,x$numeric$units)))){
        printed <- TRUE
        cat("Units present. ")
    }
    if(printed)
        cat("\n")
    invisible(output)
}

summary.natdb <- function(x, ...){
    print.natdb(x, ...)
}

"[.natdb" <- function(x, spp, traits){
    # Argument handling
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")

    # Species
    if(!missing(spp)){
        if(any(x$numeric$species %in% spp))
            x$numeric <- x$numeric[x$numeric$species %in% spp,] else
                                                                    x$numeric <- NULL
        if(any(x$categorical$species %in% spp))
            x$categorical <- x$categorical[x$categorical$species %in% spp,] else
                                                                          x$categorical <- NULL
    }
    
    # Traits
    if(!missing(traits)){
        if(any(x$numeric$variable %in% traits))
            x$numeric <- x$numeric[x$numeric$variable %in% traits,] else
                                                                        x$categorical <- NULL
        if(any(x$categorical$variable %in% traits))
            x$categorical <- x$categorical[x$categorical$variable %in% traits,] else
                                                                              x$categorical <- NULL
    }

    output <- list(categorical=x$categorical, numeric=x$numeric)
    class(output) <- "natdb"
    return(output)
}

species <- function(x, ...){
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    return(unique(c(x$numeric$species,x$categorical$species)))
}

traits <- function(x, ...){
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    return(unique(c(x$numeric$variable,x$categorical$variable)))
}

 citations <- function(x, ...){
     if(!inherits(x, "natdb"))
         stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
     # load in bibtex file
     bib <- readRDS("~/Documents/natdb/data/natdb_citations.RData")
     # create column with just the function names (.name.year)
     bib$Name <- paste0(".", tolower(bib$Author), ".", bib$Year)
     # find referenced function, get name, return bibtex
     datasets <- Filter(Negate(is.function), ls(pattern="^\\.[a-z]*\\.[0-9]+", name="package:natdb", all.names=TRUE))
 
     return(bib[which(bib$Name=='xxxxxx'),3]) 
 }
