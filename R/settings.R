# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
MYPKGsettings <- settings::options_manager(
    header = TRUE,
    method = "",
    numberOfBins = 100,
    tripleFormat = TRUE,
    numberOfThreads = -1,
    numberOfPermutations = 1000,
    pvalueCutOff = 1.0,
    batchSize = 100,
    considerZeros = TRUE,
    pChiSquare = 0.05,
    selectedGenes = NULL,
    restricted = FALSE
)

#' Set or get settings for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set settings.
#' @importFrom settings stop_if_reserved
#' @importFrom settings options_manager
#' @section Supported settings:
#' The following settings are supported
#' \itemize{
#'  \item{\code{a}}{(\code{numeric};1) The value of a }
#'  \item{\code{b}}{(\code{numeric};2) The value of b }
#' }
#'
#' @export
jami_settings <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    MYPKGsettings(...)
}

#' Create a JAMI SettingsManager instance
#'
#' @import rJava
#' @import settings
#' @return a populated settings manager
#'
jami_create_settings_manager <- function(){
    settingsManager <- .jnew("org.mpii.jami.helpers.SettingsManager")

    current_settings <- jami_settings()
    for(key in names(current_settings)){

        value <- current_settings[[key]]
        if(is.logical(value))
            obj <- .jnew("java.lang.Boolean", value)
        else if(is.character(value))
            obj <- .jnew("java.lang.String", value)
        else if(is.null(value))
            obj <- .jnull("java.lang.Object")
        else if(is.double(value)){
            if(value%%1!=0)
                obj <- .jnew("java.lang.Double", value)
            else
                obj <- .jnew("java.lang.Integer", as.integer(value))
        }
        else{
            warning(paste0("skipping option ", key,
                           "because its type is not handled"))
            next
        }
        castObj <- .jcast(obj = obj, new.class = "java.lang.Object")
        .jcall(obj = settingsManager,
               returnSig = "V",
               method = "set",
               .jnew("java.lang.String", key),
               castObj)
    }

    return(settingsManager)
}



