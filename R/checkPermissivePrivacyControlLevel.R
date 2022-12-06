#'
#' @title checkPermissivePrivacyControlLevel
#' @description This serverside function check that the server is running in "permissive" privacy control level.
#' @details Tests whether the R option "datashield.privacyControlLevel" is set to "permissive", if it isn't
#' will cause a call to stop() with the message "BLOCKED: The server is running in 'non-permissive' mode which 
#' has caused this method to be blocked".
#' @author Wheater, Dr SM., DataSHIELD Team.
#'
checkPermissivePrivacyControlLevel <- function(){

    disclosureSettings <- dsBase::listDisclosureSettingsDS()
    if (is.null(disclosureSettings) || is.null(disclosureSettings$datashield.privacyControlLevel) ||
        (disclosureSettings$datashield.privacyControlLevel != 'permissive')) {
        stop("BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked", call. = TRUE)
    }

    invisible()   
}
