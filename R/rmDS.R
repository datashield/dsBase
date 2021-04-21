#' @title rmDS an aggregate function called by ds.rm
#' @description deletes an R object on the serverside
#' @details this is a serverside function
#' based on the rm() function in native R. It is an aggregate function
#' which may be surprising because it modifies an object
#' on the serverside, and would therefore be expected to be an assign function.
#' However, as an assign function the last step in running it
#' would be to write the modified object as newobj. But this would
#' fail because the effect of the function is to delete the object and so
#' it would be impossible to write it anywhere.
#' @param x.names.transmit, the names of the objects to be deleted converted
#' into transmissable form, a comma seperated list of character  string. The
#' argument is specified via the <x.names> argument of ds.rm
#' @return the specified object is deleted from the serverside. If this
#' is successful the message "Object <x.names> successfully deleted" is returned
#' to the clientside (where x.names are the names of the object to be deleted).
#' If the objects to be deleted is already absent on a given
#' source, that source will return the message: "Object to be deleted, i.e. <x.names>,
#' does not exist so does not need deleting".
#' @author Paul Burton for DataSHIELD Development Team
#' @export

rmDS <- function(x.names.transmit)
{
    #convert x.names.transmit format from transmittable to actionable form (a vector of character strings)
    x.names<-unlist(strsplit(x.names.transmit, split=","))

    deleted.objects <- vector(mode = "character")
    missing.objects <- vector(mode = "character")
    problem.objects <- vector(mode = "character")

    #process request
    for (x.name in x.names) {
        if (exists(x.name, envir = parent.frame())) {
            rm(list = c(x.name), pos = 1, envir = parent.frame())

            if (exists(x.name, envir = parent.frame())) {
                problem.objects <- append(problem.objects, x.name)
            } else {
                deleted.objects <- append(deleted.objects, x.name)
            }
        } else {
            missing.objects <- append(missing.objects, x.name)
        }
    }

    #create return message
    return.message <- "Object(s)"
    if (length(deleted.objects) != 0) {
        return.message <- paste0(return.message, " '", paste(deleted.objects, collapse=','), "' was deleted.")
    }
    if (length(missing.objects) != 0) {
        return.message <- paste0(return.message, " '", paste(missing.objects, collapse=','), "' which are missing.")
    }
    if (length(problem.objects) != 0) {
        return.message <- paste0(return.message, " '", paste(problem.objects, collapse=','), "' which caused problems.")
    }

    return(list(return.message = return.message, deleted.objects = paste(deleted.objects, collapse=','), missing.objects = paste(missing.objects, collapse=','), problem.objects = paste(problem.objects, collapse=',')))
}
#AGGREGATE FUNCTION
# rmDS
