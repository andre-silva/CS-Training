# Reads Workday report into a data frame
getReportFromWorkday <- function(URL, destFile = NULL, authFile = "settings") {
    # First get Workday authentication credentials. From my research I didn't
    # any great way of doing that. Since obviously we don't want those in the
    # code the solution is to have a text file containing username and password.
    # The file name and path should be passed on the authFile argument.
    # Otherwise it will look for a file with the name "settings" in the working
    # directory
    
    # Settings file should have the following format
    # username:jdoe
    # password:Pass123
    
    # Libraries necessary
    suppressWarnings(library(XML))
    suppressWarnings(library(jsonlite))
    suppressWarnings(library(gdata))
    suppressWarnings(library(RCurl))
    
    # Check if the file exists and has the correct format
    if(!file.exists(authFile)) {
        stop("Unable to find authentication file")
    }
    else {
        authSettings <- read.table(authFile, header=FALSE, sep=":",
                                   colClasses = "character",
                                   col.names = c("Property", "Value"))
        format <- c("username","password")

        if(all(length(format)==length(authSettings$Property)) &
           all(format == authSettings$Property)) {
            username <- authSettings$Value[1]
            password <- authSettings$Value[2]
        }
        else {
            stop("Authentication settings file format is not the expected")
        }
    }
    
    # Download the file. If a destination file name is provided, the report will
    # be kept, otherwise a temporary file will be created in the working
    # directory which will be deleted after it is read
    f <- CFILE("workdayFile.tmp", mode = "w")
    a <- curlPerform(url = URL, username = username, password = password,
                     writedata = f@ref)
    close(f)
    
    # Check and validate format
    if (!grepl("&format=", URL)){
        format <- "workdayxml"
    }
    else {
        format <- unlist(strsplit(URL, "&format=", fixed = TRUE))[2]
    }
    
    switch(format,
           workdayxml = {
               file.remove("workdayFile.tmp")
               stop(paste("Format not supported: ", format))
           },
           simplexml = {
               file.remove("workdayFile.tmp")
               stop(paste("Format not supported: ", format))
           },
           csv = {
               file.remove("workdayFile.tmp")
               stop(paste("Format not supported: ", format))
           },
           gdata = {
               file.remove("workdayFile.tmp")
               stop(paste("Format not supported: ", format))
           },
           json = {
               #file.remove("workdayFile.tmp")
               #stop(paste("Format not supported: ", format))
           },
           {
               file.remove("workdayFile.tmp")
               stop(paste("Format not supported: ", format))
           })
    
    if(a != 0 ) {
        file.remove("workdayFile.tmp")
        stop("Error downloading the file. Please check the URL is correct as
             well as the username and password")
    }
    
    if(is.null(destFile)) {
        ## Remove temporary file
        file.remove("workdayFile.tmp")
    }
    else {
        ## Save the file locally
        file.copy("workdayFile.tmp", destFile, overwrite = TRUE)
        file.remove("workdayFile.tmp")
    }
}