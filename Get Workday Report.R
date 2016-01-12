# Reads Workday report into a list
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
    
    # Check and validate format
    if (!grepl("&format=", URL)){
        format <- "workdayxml"
    }
    else {
        format <- unlist(strsplit(URL, "&format=", fixed = TRUE))[2]
    }
    
    # Use separate function based on format
    switch(format,
           workdayxml = {
               if(is.null(destFile)) {
                   stop("Format not supported as data frame conversion for
                        this format needs to be custom. Set the parameter
                        destFile to just download the file")
               }
               else {
                   tmpFile <- "workdayFile.tmp"
                   f <- CFILE(tmpFile, mode = "w")
                   a <- curlPerform(url = URL, username = username,
                                    password = password,
                                    writedata = f@ref)
                   close(f)
                   result <- TRUE
               }
           },
           simplexml = {
               tmpFile <- "workdayFile.tmp"
               f <- CFILE(tmpFile, mode = "w")
               a <- curlPerform(url = URL, username = username,
                                password = password,
                                writedata = f@ref)
               close(f)
               
               result <- xmlParse(tmpFile)
               result <- xmlToDataFrame(result)
           },
           csv = {
               file.remove(tmpFile)
               stop(paste("Format not supported: ", format))
           },
           gdata = {
               file.remove(tmpFile)
               stop(paste("Format not supported: ", format))
           },
           json = {
               tmpFile <- "workdayFile.tmp"
               f <- CFILE(tmpFile, mode = "w")
               a <- curlPerform(url = URL, username = username,
                                password = password,
                                writedata = f@ref)
               close(f)
               
               result <- fromJSON(tmpFile)
               result <- result$Report_Entry
           },
           {
               file.remove(tmpFile)
               stop(paste("Format not supported: ", format))
           })
    
    if(a != 0 ) {
        file.remove(tmpFile)
        stop("Error downloading the file. Please check the URL is correct as
             well as the username and password")
    }
    
    if(is.null(destFile)) {
        ## Remove temporary file
        file.remove(tmpFile)
    }
    else {
        ## Save the file locally
        file.copy(tmpFile, destFile, overwrite = TRUE)
        file.remove(tmpFile)
    }
    
    result
}