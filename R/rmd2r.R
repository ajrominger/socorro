#' @title Convert .Rmd file to a ready to run .R script
#'  
#' @description Converts markdown text to comments (following roxygen protocol) and optionally
#' comments out code chunks that produce plots.
#' 
#' @details This function is inspired by code from Kevin Keena 
#' (http://rstudio-pubs-static.s3.amazonaws.com/12734_0a38887f19a34d92b7311a2c9cb15022.html).
#' However, \code{rmd2r} is a complete re-write and adds the functionallity to specify a file 
#' in which to write the outpu and also the option to comment out plotting chunks (useful, 
#' e.g., if you'd like to run the script in the background)
#' 
#' @param x the Rmd file
#' @param file the file to write to, defaults to same name as \code{x} but appended with '.R'
#' @param noPlots logical, should plotting chunks be commented out
#' 
# @examples
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
# @seealso
#' @export

rmd2r <- function(x, file, noPlots = TRUE) {
    ## deal with possible file input
    if(missing(file)) file <- gsub('\\.rmd', '.R', x, ignore.case = TRUE)
    if(!grepl('\\.R', file, ignore.case = TRUE)) file <- paste0(file, '.R')
    
    ## load the Rmd file
    x <- readLines(x)
    
    ## find R code chunk starts
    rStrt <- grep('```\\{r', x)
    
    ## find all '```' tags
    codeTag <- grep('```', x)
    
    ## match start and end
    actuallyStrt <- which(codeTag %in% rStrt)
    rEnd <- codeTag[actuallyStrt + 1]
    
    ## if we don't want plotting chunks, remove them from rStrt and rEnd
    # if(noPlots) {
    #     plotsHere <- grep('plot', x)
    #     if(length(plotsHere) > 0) {
    #         browser()
    #         rmThese <- sapply(rStrt, function(r) any(r < plotsHere)) & 
    #             sapply(rEnd, function(r) any(r > plotsHere))
    #         rStrt <- rStrt[!rmThese]
    #         rEnd <- rEnd[!rmThese]
    #     }
    # }
    
    ## add roxygen tag to everything
    x <- paste0("#' ", x)
    
    ## remove from R code chunks
    for(i in 1:length(rStrt)) {
        if(noPlots) {
            if(!any(grepl('plot|par|acf', x[(rStrt[i] + 1):(rEnd[i] + 1)]))) {
                x[(rStrt[i] + 1):(rEnd[i] + 1)] <- 
                    gsub("#' ", '', x[(rStrt[i] + 1):(rEnd[i] + 1)])
                x[c(rStrt[i], rEnd[i])] <- ''
            }
        } else {
            x[(rStrt[i] + 1):(rEnd[i] + 1)] <- 
                gsub("#' ", '', x[(rStrt[i] + 1):(rEnd[i] + 1)])
            x[c(rStrt[i], rEnd[i])] <- ''
        }
    }
    
    writeLines(x, con = file)
}
