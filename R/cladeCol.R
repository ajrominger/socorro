#' @title Create colors for plotting clades
#'  
#' @description Create a vector of colors for use in \code{plot.phylo} so that designated clades have a unique color
#' 
#' @details Clades are specified by the names of their members, see example
#' 
#' @param phy the phylogeny for which to compute colors
#' @param clades a list of clades 
#' @param cols a vector of colors, one for each clade
#' @param bg.col color for ``background'' edges (i.e. those not specified by \code{clades})
#' 
#' @return A character vector of the desired colors
#' 
#' @examples
#' phy <- read.tree(text="(A,(C,G));")
#' plot(phy, edge.color=clade.col(phy, list("A",c("C","G")),c("red","blue")))
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso phylo, plot.phylo
#' @export

cladeCol <- function(phy, clades,cols, bg.col=par("fg")) {
    sub.phy <- ape::subtrees(phy)
    
    match.clades <- sapply(sub.phy, function(x) {
        tips <- x$tip.label
        ## looping over the clades and for each vector of species
        ## names it is asking are all of these species found within
        ## one of the sub.phy
        match.tips <- sapply(clades, function(y) {
            yintips <- all(y %in% tips)
            tipsiny <- all(tips %in% y)
            yintips & tipsiny
        })
        the.match <- which(match.tips)
        if(length(the.match) == 0) the.match <- 0
        
        the.match
    })
    
    
    sub.phy <- sub.phy[match.clades != 0]
    match.clades <- match.clades[match.clades != 0]
    
    phy.col <- rep(bg.col,nrow(phy$edge))
    
    if(length(sub.phy) > 0) {
        for (i in 1:length(sub.phy)) {
            these.nodes <- sub.phy[[i]]$node.label
            phy.col[phy$edge[,1] %in% these.nodes] <- cols[match.clades[i]]
            
            ## also want the node just before the sub tree...
            phy.col[phy$edge[,2] %in% min(these.nodes)] <- cols[match.clades[i]]
        }
    }
    
    ## now must account for single taxon clades
    these.sing <- which(sapply(clades, length) == 1) # clade indeces
    
    if(length(these.sing) > 0) {
        sing.tax <- unlist(clades[these.sing])	# taxon names
        sing.col <- cols[these.sing]
        
        sing.tip.ind <- match(sing.tax, phy$tip.label)
        
        phy.col[match(sing.tip.ind, phy$edge[,2])] <- sing.col
    }
    
    return(phy.col)
}
