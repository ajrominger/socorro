#' @title Pretty ordering of clades for plotting
#'  
#' @description Orders clades according to number of decendants for plotting purposes
#' 
#' @details This function function will re-order a phylogeny such that clades with the fewest
#' decendents will be plotted `before' (above or to the left of) clades with more decendents. 
#' This works recursively from the root.
#' 
#' @param phy a phylogenetic tree (class \code{phylo}) to be re-ordered
#' 
#' @examples
#' tre <- ape::rtree(10)
#' tre <- prettyOrder(tre)
#' ape::plot.phylo(tre)
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso ape::rotate
#' @export

prettyOrder <- function(phy) {
    bal <- .getNDaughter(phy)
    
    node2rt <- which(bal[, 1] < bal[, 2]) + length(phy$tip.label)
    if(length(node2rt) > 0) for(i in 1:length(node2rt)) phy <- ape::rotate(phy, node2rt[i])
    
    phy <- ape::read.tree(text = ape::write.tree(phy))
    return(phy)
}

.getNDaughter <- function(phy) {
    ape::balance(ape::read.tree(text = ape::write.tree(phy)))
}
