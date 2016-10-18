cladeCol <- function(phy, # the phylogeny
                      clades,	# a list of clades 
                      cols,		# a vector of colors, one for each clade
                      bg.col=par("fg")) {	# the color for ``background'' edges
    sub.phy <- subtrees(phy)
    
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
        if(length(the.match)==0) the.match <- 0
        
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
    these.sing <- which(sapply(clades,length)==1) # clade indeces
    print(these.sing)
    if(length(these.sing) > 0) {
        sing.tax <- unlist(clades[these.sing])	# taxon names
        sing.col <- cols[these.sing]
        
        sing.tip.ind <- match(sing.tax,phy$tip.label)
        
        phy.col[match(sing.tip.ind,phy$edge[,2])] <- sing.col
    }
    
    return(phy.col)
}

## test it
# phy <- read.tree(text="(A,(C,G));")
# plot(phy, edge.color=clade.col(phy, list("A",c("C","G")),c("red","blue")))

