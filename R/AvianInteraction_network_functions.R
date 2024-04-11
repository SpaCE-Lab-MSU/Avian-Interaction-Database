# TITLE:          Avian Interaction Network Visualization functions
# AUTHORS:        Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker Phoebe Zarnetske
# DATA INPUT:     From AvianInteractionData_L1.R: L1 CSV data from this R function.
#                 also reads the bbs species file bbs_splist_2022_L1.csv
#
# DATA OUTPUT:    converts the interaction data into bidirectional Edge Lists and iGraph 'graph' objecs.
#                 these may be saved as CSVs, or as graphml files for import into network/graph visualization programs
#                  (like Cytoscape) or used in other analysis
# PROJECT:        Avian Interaction Database
# DATE:           30 March 2024; updated 10 April 2024
# NOTES:          The outputs from these functions were imported into the Cytoscape GUI program to create visualizations.
#                 The functions here that create visualizations using the ggraph package are too dense or don't allow labels in the center
#                 of the nodes.
#                 When creating edgelists is possible to filter out those matching AOU numbers, eg from bb
#                 L0 data are checked to assign BOW scientific and common names
#                 to the interaction pairs data (which were originally from BBS species list).

library(readr)
library(magrittr)
library(tidygraph)
library(ggraph)
library(networkD3)

L0_dir <- "L0"
L1_dir <- "L1"
bbs_species_file <- "bbs_splist_2022_L1.csv"
bbs_species <- readr::read_csv(file.path(L1_dir, bbs_species_file))
avitx.file <- "AvianInteractionData_BBS_L1.csv"


#'  get families of scientific names
#'
#'  given a vector of scientific names, lookup the taxonomic families
#'
#'  return vector of taxonomic families in same order as scientific names
family_lookup<- function(scinames){
    gsp <- strsplit(scinames, ' ')
    genera<- unlist(lapply(gsp, `[[`, 1))

    f<- function(genus) {return(bbs_species[bbs_species$Genus == genus,]$Family[1])}
    families<- unlist(lapply(genera, f))
    return(families)
}


taxonomic_families<- function(avitx.graph){
    # get a list of g,species vectors
    scinames <- unlist(strsplit(V(avitx.graph)$name, ' '))
    # extract just the genus (first item) and make list a vector
    families <- family_lookup(scinames)
    return(families)
}

rm_subspecies <- function(s) { paste(str_split_i(s, " ",1),str_split_i(s, " ",2), sep=" ")}


#' read in a list of avian interactions (BBS or full list)
#' create a graph object and add attributes like Family
read_itx_edgelist<- function(folder=L1_dir, filename=avitx.file, include_neutral = FALSE, species_filter=NULL){
    avitx <- readr::read_csv(file.path(folder, filename))
    # filter only strong evidence
    avitx <- dplyr::filter(avitx, BOW_evidence == 'strong')

    # cleaning
    # 1. remove neutral interactions
    avitx <- dplyr::filter(avitx, BOW_evidence == 'strong')

    # 2. reduce all sci names to Genus species to lump species together for viz
    avitx <- dplyr::mutate(avitx, species1_scientific = rm_subspecies(species1_scientific)) %>% dplyr::mutate(species2_scientific = rm_subspecies(species2_scientific))

    # 3. filter to only those that match a species list (e.g. BBS)
    if( !is.null(species_filter)) {
        # filter by species
        avitx <- avitx %>%
                semi_join(species.df, by = join_by(sp1_AOU == AOU), copy = FALSE) %>%
                semi_join(species.df, by = join_by(sp2_AOU == AOU), copy = FALSE)
    }


    # convert all interactions to edge list
    # since the interactions are two-way, and we want to color by +/-,
    # create an edge for each way

    edgelist1 <- data.frame(sp1=avitx$species1_scientific,
                            sp2=avitx$species2_scientific,
                            itx_value=avitx$effect_sp1_on_sp2)

    edgelist2 <- data.frame(sp1=avitx$species2_scientific,
                            sp2=avitx$species1_scientific,
                            itx_value=avitx$effect_sp2_on_sp1)



    edgelist <-  dplyr::bind_rows(edgelist1, edgelist2)

    if ( include_neutral==FALSE){
        edgelist <- dplyr::filter(edgelist, itx_value != 0)
    }

    # TODO: summarize these edgelists by summing the positive interactions and negative interactions seperately?
    return (edgelist)
}


#' read in a list of avian interactions (BBS or full list)
#' create a graph object and add attributes like Family
read_itx_graph<- function(folder=L1_dir, filename=avitx.file, sample_size = 1){

    edgelist <- read_itx_edgelist<- function(folder, filename)

    # optional reduce the size of the edgelist prior to creating the graph
    # default sample_size 1 = 100% of edges
    edgelist <- dplyr::sample_frac(edgelist, sample_size)

    g <- tidygraph::as_tbl_graph(edgelist) %>%
        mutate(Centrality = tidygraph::centrality_degree(mode = 'in')) %>%
        mutate(name_width = nchar(name)+1)

    # iGraph method for adding taxonomic family attribute to nodes
    g <- set_vertex_attr(graph=g, "Family", value=family_lookup(V(g)$name))

    return(g)

}

#' this is of limited value, not working but it's a start at getting some edges
#' the go out of a node and then into a node.  No idea how to actaully make an igraph out of these.
species_edges<- function(g, sciname){


}

#' this creates a graph of the neighborhood of a single species vertex with the exact name string
#' .e.g if there are subspecies in the names, must match the subspecies.
#' this include the edges to and from the species vertex in question, but also all the edges
#' connecting all the other species out-bound edges
#'
#' change the 'mode' in this function to 'all' to get more edges between the neighboring nodes.
species_network<- function(g, sciname){
    species_node <- V(g)[name == sciname]
    s_g <- igraph::make_ego_graph(g, order = 1, nodes = species_node, mode="out")
    return(s_g)
}

#' this does not work
species_neighbors <- function(g, sciname) {
    species_node <- V(g)[name == sciname]
    v <- igraph::neighbors(g, species_node)
    e <- igraph::incident_edges(g, species_node)
    n <- make_empty_graph() +  v + e
    return(n)
}
#
# # get unique list of all species present?
# all_species = dplyr::bind_rows(
#     data.frame(sp=avitx$species1_scientific,aou=avitx$sp1_AOU),
#     data.frame(sp=avitx$species2_scientific,aou=avitx$sp2_AOU),
#   ) %>% unique
#



## alternative graph visualization D3 network - data has to be built properly with values for edges and proper groups (e.g. families) or reduced to families for this to work
# networkD3::simpleNetwork(data.frame(from=avitx.edgelist.small$sp1, to=avitx.edgelist.small$sp2))
## this is two scattered, and using the low-level 'forcenetwork()' function requires creating names and groups


# way too complex graph for all of the nodes
gviz <- function(g){
    p<- ggraph(g, layout = 'kk') +
    geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) +
    geom_node_point(aes(size = Centrality)) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
    return(p)
}


gviz_with_species <- function(g){
    p<- ggraph(g, layout = 'kk') +
        geom_edge_arc(
            arrow = arrow(angle = 15, ends = "last", type = "closed")
                      ) +
        geom_node_text(aes(label = name), vjust = "inward", hjust = "inward", position = "identity") +
        geom_text()
        geom_node_point() +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
    return(p)
}


gviz_neighborhood<- function(g){
    d3g<- igraph_to_networkD3(g)
    forceNetwork(Links = d3g$links, Nodes = d3g$nodes, Source="source", Target="target",Value="value", group=rep(0,len(d3g$nodes)), NodeID = "name", opacity = 1)

}

plot_neighborhood<- function(g, species, n=2){
    sp_node = V(g)[species]
    nbhood <- make_neighborhood_graph(g,n,sp_node)[[1]]
    gviz_with_label(nbhood)
}

write_itx_graph = function(avitx.graph){
    # avitx.graph <- read_itx_graph(L1_dir,"AvianInteractionData_BBS_L1.csv")

    write_graph(avitx.graph, "../avitx.graphml", format="graphml")
}


