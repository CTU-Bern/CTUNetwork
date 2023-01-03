#' Network plot function - Build the network
#' Tutorials:
#' https://www.statworx.com/en/content-hub/blog/interactive-network-visualization-with-r
#' http://datastorm-open.github.io/visNetwork/shiny.html
#'
#' @param nodes dataframe containing nodes data, see visNetwork()
#' @param edges dataframe containing edges data, see visNetwork()
#'
#' @import visNetwork
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()

NetworkPlot <- function(nodes, edges){

  visNetwork::visNetwork(nodes, edges,
                main = list(text="CTU - PROJECTFACTS NETWORK", style="font-size:25px; color: #0073b7; text-align: center;"),
                submain = "Nodes width = Amount of time booked for each individual worker - Edges width = Amount of time booked overall (all workers combined)") %>%
    # visIgraphLayout(layout = "layout.reingold.tilford",circular=T)  %>%
    # visIgraphLayout(layout = "layout_as_star", physics = T, randomSeed = 1234)  %>% # why not
    # visIgraphLayout(layout = "layout_in_circle", physics = T, randomSeed = 1234)  %>% # why not
    visNetwork::visIgraphLayout(layout = "layout_on_sphere", physics = T, randomSeed = 1234)  %>% # Probably the best one !
    # visIgraphLayout(layout = "layout_with_fr", physics = F, randomSeed = 1234)  %>% # the default one
    # visIgraphLayout(layout = "layout_with_kk", physics = T, randomSeed = 1234)  %>% # Good one
    # visIgraphLayout(layout = "layout_with_mds", physics = T, randomSeed = 1234)  %>% # Nicest one I believe
    visNetwork::visOptions(highlightNearest = T, nodesIdSelection = list(enabled = TRUE, main = "Select Worker/Project"),
               selectedBy = list(variable="group", main = "Select State")) %>% #, multiple=T
    visNetwork::visPhysics(solver = "hierarchicalRepulsion", hierarchicalRepulsion = list(springLength = 850, nodeDistance = 90), stabilization = "onlyDynamicEdges") %>%
    visNetwork::visNodes(shapeProperties = list(interpolation = F)) %>%
    visNetwork::visEdges(smooth = F, color=list(color = "#848484", highlight = "#000000")) %>%
    visNetwork::visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('node_id', nodes.nodes);
                    openBox('groupedbars');
                    openBox('indivbars');
                    ;}",
              deselectNode = "function(){
                    closeBox('groupedbars');
                    closeBox('indivbars');
                    }")
  # See: https://stackoverflow.com/questions/74768667/visnetwork-visevents-javascript-to-uncollapse-box
}
