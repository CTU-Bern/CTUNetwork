#' Network plot function - Build the network
#' Tutorials:
#' https://www.statworx.com/en/content-hub/blog/interactive-network-visualization-with-r
#' http://datastorm-open.github.io/visNetwork/shiny.html
#'
#' @param nodes dataframe containing nodes data, see visNetwork()
#' @param edges dataframe containing edges data, see visNetwork()
#' @param params list of parameters to control graph behaviour, see visNetwork()
#'
#' @import visNetwork
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()

NetworkPlot <- function(nodes, edges, params = list(layout = "layout_on_sphere", physics = T)){

  # To avoid error: "the function needs igraph package to compute layout"
  # library("igraph")

  # Create the initial plot
  Plot <- visNetwork::visNetwork(nodes, edges,
                main = list(text="CTU - PROJECTFACTS NETWORK", style="font-size:25px; color: #0073b7; text-align: center;"),
                submain = "Nodes width = Amount of time booked for each individual worker - Edges width = Amount of time booked overall (all workers combined)")

  # Select the layout (from UI)
  if (params$layout == "layout.reingold.tilford"){
    Plot <- Plot %>%
    visNetwork::visIgraphLayout(layout = "layout.reingold.tilford", physics = params$physics, circular = T, randomSeed = 1234)
  } else {
    Plot <- Plot %>%
    visNetwork::visIgraphLayout(layout = params$layout, physics = params$physics, randomSeed = 1234)}

  # Add the options
  Plot %>%
    visNetwork::visOptions(highlightNearest = T, nodesIdSelection = list(enabled = TRUE, main = "Select Worker/Project"),
                           selectedBy = list(variable="group", main = "Select State")) %>% #, multiple=T
    visNetwork::visPhysics(solver = params$solver, hierarchicalRepulsion = list(springLength = 850, nodeDistance = 90), stabilization = "onlyDynamicEdges") %>%
    visNetwork::visNodes(shapeProperties = list(interpolation = F)) %>%
    visNetwork::visEdges(smooth = F, color=list(color = "#848484", highlight = "#000000")) %>%
    # visNetwork::visConfigure(enabled = TRUE, filter = "physics", container = NULL) %>% # Currently not working, since I don't know how to set up the Container argument
    visNetwork::visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('node_id', nodes.nodes);
                    openBox('Bargraphs');
                    ;}",
                          deselectNode = "function(){
                    closeBox('Bargraphs');
                    }")
  # See: https://stackoverflow.com/questions/74768667/visnetwork-visevents-javascript-to-uncollapse-box
}
