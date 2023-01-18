#' Network plot function - Build the network
#' Tutorials:
#' https://www.statworx.com/en/content-hub/blog/interactive-network-visualization-with-r
#' http://datastorm-open.github.io/visNetwork/shiny.html
#'
#' @param nodes dataframe containing nodes data, see visNetwork()
#' @param edges dataframe containing edges data, see visNetwork()
#' @param params list of parameters to control graph physics behaviour, see visIgraphLayout() and visPhysics()
#'
#' @import visNetwork
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()

NetworkPlot <- function(nodes, edges, params = list(layout = "layout_on_sphere",
                                                    physics = T,
                                                    solver = "hierarchicalRepulsion",
                                                    timestep = 0.5,
                                                    windX = 0,
                                                    windY = 0,
                                                    hierarchicalRepulsion = list(nodeDistance = 90,
                                                                                 centralGravity = 0.0,
                                                                                 springLength = 850,
                                                                                 springConstant = 0.01,
                                                                                 damping = 0.09,
                                                                                 avoidOverlap = 0))){

  # Create the initial plot
  Plot <- visNetwork::visNetwork(nodes, edges,
                main = list(text="CTU - PROJECTFACTS NETWORK", style="font-size:25px; color: #0073b7; text-align: center;"),
                submain = "Nodes width = Amount of time booked for each individual worker - Edges width = Amount of time booked overall (all workers combined)")

  # Select the layout (from UI)
  if (params$layout == "layout.reingold.tilford"){
    Plot <- Plot %>%
    visNetwork::visIgraphLayout(layout = "layout.reingold.tilford", physics = params$physics, circular = T, randomSeed = 1234, smooth = F)
  } else {
    Plot <- Plot %>%
    visNetwork::visIgraphLayout(layout = params$layout, physics = params$physics, randomSeed = 1234, smooth = F)}

  # Add the options
  if (params$solver == "barnesHut"){
    Plot <- Plot %>%
    visNetwork::visPhysics(solver = "barnesHut",
                           timestep = params$timestep,
                           wind = list(X = params$windX, Y = params$windY),
                           stabilization = "onlyDynamicEdges",
                           barnesHut = list(theta = params$barnesHut$theta,
                                            gravitationalConstant = params$barnesHut$gravitationalConstant,
                                            centralGravity = params$barnesHut$centralGravity,
                                            springLength = params$barnesHut$springLength,
                                            springConstant = params$barnesHut$springConstant,
                                            damping = params$barnesHut$damping,
                                            avoidOverlap = params$barnesHut$avoidOverlap))

  } else if (params$solver == "forceAtlas2Based") {

    Plot <- Plot %>%
      visNetwork::visPhysics(solver = "forceAtlas2Based",
                             timestep = params$timestep,
                             wind = list(X = params$windX, Y = params$windY),
                             stabilization = "onlyDynamicEdges",
                             barnesHut = list(theta = params$forceAtlas2Based$theta,
                                              gravitationalConstant = params$forceAtlas2Based$gravitationalConstant,
                                              centralGravity = params$forceAtlas2Based$centralGravity,
                                              springLength = params$forceAtlas2Based$springLength,
                                              springConstant = params$forceAtlas2Based$springConstant,
                                              damping = params$forceAtlas2Based$damping,
                                              avoidOverlap = params$forceAtlas2Based$avoidOverlap))

  } else if (params$solver == "repulsion") {
    Plot <- Plot %>%
      visNetwork::visPhysics(solver = "repulsion",
                             timestep = params$timestep,
                             wind = list(X = params$windX, Y = params$windY),
                             stabilization = "onlyDynamicEdges",
                             barnesHut = list(nodeDistance = params$repulsion$nodeDistance,
                                              centralGravity = params$repulsion$centralGravity,
                                              springLength = params$repulsion$springLength,
                                              springConstant = params$repulsion$springConstant,
                                              damping = params$repulsion$damping))

  } else if (params$solver == "hierarchicalRepulsion") {
    Plot <- Plot %>%
      visNetwork::visPhysics(solver = "hierarchicalRepulsion",
                             timestep = params$timestep,
                             wind = list(X = params$windX, Y = params$windY),
                             stabilization = "onlyDynamicEdges",
                             barnesHut = list(nodeDistance = params$hierarchicalRepulsion$nodeDistance,
                                              centralGravity = params$hierarchicalRepulsion$centralGravity,
                                              springLength = params$hierarchicalRepulsion$springLength,
                                              springConstant = params$hierarchicalRepulsion$springConstant,
                                              damping = params$hierarchicalRepulsion$damping,
                                              avoidOverlap = params$hierarchicalRepulsion$avoidOverlap))
  }

    # Add options and data
    Plot %>%
    visNetwork::visOptions(highlightNearest = T, nodesIdSelection = list(enabled = TRUE, main = "Select Worker/Project"),
                           selectedBy = list(variable="group", main = "Select State")) %>% #, multiple=T
      visNetwork::visNodes(shapeProperties = list(interpolation = F)) %>%
      visNetwork::visEdges(smooth = F, color=list(color = "#848484", highlight = "#000000")) %>%
      visNetwork::visEvents(selectNode = "function(nodes) {
                      Shiny.onInputChange('node_id', nodes.nodes);
                      openBox('Bargraphs');
                      ;}",
                            deselectNode = "function(){
                      closeBox('Bargraphs');
                      }")
    # See: https://stackoverflow.com/questions/74768667/visnetwork-visevents-javascript-to-uncollapse-box
}
