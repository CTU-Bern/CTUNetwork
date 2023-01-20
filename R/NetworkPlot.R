#' Network plot function - Build the network
#' Tutorials:
#' https://www.statworx.com/en/content-hub/blog/interactive-network-visualization-with-r
#' http://datastorm-open.github.io/visNetwork/shiny.html
#'
#' @param nodes dataframe containing nodes data, see visNetwork()
#' @param edges dataframe containing edges data, see visNetwork()
#' @param layout igraph layouts to compute coordinates, see visIgraphLayout()
#' @param physics activate/deactivate physics properties, see visIgraphLayout()
#' @param params optional parameters to manipulate physics properties, see visPhysics()
#'
#' @import visNetwork
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()

NetworkPlot <- function(nodes, edges, layout = "layout_on_sphere", physics = T){

  # params = list(solver = "hierarchicalRepulsion",
  #               timestep = 0.5,
  #               windX = 0,
  #               windY = 0,
  #               hierarchicalRepulsion = list(nodeDistance = 90,
  #                                            centralGravity = 0.0,
  #                                            springLength = 850,
  #                                            springConstant = 0.01,
  #                                            damping = 0.09,
  #                                            avoidOverlap = 0))

  # Create the initial plot
  Plot <- visNetwork::visNetwork(nodes, edges,
                main = list(text="CTU - PROJECTFACTS NETWORK", style="font-size:25px; color: #0073b7; text-align: center;"),
                submain = "Nodes width = Amount of time booked for each individual worker - Edges width = Amount of time booked overall (all workers combined)")

  # Select the layout (from UI)
  if (layout == "layout.reingold.tilford"){
    Plot <- Plot %>%
    visNetwork::visIgraphLayout(layout = "layout.reingold.tilford", physics = physics, circular = T,  randomSeed = 1234, smooth = F)
  } else {
    Plot <- Plot %>%
    visNetwork::visIgraphLayout(layout = layout, physics = physics, randomSeed = 1234, smooth = F)}

  # # Add the options
  # if (params$solver == "barnesHut" | params$solver == "forceAtlas2Based"){
  #   Plot <- Plot %>%
  #   visNetwork::visPhysics(solver = params$solver,
  #                          timestep = params$timestep,
  #                          wind = list(X = params$windX, Y = params$windY),
  #                          stabilization = "onlyDynamicEdges",
  #                          barnesHut = list(theta = params[[params$solver]]$theta,
  #                                           gravitationalConstant = params[[params$solver]]$gravitationalConstant,
  #                                           centralGravity = params[[params$solver]]$centralGravity,
  #                                           springLength = params[[params$solver]]$springLength,
  #                                           springConstant = params[[params$solver]]$springConstant,
  #                                           damping = params[[params$solver]]$damping,
  #                                           avoidOverlap = params[[params$solver]]$avoidOverlap))
  #
  # } else if (params$solver == "repulsion") {
  #   Plot <- Plot %>%
  #     visNetwork::visPhysics(solver = "repulsion",
  #                            timestep = params$timestep,
  #                            wind = list(X = params$windX, Y = params$windY),
  #                            stabilization = "onlyDynamicEdges",
  #                            barnesHut = list(nodeDistance = params$repulsion$nodeDistance,
  #                                             centralGravity = params$repulsion$centralGravity,
  #                                             springLength = params$repulsion$springLength,
  #                                             springConstant = params$repulsion$springConstant,
  #                                             damping = params$repulsion$damping))
#
#   } else if (params$solver == "hierarchicalRepulsion") {
#     Plot <- Plot %>%
#       visNetwork::visPhysics(solver = "hierarchicalRepulsion",
#                              timestep = params$timestep,
#                              wind = list(X = params$windX, Y = params$windY),
#                              stabilization = "onlyDynamicEdges",
#                              barnesHut = list(nodeDistance = params$hierarchicalRepulsion$nodeDistance,
#                                               centralGravity = params$hierarchicalRepulsion$centralGravity,
#                                               springLength = params$hierarchicalRepulsion$springLength,
#                                               springConstant = params$hierarchicalRepulsion$springConstant,
#                                               damping = params$hierarchicalRepulsion$damping,
#                                               avoidOverlap = params$hierarchicalRepulsion$avoidOverlap))
#   }

  # Add options and data
  Plot %>%
    visNetwork::visPhysics(solver = "hierarchicalRepulsion",
                           hierarchicalRepulsion = list(nodeDistance = 90,
                                                        springLength = 850)) %>%
    visNetwork::visOptions(highlightNearest = T, nodesIdSelection = list(enabled = TRUE, main = "Select Worker/Project"),
                           selectedBy = list(variable="group", main = "Select State")) %>% #, multiple=T
    visNetwork::visNodes(shapeProperties = list(interpolation = F)) %>%
    visNetwork::visEdges(smooth = F, color=list(color = "#848484", highlight = "#000000")) %>%

    # Enable to open the box based on node's press
    # See: https://stackoverflow.com/questions/74768667/visnetwork-visevents-javascript-to-uncollapse-box
    visNetwork::visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('node_id', nodes.nodes);
                    openBox('Bargraphs');
                    ;}",
                          deselectNode = "function(){
                    closeBox('Bargraphs');
                    }")
}
