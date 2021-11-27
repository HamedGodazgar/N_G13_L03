
#' dijkstra which calculates the shortest path from initial node in a graph.
#' @author Hamed
#' @description recieve graph and initial node and calculate shortest path.
#' @export dijkstra
#' @import igraph
#' @param graph data frame
#' @param init_node number
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#' @return dijkstra(graph,init_node)
#' @seealso \url{https://en.wikipedia.org/wiki/Graph_(mathematics}
#' @title dijkstra
#' @usage dijkstra(graph,init_node)



dijkstra <-
  function(graph,init_node){
    wiki_graph <-
      data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                 v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                 w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
    set.seed(500)
    el <- matrix(c(graph$v1,graph$v2), ncol=2)
    g <- igraph::graph_from_edgelist(el)
    oldpar <- par(mar = c(1,1,1,1))
    plot(g, edge.label = graph$w)
    par(oldpar)
    graph1 <- igraph::graph[!duplicated(graph$v1), ]
    g2 <- igraph::add_edges(igraph::make_empty_graph(length(graph1$w)), t(el[,1:2]), weight=graph$w)
    distances <- igraph::distances(g2, mode="out")
    distances[,init_node]
  }

