#' build_visnetwork
#'
#' This function will build a visnetwork object from and igraph object.
#'
#' @param input_igraph the igraph object
#' @param return_dataTable Whether or not to display the datatable below the graph
#' @param trim_edges If true, only edges that connnect to the query node will be displayed. Degree removed in this case.
#' @export
#' @details
#' #
#' @examples
#' build_vis_subnetwork(input_igraph = test_net, return_dataTable = T, trim_edges = F)

build_vis_subnetwork <- function(input_igraph, return_dataTable = T, trim_edges = F) {
  #######################################################

  fun1_return <- make_visnet(input_igraph)
  vis_obj <- fun1_return[[1]]
  edges <- fun1_return[[2]]
  nodes <- fun1_return[[3]]
  # x_pos <- V(input_igraph)$x_pos
  # y_pos <- V(input_igraph)$y_pos
  # l_full <- matrix(c(x_pos, y_pos), ncol=2)

  #######################################################

  if(return_dataTable){
    if(trim_edges){
      ui <- fluidPage(
        fluidRow(column(width = 3, textInput("gene_search", "Query Gene", nodes$id[1]), style = "height:75px"),
                 column(width = 7, "Gene Matches", verbatimTextOutput("search_opts"))),
        fluidRow(visNetworkOutput("sub_net"), style = "height:500px"),
        fluidRow(dataTableOutput("data_table"))
      )
    } else {
      ui <- fluidPage(
        fluidRow(column(width = 3, textInput("gene_search", "Query Gene", nodes$id[1]), style = "height:75px"),
                 column(width = 7, "Gene Matches", verbatimTextOutput("search_opts")),
                 column(width = 2, sliderInput("degree_opt", "Node Degree", 1, 2, 1, 1))),
        fluidRow(visNetworkOutput("sub_net"), style = "height:500px"),
        fluidRow(dataTableOutput("data_table"))
      )
    }
  } else {
    if(trim_edges){
      ui <- fluidPage(
        fluidRow(column(width = 3, textInput("gene_search", "Query Gene", nodes$id[1]), style = "height:75px"),
                 column(width = 7, "Gene Matches", verbatimTextOutput("search_opts"))),
        fluidRow(visNetworkOutput("sub_net"), style = "height:500px")
      )
    } else {
      ui <- fluidPage(
        fluidRow(column(width = 3, textInput("gene_search", "Query Gene", nodes$id[1]), style = "height:75px"),
                 column(width = 7, "Gene Matches", verbatimTextOutput("search_opts")),
                 column(width = 2, sliderInput("degree_opt", "Node Degree", 1, 2, 1, 1))),
        fluidRow(visNetworkOutput("sub_net"), style = "height:500px")
      )
    }
  }

  #######################################################

  server <- function(input, output) {
    observe({
      if(input$gene_search %in% nodes$id){
        if(trim_edges){
          degree_sel <- 1
        } else {
          degree_sel <- input$degree_opt
        }
        gene_search <- input$gene_search
        keep_edges <- as_data_frame(make_ego_graph(input_igraph, order = degree_sel, nodes = gene_search)[[1]])
        if(trim_edges){
          keep_from <- keep_edges$from %in% gene_search
          keep_from <- which(keep_from == T)
          keep_to <- keep_edges$to %in% gene_search
          keep_to <- which(keep_to == T)
          new_edges <- keep_edges[c(keep_from, keep_to),]
        } else {
          new_edges <- keep_edges
        }
        new_nodes <- unique(c(new_edges$from, new_edges$to))
        new_nodes <- as.data.frame(new_nodes)
        new_nodes$id <- new_nodes$new_nodes
        new_nodes$label <-  new_nodes$id
        new_nodes$color <- "#7fc97f"
        new_nodes$color[match(gene_search, new_nodes$id)] <- "#fdc086"
        return_plot <- visNetwork(new_nodes, new_edges)
        #         ind <- match(new_nodes$id, names(V(input_igraph)))
        #         l <- l_full[ind,]
        if(!is.null(return_plot)){
          if(trim_edges){
            output$sub_net <- renderVisNetwork({return_plot %>%
                visOptions(height = "500px")
              })
          } else {
            output$sub_net <- renderVisNetwork({return_plot %>%
                visOptions(height = "500px") %>%
                visIgraphLayout("layout_nicely")
            })
          }
          output$data_table <- renderDataTable(new_edges)
        }
      }
      gene_search <- input$gene_search
      return_gene <- grep(paste0("^", gene_search), nodes$id, value = T)
      if(length(return_gene) > 10){
        return_gene <- return_gene[1:10]
      }
      if(length(return_gene) == 0){
        output$search_opts <- renderText("Gene Not Found")
      } else {
        output$search_opts <- renderText(return_gene)
      }
    })
  }

  #######################################################
  shinyApp(ui = ui, server = server)
  #######################################################

}
