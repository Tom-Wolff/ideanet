#Feb 12, 2022

## Setup libraries and seed ----

#check if seed file exists,and if it does not, create



## Create Fluid Page ----
#App start page, start of fluid page, creation of initial output
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  div(style = "padding: 1px 0px; width: '100%'",
      titlePanel(
        title = "",
        windowTitle = "IDEANet Visualizer"
      )
  ),


  ### Upload node and edge data ----
  #code to upload node data
  shiny::navbarPage(
    title = "IDEANet Visualizer",
    shiny::tabPanel(
      "Upload",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Upload Files",
          shiny::sidebarPanel(
            shiny::uiOutput('select_file_type_edges'),
            shiny::uiOutput('edge_format'),
            shiny::checkboxInput("edge_names", tags$b("Does the file have a first ID column"), FALSE),
            shiny::checkboxInput("edge_header", tags$b("Does the file have a header?"), TRUE),
            tags$p(shiny::span("Large datasets may take a few seconds to render.", style = "color:red")),
            tags$p(shiny::HTML("<b>Continue</b> on to process the data before visualizing it.")),
            shiny::fileInput(
              'raw_edges', "Upload Edge Data", multiple = FALSE,
              buttonLabel = "Browse...", placeholder = "No file selected"
            ),
            shiny::checkboxInput('nodes_exist', tags$b("Does the dataset have a nodelist?"),FALSE),
            shiny::conditionalPanel(
              condition = 'input.nodes_exist',
              shiny::fileInput(
                'raw_nodes', "Upload Node Data", multiple = FALSE,
                buttonLabel = "Browse...", placeholder = "No file selected"
              ),
              tags$p(shiny::span("Large datasets may take a few seconds to render.", style = "color:red")))


          ),
          shiny::mainPanel(
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Edge Data",
                style = "overflow-x: auto;",
                shiny::dataTableOutput('edge_raw_upload')
              ),
              shiny::tabPanel(
                "Node Data",
                style = "overflow-x: auto;",
                shiny::dataTableOutput('node_raw_upload')
              )
            )
          )
        )
      )
    ),
    ### Process node and edge data ----
    #Code to process Node Data
    shiny::tabPanel(
      "Process",
      shiny::tabsetPanel(
        id = "processtabs",
        type = "tabs",
        #Code to process edge Data
        shiny::tabPanel(
          "Process Edge Data ",
          shiny::sidebarPanel(
            shiny::uiOutput("edge_in"),
            shiny::uiOutput("edge_out"),
            shiny::uiOutput("edge_weight"),
            shiny::checkboxInput("direction_toggle", tags$b("Check if the graph is directed"), FALSE),
            shiny::uiOutput('multi_relational_toggle'),
            shiny::conditionalPanel(
              condition = "input.multi_relational_toggle",
              shiny::uiOutput('relational_column')
            ),
            tags$p(shiny::span("Questions with an asterisk are required.", style = "color:red")),
            tags$p(shiny::HTML("<b>Process</b> the edge data by assigning the columns to their function.")),
            tags$p(shiny::HTML("If the graph is undirected, the order of sender and alter ID columns doesn't matter.")),
          ),
          shiny::mainPanel(
            style = "overflow-x: auto;",
            shiny::dataTableOutput('edge_processed')
          )
        ))
    ),
    ### Visualize network and user options ----
    shiny::tabPanel(
      "Visualize",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          style = "height: 90vh; overflow-y: auto;",
          shiny::uiOutput("set_seed"),
          shiny::br(),
          shiny::uiOutput("save_image"),
          shiny::br(),
          shiny::uiOutput('image_type'),
          shiny::uiOutput("plot_scalar"),
          shiny::checkboxInput("isolate_toggle", tags$b("Remove isolates?"), FALSE),
          shiny::checkboxInput("simplify_toggle", tags$b("Remove self-loops and duplicate edges?"), FALSE),
          shiny::uiOutput("layout_picker"),
          tags$p(shiny::HTML("<u>Node Features</u>")),
          shiny::uiOutput("node_size_method"),
          shiny::uiOutput("node_size_scalar"),
          shiny::uiOutput("community_detection"),
          shiny::uiOutput("palette_choice"),
          shiny::uiOutput("uniform_choice"),
          tags$p(shiny::HTML("<u>Edge Features</u>")),
          shiny::conditionalPanel(
            condition = "input.multi_relational_toggle",
            shiny::uiOutput('filter_relation_type'),
            shiny::uiOutput('toggle_relational_coloring')
          ),
          shiny::uiOutput('interactive'),
          shiny::uiOutput('edge_weight_method')
          #shiny::uiOutput('edge_weight_scalar'),
        ),
        shiny::mainPanel(
          tags$style(shiny::HTML("
      #legendcol {
        background-color: transparent;
      }

      #legend {
        background-color:transparent;
      }")),
          column(8,
                 shiny::uiOutput("network_ui")
          ),
          column(4,
                 shiny::conditionalPanel(
                   condition = "input.palette_choice != 'Uniform'",
                   shiny::plotOutput("legend")),
                 id = "legendcol"
          )
        )
      )),
    ### Network Metrics ----
    shiny::tabPanel(
      "Network Summary Graphs",
      shiny::sidebarPanel(
        shiny::uiOutput("measure_chooser"),
        shiny::conditionalPanel(
          condition = "input.measure_chooser == 'System' & input$multi_relational_toggle == TRUE",
          shiny::uiOutput('system_level_chooser')
        ),
        shiny::conditionalPanel(
          condition = "input.measure_chooser == 'Node' & input$multi_relational_toggle == TRUE",
          shiny::uiOutput('node_level_chooser')
        ),
      ),
      shiny::mainPanel(
        shiny::plotOutput( 'stats1')
      )
    ),

    ### Networks DataTable ----
    shiny::tabPanel(
      "Node-Level Measures",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          style = "height: 90vh; overflow-y: auto;",
          shiny::uiOutput('show_vars'),
          shiny::br(),
          shiny::checkboxInput('graph_wanted', tags$b("Do you want to graph a variable?"), FALSE),
          shiny::conditionalPanel(
            condition = "input.graph_wanted == true",
            shiny::uiOutput('data_table_vis_var')
          ),
          shiny::conditionalPanel(
            condition = "input.graph_wanted == true & input.var_wanted == false",
            shiny::uiOutput('data_table_vis_type')
          ),
          shiny::conditionalPanel(
            condition = "input.graph_wanted == true",
            shiny::checkboxInput('var_wanted', tags$b("Add second variable? (optional)"),FALSE)
          ),
          shiny::conditionalPanel(
            condition = "input.var_wanted == true & input.graph_wanted == true",
            shiny::uiOutput('data_table_vis_var2')
          ),
          shiny::downloadButton("downloadTable", "Download",icon = shiny::icon("download")),
        ),
        shiny::mainPanel(
          style = "overflow-x: auto;",
          DT::DTOutput('statistics_table'),
          shiny::HTML("<br><br>"),
          shiny::plotOutput('statistics_graph')
        )
      )
    ),
    ### Analysis tab ----
    # shiny::tabPanel(
    #   "Advanced Analysis Modules",
    #   shiny::uiOutput('analysis_chooser'),
    #   shiny::sidebarLayout(
    #     shiny::sidebarPanel(
    #       shiny::tabsetPanel(
    #         id = "analytic_panels",
    #         type = "hidden",
    #         shiny::tabPanelBody(
    #           "QAP",
    #           tags$p(shiny::HTML("<u>QAP Setup Options</u>")),
    #           tags$p(shiny::span("You must choose analysis type and variable as paired selections.", style = "color:red")),
    #           shiny::uiOutput('method_chooser'),
    #           shiny::uiOutput('var_cols'),
    #           tags$p(shiny::span("Method selections:", style = "color:black")),
    #           shiny::verbatimTextOutput("method_list"),
    #           tags$p(shiny::span("Variable selections:", style = "color:black")),
    #           shiny::verbatimTextOutput("var_list"),
    #           shiny::uiOutput('run_QAP_setup'),
    #           tags$p(shiny::HTML("<u>QAP Run Options</u>")),
    #           shiny::uiOutput('qap_run_dependent'),
    #           shiny::uiOutput('qap_run_choices'),
    #           shiny::uiOutput('run_QAP_model')
    #         ),
    #         shiny::tabPanelBody(
    #           "Role Detection",
    #           shiny::uiOutput('select_role_type'),
    #           shiny::uiOutput('select_role_viz'),
    #           shiny::uiOutput('role_det_min'),
    #           shiny::uiOutput('role_det_max'),
    #           shiny::uiOutput('min_cluster_size'),
    #           shinycssloaders::withSpinner(
    #             shiny::uiOutput('run_role_detect')
    #           )
    #         ))),
    #     shiny::mainPanel(
    #       shiny::conditionalPanel(
    #         condition = "input.analysis_chooser == 'Role Detection'",
    #         tags$h3(shiny::HTML("<b>Visualize Role Detection Output</b>")),
    #         shinycssloaders::withSpinner(
    #           shiny::plotOutput('role_viz')
    #         )
    #       )
    #     )
    #   ))
    shiny::tabPanel(
      "Advanced Analysis Modules",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "QAP",
          shiny::sidebarPanel(
            tags$p(HTML("<u>QAP Setup Options</u>")),
            tags$p(span("You must choose analysis type and variable as paired selections.", style = "color:red")),
            shiny::uiOutput('method_chooser'),
            shiny::uiOutput('var_cols'),
            tags$p(span("Method selections:", style = "color:black")),
            shiny::verbatimTextOutput("method_list"),
            tags$p(span("Variable selections:", style = "color:black")),
            shiny::verbatimTextOutput("var_list"),
            shiny::uiOutput('run_QAP_setup'),
            tags$p(HTML("<u>QAP Run Options</u>")),
            shiny::uiOutput('qap_run_dependent'),
            shiny::uiOutput('qap_run_choices'),
            shiny::uiOutput('run_QAP_model')
          ),
          shiny::mainPanel(
            style = "overflow-x: auto;",
            DT::DTOutput('qap_table')
          )
        ),
        shiny::tabPanel(
          "Role Detection",
          shiny::sidebarPanel(
            shiny::uiOutput('select_role_type'),
            shiny::uiOutput('select_role_viz'),
            shiny::uiOutput('role_det_min'),
            shiny::uiOutput('role_det_max'),
            shiny::uiOutput('min_cluster_size'),
            shinycssloaders::withSpinner(
              shiny::uiOutput('run_role_detect')
            )
          ),
          shiny::mainPanel(
            style = "overflow-x: auto;",
            tags$h3(HTML("<b>Visualize Role Detection Output</b>")),
            shinycssloaders::withSpinner(
              plotOutput('role_viz')
            )
          )),
        shiny::tabPanel(
          "CHAMP",
          shiny::sidebarPanel(
            tags$p(HTML("<u>CHAMP Algorithm Options</u>")),
            shiny::numericInput("champ_n_runs", "Number of runs", value = 1000, min = 100, max = 5000),
            shiny::numericInput("champ_gamma_min", "Minimum gamma value", value = 0, min = 0, max = 10),
            shiny::numericInput("champ_gamma_max", "Maximum gamma value", value = 3, min = 1, max = 10),
            shiny::numericInput("champ_seed", "Random seed (for reproducibility)", value = 12345),
            shiny::actionButton("run_champ", "Run CHAMP Algorithm",
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            tags$hr(),
            tags$p(HTML("<u>CHAMP Map Options</u>")),
            shiny::uiOutput("champ_map_label_input"),
            shinycssloaders::withSpinner(
              shiny::uiOutput('run_champ_map')
            )
          ),
          shiny::mainPanel(
            style = "overflow-x: auto;",
            shinycssloaders::withSpinner(
              shiny::plotOutput('champ_plot', height = "400px")
            ),
            shiny::HTML("<br>"),
            shinycssloaders::withSpinner(
              shiny::plotOutput('champ_map_plot', height = "400px")
            ),
            shiny::HTML("<br>"),
            shiny::dataTableOutput('champ_summary_table')
          )
        )
      ))
  )
)

## Server Function ----

#Create server
server <- function(input, output, session) {
  if(exists('edge_data')) {
    rm(edge_data)
  }
  #remove existing edgelist if rerunning and in environment
  if(exists('node_data')) {
    rm(node_data)
  }
  if(!file.exists("temp/seed.txt")) {
    #read seed file o, create if not written
    print('seed not found')
    print(getwd())
    writeLines("999", "temp/seed.txt")
  }
  #remove existing edgelist if rerunning and in environment

  if(file.exists('inst/apps/ideanetViz/temp/plot_output.png')) {
    unlink('inst/apps/ideanetViz/temp/plot_output.png')
  }
  library(magrittr)

  ### Upload Node  and Edge Data ----

  # Create a placeholder for Node Data; if it isn't uploaded, stored
  # as `NULL` to ensure compatibility downstream

  #Upload Node Data


  output$select_file_type_edges <- shiny::renderUI({
    shiny::selectInput('select_file_type_edges', label = "Choose file type", choices = c('csv', 'excel'))
  })

  output$edge_format <- shiny::renderUI({
    shiny::selectInput('edge_format', label = "Choose edge format", choices = c('Edgelist', 'Adjacency Matrix'))
  })


  edge_data <- shiny::reactive({

    shiny::req(input$raw_edges)

    # If "Edgelist" is selected
    if (input$edge_format == "Edgelist") {
      # Reading CSV
      if (input$select_file_type_edges == "csv") {
        # network edgelist
        read.csv(input$raw_edges$datapath, header = input$edge_header)
        # Reading Excel
      } else {
        if(stringr::str_detect(input$raw_edges$datapath, "xlsx$")) {
          readxl::read_xlsx(path = input$raw_edges$datapath, col_names = input$edge_header)
        } else {
          readxl::read_xls(path = input$raw_edges$datapath, col_names = input$edge_header)
        }
      }
      # If "Adjacency Matrix" is selected
    } else {


      as.data.frame(netread(path = input$raw_edges$datapath,
                            filetype = input$select_file_type_edges,
                            col_names = input$edge_header,
                            row_names = input$edge_names,
                            format = "adjacency_matrix")$edgelist)

    }
    # if (input$nodes_exist & !is.null(input$raw_nodes) & !is.null(input$raw_edges)) {
    # netread(
    #   path = input$raw_edges$datapath,
    #   filetype = input$select_file_type_edges,
    #   nodelist = input$raw_nodes$datapath,
    #   col_names = input$edge_header,
    #   row_names = input$edge_names,
    #   format = input$edge_format,
    #   net_name = "network",
    #   missing_code = 99999
    # )
    # }
    # else if (!is.null(input$raw_edges)) {
    #   netread(
    #     path = input$raw_edges$datapath,
    #     filetype = input$select_file_type_edges,
    #     format = input$edge_format,
    #     col_names = input$edge_header,
    #     row_names = input$edge_names,
    #     nodelist = NULL,
    #     net_name = "network",
    #     missing_code = 99999
    #   )
    # }
    # as.data.frame(network_edgelist)
  })

  node_data <- shiny::reactive({
    # path_edges = input$raw_edges$datapath
    # path_nodes = input$raw_nodes$datapath
    # as.data.frame(network_nodelist)
    print(exists("raw_nodes"))
    test <- input$raw_nodes$datapath
    print(test)

    # If `raw_nodes` path is defined...
    if (is.character(test)) {
      # Reading CSV
      if (input$select_file_type_edges == "csv") {
        # network edgelist
        read.csv(input$raw_nodes$datapath, header = input$edge_header)
        # Reading Excel
      } else {
        if(stringr::str_detect(input$raw_nodes$datapath, "xlsx$")) {
          readxl::read_xlsx(path = input$raw_nodes$datapath, col_names = input$edge_header)
        } else {
          readxl::read_xls(path = input$raw_nodes$datapath, col_names = input$edge_header)
        }
      }
      # Otherwise store as `NULL`
    } else {
      NULL
    }
  })

  #Display Node Data
  output$node_raw_upload <- shiny::renderDataTable({
    print(class(node_data()))
    shiny::validate(
      shiny::need(!is.null(node_data()), 'Upload Node Data!')
    )
    print("display test")
    print(node_data())
    node_data()
  })


  #Display Edge Data
  output$edge_raw_upload <- shiny::renderDataTable({
    shiny::validate(
      shiny::need(input$raw_edges, 'Upload Edge Data!'),
    )
    edge_data()
  })


  ### Process edge and node data ----
  # Redisplay Datatables
  output$node_processed <- shiny::renderDataTable({
    # shiny::validate(
    #   shiny::need(input$raw_nodes, 'Upload Node Data!'),
    # )
    node_data()
  })
  output$edge_processed <- shiny::renderDataTable({
    # shiny::validate(
    #   shiny::need(input$raw_edges, 'Upload Edge Data!'),
    # )
    edge_data()
  })

  shiny::observeEvent(input$raw_nodes, {
    shiny::insertTab(inputId = "processtabs",
                     shiny::tabPanel("Process Node Data",
                                     shiny::sidebarPanel(
                                       shiny::uiOutput("node_ids"),
                                       shiny::uiOutput("node_labels"),
                                       shiny::uiOutput("node_factor"),
                                       shiny::uiOutput("node_numeric"),
                                       tags$p(shiny::span("Questions with an asterisk are required.", style = "color:red")),
                                       tags$p(shiny::HTML("<b>Process</b> the node data by assigning the columns to their function.")),
                                       tags$p(shiny::HTML("The <b>node</b> <b>ids</b> should reflect ids in the edge list. It's required to correctly link the node attributes.")),
                                     ),
                                     shiny::mainPanel(
                                       style = "overflow-x: auto;",
                                       shiny::dataTableOutput('node_processed')
                                     )
                     ), target = "Process Edge Data ")
  })
  #Node Processing Options

  output$node_ids <- shiny::renderUI({

    shiny::selectInput(inputId = "node_id_col", label = "Column with node ids*", choices = append("Empty",colnames(node_data())), selected = 'N/A', multiple = FALSE)

  })
  output$node_labels <- shiny::renderUI({

    shiny::selectInput(inputId = "node_label_col", label = "Column with node labels", choices = append("Empty",colnames(node_data())), selected = "Empty", multiple = FALSE)

  })
  output$node_factor <- shiny::renderUI({

    shiny::selectInput(inputId = "node_factor_col", label = "Column with groups", choices = append("Empty",colnames(node_data())), selected = NULL, multiple = TRUE)

  })
  output$node_numeric <- shiny::renderUI({

    shiny::selectInput(inputId = "node_numeric_col", label = "Column with node sizes", choices = append("Empty",colnames(node_data())), multiple = FALSE)

  })



  nodes_used <- shiny::reactive({
    print('here nodes used')
    if(!is.null(node_data())) {
      temp <- FALSE
      temp
    }
    else {
      NULL
    }
  })

  nodes_done <- shiny::reactiveVal(TRUE)

  observeEvent(input$raw_nodes, {
    print('oberve event nodes')
    nodes_done(NULL)
  })

  observeEvent(input$node_id_col, {
    print('oberve event nodes')
    if(input$node_id_col != 'Empty') {
      nodes_done(TRUE)
    }
    else{
      nodes_done(NULL)
    }
  })



  #Edge Processing Options
  output$edge_in <- shiny::renderUI({
    shiny::selectInput(inputId = "edge_in_col", label = "Column with sender IDs*", choices = append("Empty",colnames(edge_data())), selected = 'N/A', multiple = FALSE)
  })
  output$edge_out <- shiny::renderUI({
    shiny::selectInput(inputId = "edge_out_col", label = "Column with the alter IDs*", choices = append("Empty",colnames(edge_data())), selected = 'N/A', multiple = FALSE)
  })
  output$edge_weight <- shiny::renderUI({
    shiny::selectInput(inputId = "edge_weight_col", label = "Column with edge weights", choices = append("Empty",colnames(edge_data())), selected = NULL, multiple = FALSE)
  })

  output$multi_relational_toggle <- shiny::renderUI({
    shiny::checkboxInput("multi_relational_toggle", tags$b("Check if the graph is multirelational"), FALSE)
  })

  output$relational_column <- shiny::renderUI({
    shiny::selectInput('relational_column', label = "Column with relation type", choices = append("Empty",colnames(edge_data())), selected = 'Empty', multiple = FALSE)
  })


  edges_done <- shiny::reactiveVal(0)

  observeEvent(input$edge_in_col, {
    print('observe event edges')
    temp <- edges_done()+1
    edges_done(temp)
  })

  observeEvent(input$edge_out_col, {
    temp <- edges_done()+1
    edges_done(temp)
  })



  ### Network Generation ----

  #Edge Weight Setting
  initial_edge <- shiny::reactive({
    if (input$edge_weight_col == 'Empty') {
      NULL
    } else {
      temp <- edge_data()[,input$edge_weight_col]
      temp
    }
  })


  #### Create network 0 to run IDEANet ----
  net0 <- shiny::reactive({
    type_ret <- c()
    if (is.null(input$relational_column)) {
      type_ret = NULL
    } else if (input$relational_column == "Empty") {
      type_ret = NULL } else {
        type_ret <- edge_data()[,input$relational_column]
      }
    if (!is.null(input$raw_nodes) & shiny::isTruthy(input$node_id_col))  {
      if (input$node_id_col != "Empty") {
        print('started netwrite 1')
        list2env(netwrite(data_type = c('edgelist'), adjacency_matrix=FALSE,
                          adjacency_list=FALSE, nodelist=node_data(),
                          node_id=input$node_id_col,
                          i_elements=edge_data()[,input$edge_in_col],
                          j_elements=edge_data()[,input$edge_out_col],
                          weights=initial_edge(),
                          type=type_ret,
                          missing_code=99999, weight_type='frequency',
                          directed=input$direction_toggle,
                          net_name='init_net',
                          shiny=TRUE),
                 .GlobalEnv)
        print('processed netwrite')
        init_net

      } else {
        print('started netwrite 2')
        list2env(netwrite(data_type = c('edgelist'), adjacency_matrix=FALSE,
                          adjacency_list=FALSE,
                          i_elements=edge_data()[,input$edge_in_col],
                          j_elements=edge_data()[,input$edge_out_col],
                          weights=initial_edge(),
                          type=type_ret,
                          missing_code=99999, weight_type='frequency',
                          directed=input$direction_toggle,
                          net_name='init_net',shiny=TRUE),
                 .GlobalEnv)
        print('processed netwrite')
        init_net
      }
    } else {
      print('started netwrite 3')
      list2env(netwrite(data_type = c('edgelist'), adjacency_matrix=FALSE,
                        adjacency_list=FALSE,
                        i_elements=edge_data()[,input$edge_in_col],
                        j_elements=edge_data()[,input$edge_out_col],
                        weights=initial_edge(),
                        type=type_ret,
                        missing_code=99999, weight_type='frequency',
                        directed=input$direction_toggle,
                        net_name='init_net',shiny=TRUE),
               .GlobalEnv)
      print('processed netwrite')
      init_net
    }
  })

  #### Add node attributes ----

  # Joining all node_data to ideanet to preserve ordering


  ### MAKE SURE TO ADD CHECK FOR PROCESSING BACK IN!!!!



  # join node data with nodelist
  nodelist2 <- shiny::reactive({

    if (!is.null(node_data())) {
      node_data3 <- node_data()
      node_data3[,input$node_id_col] <- as.character(node_data3[,input$node_id_col])
      node_measures <- node_measures %>% dplyr::mutate(id = as.character(id))
      node_measures <- node_measures %>%
        dplyr::left_join(node_data3)
      node_measures %>% dplyr::mutate(id = as.double(id))
    } else {
      node_measures
    }
  })

  #Run Community detection
  nodelist3 <- shiny::reactive({
    shiny::validate(
      shiny::need(input$raw_edges, 'Upload Edge Data!'),
    )

    net <- net0()

    nodes <- nodelist2()
    print('started community detection')
    list2env(comm_detect(net, shiny  = TRUE),
             .GlobalEnv)
    print('finished community detection')
    memberships <- memberships %>%
      dplyr::mutate_all(~replace(., is.na(.), 0))
    #comm_members_net$id <- as.character(comm_members_net$id)
    nodes <- nodes %>%
      dplyr::left_join(memberships, by = "id")
    if (ran_toggle_role_detect$x==1) {
      nodes <- nodes %>%
        dplyr::left_join(cluster_assignments %>% dplyr::select('best_fit','id'), by = "id")
    }
    if (ran_toggle_champ_map() == 1) {
      print(input$direction_toggle)
      nodes <- nodes %>%
        dplyr::left_join(champ_results()$shiny_partitions, by = "id")

    }
    as.data.frame(nodes)
  })

  #Add labels in network 1
  net1 <- shiny::reactive({
    net <- net0()
    # Testing to see if this will handle cases where nodelists aren't added
    node_label_col <- input$node_label_col
    if (is.null(node_label_col)) {
      node_label_col <- "Empty"
    }

    # Adding Vector Labels
    # if (input$node_label_col != 'Empty') {
    if (node_label_col != 'Empty') {
      igraph::V(net)$label <- nodelist3()[,input$node_label_col]
    } else {
      igraph::V(net)$label <- nodelist3()[,'id']
    }

    #Add group elements (manually selected, automatically applied)
    if (!is.null(input$node_factor_col)) {
      if (length(input$node_factor_col) > 2) {
        igraph::V(net)$group <- nodelist3() %>% dplyr::pull(input$node_factor_col[1])
      } else {
        igraph::V(net)$group <- nodelist3() %>% dplyr::pull(input$node_factor_col[1])
      }} else {
        igraph::V(net)$group <- rep("A", length(nodelist3()$id))
      }
    net

  })


  #### Set Node Size ----
  output$node_size_method <- shiny::renderUI({
    shiny::selectInput(inputId = "node_size_method", label = "Node size method", choices = c("Uniform",
                                                                                             # "Node Data",
                                                                                             "Degree", "Eigen Centrality", "Betweenness Centrality"), selected = "Uniform", multiple = FALSE)
  })

  output$node_size_scalar <- shiny::renderUI({
    shiny::sliderInput(inputId = "node_scalar_value", label = "Node size scalar", min = 0, max = 4, value =2, step = .1)
  })

  net2 <- shiny::reactive({
    net <- net1()

    rescale2 = function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
    if (input$node_size_method == "Uniform") {
      igraph::V(net)$size <- rep(10, length(igraph::V(net)$label)) * input$node_scalar_value
    } else if (input$node_size_method == "Node Data") {
      if (shiny::isTruthy(input$node_numeric_col)) {
        if (input$node_numeric_col == "Empty") {
          igraph::V(net)$size <- rep(10, length(V(net)$label)) * input$node_scalar_value
        } else {
          igraph::V(net)$size <- rescale2(nodelist3()[,input$node_numeric_col], min(nodelist3()[,input$node_numeric_col]), max(nodelist3()[,input$node_numeric_col]), 3,17) * input$node_scalar_value
        }
      } else {
        igraph::V(net)$size <- rep(10, length(V(net)$label)) * input$node_scalar_value
      }
    } else if (input$node_size_method == "Degree") {
      igraph::V(net)$size <- rescale2(igraph::degree(net, mode = "all"), min(igraph::degree(net, mode= "all")), max(igraph::degree(net, mode= "all")), 3,17) * input$node_scalar_value
    } else if (input$node_size_method == "Eigen Centrality") {
      igraph::V(net)$size <- rescale2(igraph::eigen_centrality(net)$vector, min(igraph::eigen_centrality(net)$vector), max(igraph::eigen_centrality(net)$vector), 3,17) * input$node_scalar_value
    } else if (input$node_size_method == "Betweenness Centrality") {
      igraph::V(net)$size <- rescale2(igraph::centr_betw(net)$res, min(igraph::centr_betw(net)$res), max(igraph::centr_betw(net)$res), 3,17) * input$node_scalar_value
    }
    net
  })



  #### Handle output community detection ----
  output$community_detection <- shiny::renderUI({

    if (ran_toggle_role_detect$x==1 & ran_toggle_champ_map() == 0) {
      vals <- nodelist3() %>%
        dplyr::select(dplyr::ends_with('membership'),
                      'cp_cluster',
                      'lc_cluster',
                      'best_fit') %>%
        dplyr::select(-c("strong_membership", "weak_membership")) %>%
        colnames()
    } else if (ran_toggle_role_detect$x==0 & ran_toggle_champ_map() == 1) {
      vals <- nodelist3() %>%
        dplyr::select(dplyr::ends_with('membership'),
                      'cp_cluster',
                      'lc_cluster',
                      dplyr::starts_with('champ')) %>%
        dplyr::select(-c("strong_membership", "weak_membership")) %>%
        colnames()
    } else if (ran_toggle_role_detect$x==1 & ran_toggle_champ_map() == 1) {
      vals <- nodelist3() %>%
        dplyr::select(dplyr::ends_with('membership'),
                      'cp_cluster',
                      'lc_cluster',
                      dplyr::starts_with('champ'),
                      'best_fit') %>%
        dplyr::select(-c("strong_membership", "weak_membership")) %>%
        colnames()
    } else {
      vals <- nodelist3() %>%
        dplyr::select(dplyr::ends_with('membership')) %>%
        dplyr::select(-c("strong_membership", -"weak_membership")) %>%
        colnames()}

    all_choices = c("None", input$node_factor_col, vals[!vals %in% "id"])

    shiny::selectInput(inputId = "community_input", label = "Node Coloring", choices = all_choices, selected = "None", multiple = FALSE)
  })

  # Create network to handle community attributes
  net6 <- shiny::reactive({
    net <- net2()
    if (!(input$community_input == "None")) {
      val <-  input$community_input
      igraph::V(net)$communities <- nodelist3()[,val]
      net
    } else {
      net
    }
  })

  #### Choose colors ----
  output$palette_choice <- shiny::renderUI({
    shiny::selectInput(inputId = "palette_input", label = "Color Palette", choices = c("Uniform", "Rainbow", "Heat", "Terrain", "Topo", "CM"), selected = "Uniform", multiple = FALSE)
  })

  output$uniform_choice <- shiny::renderUI({
    shiny::textInput(inputId = "uniform_hex_code", label = "Uniform color HEX", value = "#ADD8E6")
  })



  #### Set node colors ----
  #Get number of necessary colors based on input
  number_of_color_groups <- shiny::reactive({
    if (input$community_input != "None") {
      length(unique(igraph::V(net6())$communities))
    }
    else {
      length(unique(igraph::V(net6())$group))
    }
  })

  #Generate Color patterns/hues
  color_generator <- shiny::reactive({
    if (input$palette_input == 'Uniform') {
      rep(input$uniform_hex_code, number_of_color_groups())
    } else if (input$palette_input == 'Rainbow') {
      if (number_of_color_groups() == 1) {
        grDevices::rainbow(10)[5]
      } else {
        grDevices::rainbow(number_of_color_groups())
      }
    } else if (input$palette_input == 'Heat') {
      if (number_of_color_groups() == 1) {
        grDevices::heat.colors(10)[5]
      } else {
        grDevices::heat.colors(number_of_color_groups())
      }
    } else if (input$palette_input == 'Terrain') {
      if (number_of_color_groups() == 1) {
        grDevices::terrain.colors(10)[5]
      } else {
        grDevices::terrain.colors(number_of_color_groups())
      }
    } else if (input$palette_input == 'Topo') {
      if (number_of_color_groups() == 1) {
        grDevices::topo.colors(10)[5]
      } else {
        grDevices::topo.colors(number_of_color_groups())
      }
    } else if (input$palette_input == 'CM') {
      if (number_of_color_groups() == 1) {
        grDevices::cm.colors(10)[5]
      } else {
        grDevices::cm.colors(number_of_color_groups())
      }
    }
  })

  #match by color or groups using groups or community
  color_matcher <- shiny::reactive({
    if (input$community_input != "None") {
      groups <- unique(igraph::V(net6())$communities)
    } else {
      groups <- unique(igraph::V(net6())$group)
    }
    colrs <- color_generator()
    data.frame(groups, colrs)
  })

  #NOTE: actual application to the network condained in edge coloring

  #### Set edge colors (type of edge relational) ----
  #Get number of necessary colors based on input
  number_of_color_groups_edges <- shiny::reactive({
    if (input$multi_relational_toggle == TRUE & input$relational_column != "Empty") {
      length(unique(edge_data()[,input$relational_column]))
    }
    else {
      1
    }
  })

  #Generate Color patterns/hues

  # idea for grabbing user's inputted color palette: set value equal to whatever
  # is inside if statement, and then extract out that value

  color_generator_edges <- shiny::reactive({
    palette <- NULL

    if (input$palette_input == 'Uniform') {
      rep(input$uniform_hex_code, number_of_color_groups_edges())
      palette <- rep(input$uniform_hex_code, number_of_color_groups_edges())
    } else if (input$palette_input == 'Rainbow') {
      if (number_of_color_groups_edges() == 1) {
        grDevices::rainbow(10)[5]
        palette <- grDevices::rainbow(10)[5]
      } else {
        grDevices::rainbow(number_of_color_groups_edges())
        palette <- grDevices::rainbow(number_of_color_groups_edges())
      }
    } else if (input$palette_input == 'Heat') {
      if (number_of_color_groups_edges() == 1) {
        grDevices::heat.colors(10)[5]
        palette <- heat(10)[5]
      } else {
        grDevices::heat.colors(number_of_color_groups_edges())
        palette <- grDevices::heat.colors(number_of_color_groups_edges())
      }
    } else if (input$palette_input == 'Terrain') {
      if (number_of_color_groups_edges() == 1) {
        grDevices::terrain.colors(10)[5]
        palette <- grDevices::terrain.colors(10)[5]
      } else {
        grDevices::terrain.colors(number_of_color_groups_edges())
        palette <- grDevices::terrain.colors(number_of_color_groups_edges())
      }
    } else if (input$palette_input == 'Topo') {
      if (number_of_color_groups_edges() == 1) {
        grDevices::topo.colors(10)[5]
        palette <- grDevices::topo.colors(10)[5]
      } else {
        grDevices::topo.colors(number_of_color_groups_edges())
        palette <- grDevices::topo.colors(number_of_color_groups_edges())
      }
    } else if (input$palette_input == 'CM') {
      if (number_of_color_groups_edges() == 1) {
        grDevices::cm.colors(10)[5]
        palette <- grDevices::cm.colors(10)[5]
      } else {
        grDevices::cm.colors(number_of_color_groups_edges())
        palette <- grDevices::cm.colors(number_of_color_groups_edges())
      }
    }
  })

  #match by color or groups using groups or community
  color_matcher_edges <- shiny::reactive({
    if (input$multi_relational_toggle == TRUE & input$relational_column != "Empty") {
      groups <- unique(edge_data()[,input$relational_column])
    } else {
      groups <- rep(1,length(edge_data()))
    }
    colrs <- color_generator_edges()
    data.frame(groups, colrs)
  })

  #Set edge and vertex Color Attribute in network
  net3 <- shiny::reactive({
    net <- net6()
    if (input$community_input != "None") {
      igraph::V(net)$color <- color_matcher()$colrs[match(igraph::V(net)$communities, color_matcher()$groups)]
    } else {
      igraph::V(net)$color <- color_matcher()$colrs[match(igraph::V(net)$group, color_matcher()$groups)]
    }

    if (input$multi_relational_toggle == TRUE) {
      if (input$relational_column != "Empty") {
        igraph::E(net)$type <- edge_data()[,input$relational_column]
        igraph::E(net)$color <- color_matcher_edges()$colrs[match(igraph::E(net)$type, color_matcher_edges()$groups)]
      }
    }
    net
  })
  #### Update Edge weights ----
  #set edge weight
  # output$edge_weight_scalar <- shiny::renderUI({
  #   shiny::sliderInput(inputId = "edge_weight_scalar", label = "Edge width scalar", min = 0, max = 4, value =2, step = .1)
  # })

  net7 <- shiny::reactive({
    net <- net3()
    rescale1 = function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
    temp <- rep(1, length(edge_data()[,input$edge_in_col]))
    igraph::E(net)$uni_weight <-  temp * 2 #input$edge_weight_scalar
    if (input$edge_weight_col == 'Empty') {
      igraph::E(net)$weight <-  temp * 2 #input$edge_weight_scalar
      net
    } else {
      temp <- igraph::E(net)$weight
      igraph::E(net)$weight <- rescale1(temp, min(temp), max(temp), 1,5) * 2 #input$edge_weight_scalar
      net
    }
  })



  #### Update isolates ----
  net4 <- shiny::reactive({
    if (input$isolate_toggle == TRUE) {
      net <- net7()
      bad.vs<-igraph::V(net)[igraph::degree(net) == 0]
      net <- igraph::delete.vertices(net, bad.vs)
      net
    } else {
      net <- net7()
      net
    }

  })



  # 2. Simplify (Self Loops and Repeating Edges)

  net5 <- shiny::reactive({
    if (input$simplify_toggle == TRUE) {
      net <- net4()
      net <- igraph::simplify(net)
      #igraph::V(net)$color <- color_generator()
      net
    } else {
      net <- net4()
      #igraph::V(net)$color <- color_generator()
      net
    }
  })

  #### Pick Network layout ----

  layout_choices <- c("Star" = "layout_as_star", "Tree" = "layout_as_tree", "Circle" = "layout_in_circle",
                      "Nicely" = "layout_nicely", "Grid" = "layout_on_grid", "Sphere" = "layout_on_sphere", "Random" = "layout_randomly", "Davidson-Harel" = "layout_with_dh", "Fruchterman-Reingold" = "layout_with_fr",
                      "GEM" = "layout_with_gem", "Graphopt" = "layout_with_graphopt", "Kamada-Kawai" = "layout_with_kk", "Large Graph Layout (LGL)" = "layout_with_lgl", "Multidimensional Scaling (MDS)" = "layout_with_mds"
  )

  #set the layout of the network
  output$layout_picker <- shiny::renderUI({
    shiny::selectInput(inputId = "layout_choice", label = "Network layout", choices = layout_choices, selected = "layout_with_fr", multiple = FALSE)
  })

  #change the plot dimentions
  output$plot_scalar <- shiny::renderUI({
    shiny::sliderInput(inputId = "plot_scalar", label = "Plot dimensions", min = 100, max = 1000, value =600, step = 50)
  })

  #toggle interactivity in network vizualization
  output$interactive <-
    shiny::renderUI({
      shinyWidgets::materialSwitch(inputId = "interactive_switch", label = "Toggle Interactivity", status = "info", value = FALSE)
    })

  #set method for weighting edges
  output$edge_weight_method <-
    shiny::renderUI({
      shiny::selectInput(inputId = "edge_weight_method", label = "Edge width method", choices = c("Uniform", "Edge Data"), selected = "Uniform", multiple = FALSE)
    })


  # output$edge_color_method <- shiny::renderUI({
  #
  # })

  #change seed number
  output$set_seed <-
    shiny::renderUI({
      shiny::actionButton("set_seed", "Generate New Layout",
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })

  seed_number <-
    shiny::reactiveValues(seed =  as.integer(readLines("temp/seed.txt", n = 1)))

  shiny::observeEvent(input$set_seed, {
    writeLines(as.character(sample.int(1000, 1)), "temp/seed.txt")
    seed_number$seed <-  as.integer(readLines("temp/seed.txt", n = 1))
  })

  ### Visualize network ----
  #output for network vizualizations
  output$filter_relation_type <- shiny::renderUI({
    shiny::selectInput('filter_relation_type', label = 'Filter edges by relation', choices = append('None',edge_data()[,input$relational_column]), selected = "None",)
  })

  output$toggle_relational_coloring <- shiny::renderUI({
    shiny::checkboxInput(inputId = "toggle_relational_coloring", label = "Toggle Multi-relational Coloring", value = TRUE)
  })

  net8 <-
    shiny::reactive({
      net <- net5()
      if (input$multi_relational_toggle == TRUE) {
        if (input$filter_relation_type != 'None') {
          net <- igraph::delete.edges(net, igraph::E(net)[igraph::E(net)$type == input$filter_relation_type])
        }
        if (input$toggle_relational_coloring == FALSE) {
          if (input$relational_column != 'Empty') {
            net <- igraph::delete_edge_attr(net,'color')
          }
        }}

      net.visn <- visNetwork::toVisNetworkData(net)

      #set node labels manually because for SOME REASON it chooses to misbehave
      net.visn$nodes$label <- igraph::V(net)$label

      net.visn$nodes$label <- igraph::V(net)$label

      print(net.visn$nodes$label)


      if (input$interactive_switch) {
        if (input$edge_weight_method == "Uniform") {
          net.visn$edges$value <- net.visn$edges$uni_weight
          visNetwork::visNetwork(net.visn$nodes, net.visn$edges, width = "100%") %>%
            visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>%
            visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T),
                                   nodesIdSelection = T) %>%
            visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>%
            visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date()))  %>%
            visNetwork::visGroups()
        } else {
          net.visn$edges$value <- net.visn$edges$weight
          visNetwork::visNetwork(net.visn$nodes, net.visn$edges) %>%
            visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>%
            visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T),
                                   nodesIdSelection = T) %>%
            visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>%
            visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date())) %>%
            visNetwork::visGroups()
        }} else {
          if (input$edge_weight_method == "Uniform") {
            net.visn$edges$value <- net.visn$edges$uni_weight
            visNetwork::visNetwork(net.visn$nodes, net.visn$edges) %>%
              visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>%
              visNetwork::visInteraction(dragNodes = FALSE,
                                         dragView = FALSE) %>%
              visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>%
              visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date())) %>%
              visNetwork::visGroups()
          } else {
            net.visn$edges$value <- net.visn$edges$weight
            visNetwork::visNetwork(net.visn$nodes, net.visn$edges) %>%
              visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>%
              visNetwork::visInteraction(dragNodes = FALSE,
                                         dragView = FALSE) %>%
              visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>%
              visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date())) %>%
              visNetwork::visGroups()
          }}
    })

  output$network <- visNetwork::renderVisNetwork(net8())

  output$legend <- shiny::renderPlot({
    #### Create legend here
    shiny::req(input$palette_input != 'Uniform')
    # dataframe of groups (from net1)
    color_net <- net5()

    # Extract node colors from `color_net`
    if (input$community_input != "None") {
      # This bit of code from above just kept here for reference, shouldn't be un-commented-out
      #### igraph::V(net)$color <- color_matcher()$colrs[match(igraph::V(net)$communities, color_matcher()$groups)]
      node_legend_df <- unique(data.frame(group = igraph::V(color_net)$communities,
                                          color = igraph::V(color_net)$color))
    } else {
      # This bit of code from above just kept here for reference, shouldn't be un-commented-out
      #### igraph::V(net)$color <- color_matcher()$colrs[match(igraph::V(net)$group, color_matcher()$groups)]
      node_legend_df <- unique(data.frame(group = igraph::V(color_net)$group,
                                          color = igraph::V(color_net)$color))
    }

    node_legend_df <- dplyr::arrange(node_legend_df, group)

    # # links group values to group identifier
    # group_index <- data.frame(group = unique(igraph::V(net1)$group),
    #                           group_id = 1:length(unique(igraph::V(net1)$group)))
    #
    # # not too sure about this - how to get color theme that user selected?
    # # color_palette = color_generator_edges$palette
    # # group_index$color = palette(num.color = nrow(group_index))
    #
    # color_assign <- color_assign %>% dplyr::left_join(group_index, by = "group")
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    # figure out how to populate with right values
    # Only display/populate if a color palette is actually chosen
    if (input$palette_input != 'Uniform') {
      legend("topleft", legend = node_legend_df$group, pch=16, pt.cex=3, cex=1.5, bty='n',
             col = node_legend_df$color)
      mtext("Legend", at=0.2, cex=2)
    }
  })

  output$network_ui <-
    shiny::renderUI({
      shiny::validate(
        shiny::need(input$raw_edges, 'Upload Edge Data!'),
        # shiny::need(input$edge_in_col != "Empty" | input$edge_out_col != "Empty", 'Make sure you have selected an edge in and out column!'),
        # shiny::need(try(!is.null(net0())), 'Error computing network statistics. Check edge in and out columns to make sure you have uploaded the right data.')
        shiny::need(input$edge_in_col != "Empty", 'Make sure you have selected an edge in column!'),
        shiny::need(input$edge_out_col != "Empty", 'Make sure you have selected an edge out column!'),
        shiny::need(!input$nodes_exist || nodes_done(), 'Make sure you have selected a node id column!'),
        shiny::need(try(!is.null(net0())), 'Error computing network statistics. Check edge in and out columns to make sure you have uploaded the right data.'),
      )
      visNetwork::visNetworkOutput('network', height = input$plot_scalar, width = input$plot_scalar) %>% shinycssloaders::withSpinner(type = 5)
    })

  output$save_image <- shiny::renderUI({
    shiny::actionButton("save_image", "Save Graph as HTML", icon("download"),
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })

  output$image_type <- shiny::renderUI({
    shiny::selectInput('image_type', 'Select Image Format', choices = c('png','jpeg','pdf'))
  })

  shiny::observeEvent(input$save_image, {
    visNetwork::visSave(net8(), file = paste0(input$layout_choice, seed_number$seed,Sys.Date(),".html"))
  })

  output$measure_chooser <- shiny::renderUI({
    shiny::selectInput(inputId = "measure_chooser", label = "Choose Summary Level", choices = c("System", "Node"), selected = "System", multiple = FALSE)
  })

  ### Visualize summary statistics ----

  output$system_level_chooser <- shiny::renderUI({
    shiny::validate (
      shiny::need(input$raw_edges, 'Upload Edge Data!'),
      shiny::need(input$edge_in_col != "Empty", 'Select edge in column!'),
      shiny::need(input$edge_out_col != "Empty", 'Select edge out column!'),
      shiny::need(nodes_done(), 'Select node id column!')
      # shiny::need(input$edge_out_col != "Empty", 'Select edge out column!')
    )
    if (input$multi_relational_toggle == TRUE) {
      shiny::selectInput('system_level_chooser', 'Choose which relation you want to visualize', choices = names(system_measure_plot), selected = NULL)
    }
  })

  output$node_level_chooser <- shiny::renderUI({
    shiny::validate (
      shiny::need(input$raw_edges, 'Upload Edge Data!'),
      shiny::need(input$edge_in_col != "Empty", 'Select edge in column!'),
      # shiny::need(input$edge_out_col != "Empty", 'Select edge out column!')
      shiny::need(input$edge_out_col != "Empty", 'Select edge out column!'),
      shiny::need(nodes_done(), 'Select node id column!')
    )
    if (input$multi_relational_toggle == TRUE) {
      shiny::selectInput('node_level_chooser', 'Choose which relation you want to visualize', choices = names(node_measure_plot), selected = NULL)
    }
  })

  output$stats1 <-
    shiny::renderPlot({
      shiny::validate(
        shiny::need(input$raw_edges, 'Upload Edge Data!'),
        shiny::need(input$edge_in_col != "Empty", 'Select edge in column!'),
        # shiny::need(input$edge_out_col != "Empty", 'Select edge out column!')
        shiny::need(input$edge_out_col != "Empty", 'Select edge out column!'),
        shiny::need(nodes_done(), 'Select node id column!')
      )

      # Multirelational
      if(input$multi_relational_toggle == TRUE) {
        if (input$measure_chooser == "System") {
          plot(system_measure_plot[[match(input$system_level_chooser,names(system_measure_plot))]])
        } else {
          plot(node_measure_plot[[match(input$node_level_chooser,names(node_measure_plot))]])
        }
        # Single Relation
      } else {
        if (input$measure_chooser == "System") {
          plot(system_measure_plot)
        } else {
          plot(node_measure_plot)
        }
      }

    })

  ### Visualize nodemeasures ----
  # custom_theme <- function() {
  #   ggplot2::theme_light() +
  #     ggplot2::theme(
  #       text = ggplot2::element_text(family = "Helvetica", color = "#333333"),
  #       plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
  #       plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
  #       plot.caption = ggplot2::element_text(size = 8, hjust = 0.5),
  #       axis.title = ggplot2::element_text(size = 10),
  #       axis.text = ggplot2::element_text(size = 8),
  #       legend.title = ggplot2::element_text(size = 10),
  #       legend.text = ggplot2::element_text(size = 8)
  #     )
  # }

  ggplot2::theme_set(ggplot2::theme_light(base_size = 18))

  output$show_vars <- shiny::renderUI({
    shiny::checkboxGroupInput("show_vars", "Columns in node variables to show:",
                              names(node_measures), selected = names(node_measures)[1:5])
  })


  graph_wanted_val <- shiny::reactive({input$graph_wanted})


  output$graph_wanted <- shiny::renderUI({
    shiny::checkboxInput('graph_wanted', value = FALSE)
  })

  output$var_wanted <- shiny::renderUI({
    shiny::req(input$graph_wanted)
    shiny::checkboxInput('var_wanted', value = FALSE)
  })

  output$data_table_vis_var <-
    shiny::renderUI({
      shiny::req(input$graph_wanted)
      shiny::selectInput('data_table_vis_var',label = 'Select variable to plot', choices = nodelist3() %>% colnames(), selected = NULL)
    })

  output$data_table_vis_var2 <-
    shiny::renderUI({
      shiny::req(input$graph_wanted)
      shiny::req(input$var_wanted)
      shiny::selectInput('data_table_vis_var2',label = 'Select second variable to plot',choices = nodelist3() %>% colnames(), selected = NULL)
    })

  chosen_node_graph <- shiny::reactiveVal()

  shiny::observeEvent(input$data_table_vis_type, {
    chosen_graph <-
      if(input$data_table_vis_type == 'boxplot') {
        chosen_node_graph('boxplot')
      }
    else if(input$data_table_vis_type == 'histogram'){
      chosen_node_graph('histogram')
    }
    else if(input$data_table_vis_type == 'density plot'){
      chosen_node_graph('density plot')
    }
    else if(input$data_table_vis_type == 'scatterplot'){
      chosen_node_graph('scatterplot')
    }
  })

  output$data_table_vis_type <-
    shiny::renderUI({
      shiny::req(input$graph_wanted)
      shiny::selectInput('data_table_vis_type', label = 'Select visualisation', choices = c("Histogram" = 'histogram',
                                                                                            "Density Plot" = 'density plot',
                                                                                            "Boxplot" = 'boxplot'), selected = NULL)
    })

  # Removed automatic scatterplotting and made it available if two variables are selected.
  shiny::observeEvent(input$data_table_vis_var2, {
    output$data_table_vis_type <-
      shiny::renderUI({
        shiny::req(input$graph_wanted)
        shiny::selectInput('data_table_vis_type', label = 'Select visualisation', choices = c("Histogram" = 'histogram',
                                                                                              "Density Plot" = 'density plot',
                                                                                              "Boxplot" = 'boxplot',
                                                                                              "Scatter Plot" = 'scatterplot'), selected = NULL)
      })
  })


  output$statistics_table <- DT::renderDataTable({#print("Reached data table")
    nodelist3()[, input$show_vars, drop = FALSE]})
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      paste("node_measures_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(nodelist3(),file)
    }
  )


  output$statistics_graph <-
    shiny::renderPlot({
      shiny::req(input$graph_wanted)
      if(chosen_node_graph() == 'boxplot') {
        ggplot2::ggplot(data = nodelist3(), ggplot2::aes(x = nodelist3()[,input$data_table_vis_var])) +
          ggplot2::geom_boxplot(color="#0073C2FF", fill="#0073C2FF", alpha=0.2) +
          ggplot2::labs(title = paste("Distribution of", input$data_table_vis_var), x = input$data_table_vis_var)
      }
      else if(chosen_node_graph() == 'histogram') {
        if (length(unique(nodelist3()[,input$data_table_vis_var])) < 11) {
          dat <- data.frame(table(nodelist3()[,input$data_table_vis_var]))
          ggplot2::ggplot(dat, ggplot2::aes(x = Var1, y = Freq)) +
            ggplot2::geom_col(fill = "#0073C2FF", color = "#FFFFFF") +
            ggplot2::labs(title = paste("Distribution of", input$data_table_vis_var), x = input$data_table_vis_var, y = "N")
        } else {
          dat <- hist(nodelist3()[,input$data_table_vis_var], plot = F)
          dat <- data.frame(x = dat$mids, y = dat$counts)
          ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_col(fill = "#0073C2FF", color = "#FFFFFF") +
            ggplot2::labs(title = paste("Distribution of", input$data_table_vis_var), x = input$data_table_vis_var, y = "N")
        }
      }
      else if(chosen_node_graph() == 'density plot') {
        ggplot2::ggplot(data = nodelist3(), ggplot2::aes(x = nodelist3()[,input$data_table_vis_var])) +
          ggplot2::geom_density(alpha = 0.7, fill = "#0073C2FF") +
          ggplot2::labs(title = paste("Distribution of", input$data_table_vis_var), x = input$data_table_vis_var) +
          ggplot2::scale_x_continuous(labels = scales::comma)
      }
      else if(chosen_node_graph() == 'scatterplot') {
        ggplot2::ggplot(data = nodelist3(), ggplot2::aes(x = nodelist3()[,input$data_table_vis_var], y = nodelist3()[,input$data_table_vis_var2])) +
          ggplot2::geom_point(color="#0073C2FF") +
          ggplot2::geom_line(stat = "smooth", method = "lm", alpha = 0.5, formula = y ~ x) +
          ggplot2::labs(title = paste(input$data_table_vis_var, "vs", input$data_table_vis_var2), x = input$data_table_vis_var, y = input$data_table_vis_var2) +
          ggplot2::scale_x_continuous(labels = scales::comma)
      }
    })

  ### Setup Analysis Tab ----
  output$analysis_chooser <- shiny::renderUI({
    shiny::selectInput(inputId = "analysis_chooser", label = "Choose Analysis Module", choices = c("QAP", "Role Detection"), selected = "QAP", multiple = FALSE)
  })

  shiny::observeEvent(input$analysis_chooser, {
    shiny::updateTabsetPanel(inputId = "analytic_panels", selected = input$analysis_chooser)
  })

  #### QAP ----

  #CHOOSE METHODS
  output$method_chooser <- shiny::renderUI({
    shiny::selectInput(input="method_chooser", label = "Choose your method", choices = c("None",
                                                                                         "Multi-Category" = "multi_category",
                                                                                         "Reduced Category" = "reduced_category",
                                                                                         "Both Multi- and Reduced Category" = "both",
                                                                                         "Difference" = "difference"), selected="None",multiple=FALSE)
  })

  # output$method_chooser <- shiny::renderUI({
  #   shiny::selectInput(input="method_chooser", label = "Choose your method", choices = c("None",
  #                                                                                        "multi_category",
  #                                                                                        "reduced_category",
  #                                                                                        "both",
  #                                                                                        "difference"), selected="None",multiple=FALSE)
  # })


  chosen_methods <- shiny::reactiveVal(c())

  shiny::observeEvent(input$method_chooser, {
    shiny::req(input$method_chooser)
    if(input$method_chooser[[1]] == "None") {
      chosen_methods()
    }
    else {
      chosen_methods(c(chosen_methods(), input$method_chooser[[1]]))
      print(chosen_methods())
    }
  })

  shiny::observeEvent(chosen_methods(), {
    shiny::req(chosen_methods())
    shiny::updateSelectInput(session, "chosen_methods",
                             selected = "None",
                             choices = c("None","multi_category","reduced_category","both","difference")
    )
  })

  output$method_list <- shiny::renderPrint({
    print(chosen_methods())
  })


  #CHOOSE VARIABLES
  output$var_cols <- shiny::renderUI({
    shiny::validate(
      shiny::need(input$raw_edges, 'Input edge data!'),
      shiny::need(input$raw_nodes, 'Input node data!')
    )
    shiny::selectInput(inputId = "var_cols", label = "Column with variable", choices = append("None",colnames(node_data())), selected = "None", multiple = FALSE)
  })

  chosen_var <- shiny::reactiveVal(c())

  shiny::observeEvent(input$var_cols, {
    shiny::req(input$var_cols)
    if(input$var_cols[[1]] == "None") {
      chosen_var()
    } else {
      chosen_var(c(chosen_var(), input$var_cols[[1]]))
      print(chosen_var())
    }
  })

  shiny::observeEvent(chosen_var(), {
    shiny::req(chosen_var())
    shiny::updateSelectInput(session, "var_cols",
                             selected = "None",
                             choices = append("None",colnames(node_data()))

    )
  })

  output$var_list <- shiny::renderPrint({
    print(chosen_var())
  })

  #run options

  output$run_QAP_setup <-
    shiny::renderUI({
      shiny::validate(
        shiny::need(input$raw_edges, 'Input edge data!'),
        shiny::need(input$raw_nodes, 'Input raw nodes!'),
        shiny::need(!is.null(chosen_methods),"Choose a method"),
        shiny::need(!is.null(chosen_var),"Choose a variable")
      )
      shiny::actionButton("run_QAP_setup", "Run Initial QAP measures",
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })

  ran_toggle_qap <- shiny::reactiveValues(x=0)

  shiny::observeEvent(input$run_QAP_setup, {
    net <- net5()
    print("FOREACH")
    # foreach::foreach(i=1:length(chosen_var())) %do% {
    #   net <- igraph::set_vertex_attr(net,chosen_var()[i],value=nodelist3() %>% dplyr::pull(parse_expr(chosen_var()[i])))
    # }
    print("IS THIS WHERE QAP BREAKS?")
    list2env(qap_setup(net,chosen_var(),chosen_methods()), .GlobalEnv)
    ran_toggle_qap$x <- 1
  })

  #Run QAP MODEL
  output$qap_run_choices <- shiny::renderUI({
    print("qap_run_choices")
    shiny::validate(
      shiny::need(ran_toggle_qap$x != 0, 'Run QAP Setup'),
    )
    shiny::selectInput(inputId = "qap_run_choices", label = "Choose independent variable(s) (prefix: `same`, `both`, `diff` or `absdiff`)", choices = append("None",setdiff(edges %>% names(),c("to","from","weight"))), selected = "None", multiple = TRUE)
  })

  output$qap_run_dependent <- shiny::renderUI({
    print("qap_run_dependent")
    shiny::validate(
      shiny::need(ran_toggle_qap$x != 0, 'Run QAP Setup'),
    )
    shiny::selectInput(inputId = "qap_run_dependent", label = "Choose dependent variable (prefix: `same`, `both`, `diff` or `absdiff`)", choices = append("Tie Exists",setdiff(edges %>% names(),c("to","from","weight"))), selected = "None", multiple = FALSE)
  })


  output$run_QAP_model <- shiny::renderUI({
    print("run_QAP_model")
    shiny::validate(
      shiny::need(ran_toggle_qap$x != 0, 'Run QAP Setup'),
    )
    shiny::actionButton("run_QAP_model", "Run QAP Model",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })

  qap_df <- shiny::reactive(
    data.frame(message = "Awaiting QAP Run")
  )


  # shiny::observeEvent(input$run_QAP_model, {
  #   shiny::validate(
  #     shiny::need(qap_results, message = "Need to Run QAP Setup"),
  #   )
  #   print("AT QAP RUN STEP")
  #   print(input$qap_run_choices)
  #   print(input$qap_run_dependent)
  #   if (input$qap_run_dependent == "Tie Exists") {
  #     dep_var <- NULL
  #   } else {
  #     dep_var <- input$qap_run_dependent
  #   }
  #   qap_run(net = qap_results[[1]], variables = input$qap_run_choices,
  #           dependent = dep_var, directed = T)
  #   print(model_results[[1]])
  #   model_results[[1]]
  # })

  qap_df <- shiny::eventReactive(input$run_QAP_model, {
    shiny::validate(
      shiny::need(graph, message = "Need to Run QAP Setup"),
    )
    print("AT QAP RUN STEP")
    print(input$qap_run_choices)
    print(input$qap_run_dependent)
    if (input$qap_run_dependent == "Tie Exists") {
      dep_var <- NULL
    } else {
      dep_var <- input$qap_run_dependent
    }
    print(input$qap_run_choices)
    list2env(qap_run(net = graph, variables = input$qap_run_choices,
                     dependent = dep_var, directed = input$direction_toggle),
             .GlobalEnv)
    covs_df$estimate <- round(covs_df$estimate, digits = 3)
    covs_df
  })


  # shiny::observeEvent(input$run_QAP_model, {
  #   shiny::validate(
  #     shiny::need(qap_results, message = "Need to Run QAP Setup"),
  #   )
  #   print("AT QAP RUN STEP")
  #   print(input$qap_run_choices)
  #   print(input$qap_run_dependent)
  #   if (input$qap_run_dependent == "None") {
  #     dep_var <- NULL
  #   } else {
  #     dep_var <- input$qap_run_dependent
  #   }
  #   qap_run(net = qap_results[[1]], variables = input$qap_run_choices,
  #           dependent = dep_var, directed = T)
  #   qap_df <- model_results[[1]]
  #   print(model_results[[1]])
  # })

  #replace table
  output$qap_table <- DT::renderDataTable({
    qap_df()
  })

  # #second attempt if that doesnt work
  # output$qap_table <- DT::renderDataTable({
  #   DT::datatable(qap_df())
  # })

  # #third attempt
  # output$qap_table <- DT::renderDataTable({
  #   DT::datatable(model_results[[1]])
  # })

  # output$qap_model_results <- shiny::reactive({
  #   model_results[[1]]
  # })
  #
  # output$qap_table <- DT::renderDataTable({
  #  #  model_results[[1]]
  #   input$qap_model_results()
  # })



  #### Role Detection ----

  ran_toggle_role_detect <- shiny::reactiveValues(x=0)

  output$role_det_min <- shiny::renderUI({
    shiny::sliderInput(inputId = "role_det_min", label = "Choose Minimum # of Clusters", min = 2, max = nrow(nodelist3()), round = TRUE, step = 1, value = 4)
  })

  output$role_det_max <- shiny::renderUI({
    shiny::sliderInput(inputId = "role_det_max", label = "Choose Max # of Clusters", min = 2, max = nrow(nodelist3()), round = TRUE, step = 1, value = 4)
  })

  output$run_role_detect <-
    shiny::renderUI({
      shiny::validate(
        shiny::need(input$raw_edges, 'Input edge data!')
      )
      shiny::actionButton("run_role_detect", "Run Role Detection",
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })

  output$select_role_type <- shiny::renderUI({
    shiny::selectInput('select_role_type', label = "Choose Role Detection Method", choices = c("CONCOR" = 'concor',
                                                                                               "Hierarchical Clustering" = 'cluster'))
  })

  role_detect_choices <- shiny::reactive({
    choices_yah <- c()
    if (input$select_role_type == 'cluster') {
      choices_yah <- c("Modularity" = 'cluster_modularity',
                       "Dendrogram" = 'cluster_dendrogram',
                       "Heatmap (Chi-Squared)" = 'cluster_relations_heatmaps_chisq',
                       "Heatmap (Density, Centered)" ='cluster_relations_heatmaps_density_centered',
                       "Heatmap (Density, Standardized)" = 'cluster_relations_heatmaps_density_std',
                       "Heatmap (Density)" = 'cluster_relations_heatmaps_density',
                       "Cluster Relations Sociogram" = 'cluster_relations_sociogram',
                       "Cluster Summaries (Centrality)" = 'cluster_summaries_cent',
                       "Cluster Summaries (Motifs)" = 'cluster_summaries_triad')
    }
    else if (input$select_role_type == 'concor') {
      choices_yah <- c("Block Tree" = 'concor_block_tree',
                       "Modularity" = 'concor_modularity',
                       "Heatmap (Chi-Squared)" = 'concor_relations_heatmaps_chisq',
                       "Heatmap (Density)" = 'concor_relations_heatmaps_density',
                       "Heatmap (Density, Standardized)" = 'concor_relations_heatmaps_density_std',
                       "Heatmap (Density, Centered)" = 'concor_relations_heatmaps_density_centered',
                       "Cluster Relations Sociogram" = 'concor_relations_sociogram')
    }
    choices_yah
  })

  output$role_viz <- shiny::renderPlot({
    shiny::validate(
      shiny::need(input$raw_edges, 'Upload Edge Data!'),
      shiny::need(ran_toggle_role_detect$x == 1, "Input Role Detection Parameters and Run!")
    )
    if(input$select_role_viz == "cluster_modularity") {
      grDevices::replayPlot(cluster_modularity)
    }
    else if(input$select_role_viz == 'cluster_dendrogram') {
      grDevices::replayPlot(cluster_dendrogram)
    }
    else if(input$select_role_viz == 'cluster_relations_sociogram') {
      grDevices::replayPlot(cluster_relations_sociogram$summary_graph)
    }
    else if(input$select_role_viz == 'cluster_sociogram') {
      grDevices::replayPlot(cluster_sociogram)
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_chisq') {
      plot(cluster_relations_heatmaps$chisq)
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_density') {
      plot(cluster_relations_heatmaps$density)
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_density_std') {
      plot(cluster_relations_heatmaps$density_std)
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_density_centered') {
      plot(cluster_relations_heatmaps$density_centered)
    }
    else if(input$select_role_viz == 'cluster_summaries_cent') {
      plot(cluster_summaries_cent$summary_graph)
    }
    else if(input$select_role_viz == 'cluster_summaries_triad') {
      plot(cluster_summaries_triad$summary_graph)
    }
    else if(input$select_role_viz == 'concor_block_tree') {
      grDevices::replayPlot(concor_block_tree)
    }
    if(input$select_role_viz == "concor_modularity") {
      grDevices::replayPlot(concor_modularity)
    }
    else if(input$select_role_viz == 'concor_relations_sociogram') {
      grDevices::replayPlot(concor_relations_sociogram$summary_graph)
    }
    else if(input$select_role_viz == 'concor_sociogram') {
      grDevices::replayPlot(concor_sociogram)
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_chisq') {
      plot(concor_relations_heatmaps$chisq)
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_density') {
      plot(concor_relations_heatmaps$density)
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_density_std') {
      plot(concor_relations_heatmaps$density_std)
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_density_std') {
      plot(concor_relations_heatmaps$density_centered)
    }
  })

  output$select_role_viz <-
    shiny::renderUI({
      shiny::selectInput('select_role_viz', label = 'Choose Role Output Summary', choices = role_detect_choices())
    })

  output$min_cluster_size <- shiny::renderUI({
    shiny::selectInput(inputId = "min_cluster_size", label = "Choose Minimum Cluster Size", choices = append(NA,c(1:8)), selected = NA)
  })

  shiny::observeEvent(input$run_role_detect, {
    list2env(role_analysis(init_net,
                           nodes = node_measures,
                           directed = input$direction_toggle,
                           method = input$select_role_type,
                           min_partitions = input$role_det_min,
                           max_partitions = input$role_det_max,
                           min_partition_size = as.integer(input$min_cluster_size),
                           viz = TRUE),
             .GlobalEnv)
    ran_toggle_role_detect$x <- 1
  })

  #### CHAMP ----

  champ_results <- shiny::reactiveVal(NULL)
  ran_toggle_champ <- shiny::reactiveVal(0)
  ran_toggle_champ_map <- shiny::reactiveVal(0)

  output$champ_map_label_input <- shiny::renderUI({
    shiny::req(ran_toggle_champ() == 1)
    shiny::textInput("champ_map_label", "Plot label (optional)", value = "")
  })

  output$run_champ_map <- shiny::renderUI({
    shiny::req(ran_toggle_champ() == 1)
    shiny::actionButton("run_champ_map", "Run CHAMP Map",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })

  shiny::observeEvent(input$run_champ, {
    shiny::req(input$raw_edges)
    shiny::req(input$edge_in_col != "Empty")
    shiny::req(input$edge_out_col != "Empty")
    # Require undirected network
    shiny::req(isFALSE(input$direction_toggle))

    withProgress(message = 'Running CHAMP algorithm...', value = 0, {

      incProgress(0.1, detail = "Generating partitions...")
      partitions <- get_partitions(
        network = net5(),
        gamma_range = c(input$champ_gamma_min, input$champ_gamma_max),
        n_runs = input$champ_n_runs,
        seed = input$champ_seed
      )

      incProgress(0.7, detail = "Running CHAMP analysis...")
      partitions <- CHAMP(
        network = net5(),
        partitions = partitions,
        plottitle = "CHAMP Analysis"
      )

      champ_results(partitions)
      ran_toggle_champ(1)
    })
  })

  shiny::observeEvent(input$run_champ_map, {
    shiny::req(ran_toggle_champ() == 1)

    withProgress(message = 'Running CHAMP Map...', value = 0, {
      incProgress(0.5, detail = "Generating map...")

      partitions <- get_CHAMP_map(
        network = net5(),
        partitions = champ_results(),
        plotlabel = input$champ_map_label,
        shiny = TRUE
      )

      champ_results(partitions)
      ran_toggle_champ_map(1)
    })
  })

  # THIS WILL DISPLAY WARNING ABOUT DIRECTED NETS IF FAILS
  output$champ_plot <- shiny::renderPlot({
    shiny::validate(
      shiny::need(isFALSE(input$direction_toggle), "CHAMP is only supported for undirected networks.\nIf you would like to apply CHAMP to this network, you may want to set it as an undirected network in the Process tab.")
    )
    shiny::req(ran_toggle_champ() == 1)
    champ_results()$CHAMPfigure
  })

  output$champ_map_plot <- shiny::renderPlot({
    shiny::req(ran_toggle_champ() == 1)
    if (!is.null(champ_results()$CHAMPmap)) {
      champ_results()$CHAMPmap
    }
  })

  output$champ_summary_table <- DT::renderDataTable({
    shiny::req(ran_toggle_champ() == 1)
    champ_results()$CHAMPsummary
  })



}



shinyApp(ui, server)
