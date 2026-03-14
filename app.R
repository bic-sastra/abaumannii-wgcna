# =====================================================
# A. baumannii WGCNA Explorer - DEPLOYMENT VERSION
# FIXED: Gene search colors + threshold messages
# =====================================================

# DO NOT INSTALL PACKAGES HERE - just load them
# Packages must be installed before deployment

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(visNetwork)
library(dplyr)
library(ggplot2)
library(tidyr)
library(igraph)

# Load data with error handling
data_file <- "acinetobacter_wgcna_data.rds"
if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file, ". 
       Please ensure it's in the same directory.")
}
data <- readRDS(data_file)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "A. baumannii WGCNA Explorer", titleWidth = 350),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Module Browser", tabName = "modules", icon = icon("project-diagram")),
      menuItem("Gene Search", tabName = "search", icon = icon("search")),
      menuItem("Network Viewer", tabName = "network", icon = icon("share-alt")),
      menuItem("Virulence vs Resistance", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Data Download", tabName = "download", icon = icon("download")),
      menuItem("About & Citation", tabName = "help", icon = icon("info-circle"))
    ),
    
    hr(),
    
    selectInput("module_select", label = h5("Select Module"),
                choices = c("All", unique(data$gene_metadata$module)),
                selected = "All"),
    
    checkboxGroupInput("type_filter", label = h5("Gene Type"),
                       choices = c("Virulence", "Resistance", "Both", "Other"),
                       selected = c("Virulence", "Resistance", "Both")),
    
    sliderInput("cor_threshold", label = h5("Correlation Threshold"),
                min = 0.1, max = 0.9, value = 0.2, step = 0.05),
    
    hr(),
    h5("Quick Stats"),
    verbatimTextOutput("quick_stats")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-box {height: 150px;}
      .content-wrapper {background-color: #f4f4f4;}
      .citation-box {
        background-color: #f8f9fa;
        border-left: 4px solid #3c8dbc;
        padding: 15px;
        margin: 20px 0;
        font-family: monospace;
      }
    "))),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_genes_box"),
          valueBoxOutput("virulence_box"),
          valueBoxOutput("resistance_box")
        ),
        fluidRow(
          valueBoxOutput("modules_box"),
          valueBoxOutput("both_box"),
          valueBoxOutput("correlations_box")
        ),
        fluidRow(
          box(title = "Module Composition", width = 6,
              plotlyOutput("module_composition_plot", height = "350px")),
          box(title = "Gene Type Distribution", width = 6,
              plotlyOutput("gene_type_pie", height = "350px"))
        ),
        fluidRow(
          box(title = "Module Summary Table", width = 12,
              DTOutput("dashboard_module_table"))
        )
      ),
      
      # Module Browser Tab - REMOVED MODULE NETWORK PREVIEW
      tabItem(tabName = "modules",
        fluidRow(
          box(title = textOutput("module_title"), width = 12,
              DTOutput("module_genes_table"))
        ),
        fluidRow(
          box(title = "Module Statistics", width = 12,  # Changed to full width
              tableOutput("module_stats"))
          # REMOVED: visNetworkOutput("module_preview", height = "400px")
        )
      ),
      
      # Gene Search Tab
      tabItem(tabName = "search",
        fluidRow(
          box(title = "Search Genes", width = 12,
              selectizeInput("gene_search", label = "Enter gene symbols or locus tags:",
                           choices = setNames(data$gene_lookup$locus_tag,
                                            data$gene_lookup$display_name),
                           multiple = TRUE),
              actionButton("search_btn", "Search", icon = icon("search"),
                          class = "btn-primary btn-lg", width = "100%"))
        ),
        fluidRow(
          box(title = "Search Results", width = 12,
              DTOutput("search_results"))
        ),
        fluidRow(
          box(title = "Gene Network", width = 12,
              visNetworkOutput("gene_network", height = "500px"))
        )
      ),
      
      # Network Viewer Tab
      tabItem(tabName = "network",
        fluidRow(
          box(title = "Network Controls", width = 12, collapsed = TRUE,
              column(4, selectInput("layout_choice", "Layout:",
                                   choices = c("Force-directed" = "layout_with_fr",
                                             "Circle" = "layout_in_circle",
                                             "Tree" = "layout_as_tree"))),
              column(4, checkboxInput("show_labels", "Show Labels", TRUE),
                        checkboxInput("highlight_hubs", "Highlight Hubs", FALSE)),
              column(4, numericInput("node_size", "Node Size:", 30, 10, 100)))
        ),
        fluidRow(
          box(title = textOutput("network_title"), width = 12,
              visNetworkOutput("full_network", height = "600px"))
        ),
        fluidRow(
          box(title = "Node Information", width = 12,
              verbatimTextOutput("node_info"))
        )
      ),
      
      # Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(title = "Module Comparison", width = 12,
              plotlyOutput("comparison_heatmap", height = "500px"))
        ),
        fluidRow(
          box(title = "Virulence Genes by Module", width = 6,
              DTOutput("virulence_table")),
          box(title = "Resistance Genes by Module", width = 6,
              DTOutput("resistance_table"))
        ),
        fluidRow(
          box(title = "Genes with Both", width = 12,
              DTOutput("both_table"))
        )
      ),
      
      # Download Tab
      tabItem(tabName = "download",
        fluidRow(
          box(title = "Download Data", width = 6,
              h4("Complete Dataset"),
              downloadButton("download_full", "Download Full Data (RDS)"),
              br(), br(),
              h4("Module-specific Data"),
              selectInput("download_module", "Select Module:",
                         choices = unique(data$gene_metadata$module)),
              downloadButton("download_module_data", "Download Module Data (CSV)"),
              br(), br(),
              h4("Network Edges"),
              downloadButton("download_network", "Download All Edges (CSV)")),
          box(title = "Data Summary", width = 6,
              verbatimTextOutput("data_summary_detailed"))
        )
      ),
      
      # ENHANCED: About & Citation Tab
      tabItem(tabName = "help",
        fluidRow(
          box(title = "About This Application", width = 12, status = "primary",
              h3("A. baumannii WGCNA Explorer"),
              p("This interactive application allows exploration of Weighted Gene Co-expression Network Analysis (WGCNA) results from doripenem-treated Acinetobacter baumannii."),
              
              h4("📊 Study Overview"),
              p("This application accompanies the manuscript:"),
              div(class = "citation-box",
                  p(strong("WGCNA Analysis Reveals Co-expression of Virulence and Resistance Genes in Doripenem-Susceptible Acinetobacter baumannii")),
                  p("Ramprakash Rayala, Akilandeswari Ramu, Vigneshwar Ramakrishnan"),
                  p("Computational Molecular Biophysics Laboratory, SASTRA Deemed University, India")
              ),
              
              h4("🔬 Key Findings"),
              tags$ul(
                tags$li(strong("29 upregulated"), " and ", strong("15 downregulated"), " DEGs at 15 min post-doripenem treatment"),
                tags$li(strong("90 upregulated"), " and ", strong("37 downregulated"), " DEGs at 60 min post-doripenem treatment"),
                tags$li("Activated pathways: oxidative stress, two-component systems, efflux pumps, lipoproteins, biofilm formation"),
                tags$li("WGCNA revealed ", strong("14 co-expression modules"), " linking virulence and resistance genes"),
                tags$li("Key co-expressed nodes: ", em("rpoH, efp, ahpC, katG, nudC"), " as potential therapeutic targets")
              ),
              
              h4("📦 Data Overview"),
              tags$ul(
                tags$li(strong("Total genes:"), " ", data$stats$total_genes),
                tags$li(strong("Total modules:"), " ", data$stats$total_modules),
                tags$li(strong("Virulence genes:"), " ", data$stats$virulence_genes),
                tags$li(strong("Resistance genes:"), " ", data$stats$resistance_genes),
                tags$li(strong("Genes with both:"), " ", data$stats$both_genes)
              ),
              
              h4("📖 How to Cite"),
              div(class = "citation-box",
                  p("Rayala R, Ramu A, Ramakrishnan V. (2025). WGCNA Analysis Reveals Co-expression of Virulence and Resistance Genes in Doripenem-Susceptible Acinetobacter baumannii. [Journal Name], [Volume], [Pages]. DOI: [Add DOI]"),
                  br(),
                  p("Alternatively, cite this Shiny application:"),
                  p("Rayala R, Ramu A, Ramakrishnan V. (2025). A. baumannii WGCNA Explorer: Interactive visualization of virulence-resistance co-expression networks. https://[your-app-url].shinyapps.io/abaumannii-wgcna-explorer")
              ),
              
              h4("🔗 Links"),
              tags$ul(
                tags$li(a("GitHub Repository", href = "https://github.com/Akilaramu5698/abaumannii-wgcna", target = "_blank")),
                tags$li(a("PubMed (coming soon)", href = "#", target = "_blank")),
                tags$li(a("BioRxiv preprint (coming soon)", href = "#", target = "_blank"))
              ),
              
              h4("📧 Contact"),
              p("For questions or feedback, contact:"),
              p(a("Vigneshwar Ramakrishnan", href = "mailto:vignesh@scbt.sastra.edu"), " - Corresponding author"),
              
              hr(),
              p("App version: 1.0", br(), "Last updated:", Sys.Date())
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Quick stats
  output$quick_stats <- renderPrint({
    cat("Genes:", data$stats$total_genes, "\n")
    cat("Modules:", data$stats$total_modules, "\n")
    cat("Virulence:", data$stats$virulence_genes, "\n")
    cat("Resistance:", data$stats$resistance_genes)
  })
  
  # Value boxes
  output$total_genes_box <- renderValueBox({
    valueBox(data$stats$total_genes, "Total Genes", icon("dna"), "blue")
  })
  
  output$virulence_box <- renderValueBox({
    valueBox(data$stats$virulence_genes, "Virulence Genes", icon("virus"), "red")
  })
  
  output$resistance_box <- renderValueBox({
    valueBox(data$stats$resistance_genes, "Resistance Genes", icon("shield-alt"), "green")
  })
  
  output$modules_box <- renderValueBox({
    valueBox(data$stats$total_modules, "Modules", icon("project-diagram"), "purple")
  })
  
  output$both_box <- renderValueBox({
    valueBox(data$stats$both_genes, "Both Vir & Res", icon("balance-scale"), "yellow")
  })
  
  output$correlations_box <- renderValueBox({
    total_edges <- sum(sapply(data$network_edges, nrow))
    valueBox(format(total_edges, big.mark = ","), "Total Correlations", icon("link"), "teal")
  })
  
  # Module composition plot
  output$module_composition_plot <- renderPlotly({
    p <- ggplot(data$module_summary, aes(x = reorder(module, total_genes), y = total_genes)) +
      geom_bar(aes(fill = module), stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = total_genes), hjust = -0.1, size = 3) +
      coord_flip() + theme_minimal() +
      labs(x = "", y = "Number of Genes") +
      scale_fill_manual(values = data$module_colors)
    ggplotly(p, height = 350)
  })
  
  # Gene type pie chart
  output$gene_type_pie <- renderPlotly({
    type_counts <- data$gene_metadata %>% count(gene_type)
    plot_ly(type_counts, labels = ~gene_type, values = ~n, type = "pie",
            marker = list(colors = c("#FF6B6B", "#4ECDC4", "#FFD93D", "#95A5A6")),
            textinfo = "label+percent")
  })
  
  # Dashboard table
  output$dashboard_module_table <- renderDT({
    data$module_summary %>% datatable(options = list(pageLength = 5), rownames = FALSE)
  })
  
  # Module title
  output$module_title <- renderText({
    if(input$module_select == "All") "All Genes" else paste("Module:", input$module_select)
  })
  
  # Filtered genes
  filtered_genes <- reactive({
    df <- data$gene_metadata
    if(input$module_select != "All") df <- df %>% filter(module == input$module_select)
    if(!is.null(input$type_filter)) df <- df %>% filter(gene_type %in% input$type_filter)
    df
  })
  
  # Module genes table
  output$module_genes_table <- renderDT({
    filtered_genes() %>% select(locus_tag, gene_symbol, module, gene_type) %>%
      datatable(options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  # Module stats
  output$module_stats <- renderTable({
    filtered_genes() %>% summarise(
      Total = n(), Virulence = sum(gene_type %in% c("Virulence", "Both")),
      Resistance = sum(gene_type %in% c("Resistance", "Both")),
      Both = sum(gene_type == "Both"), Other = sum(gene_type == "Other")
    )
  })
  
  # REMOVED: module_preview render function completely
  
  # Gene search
  observeEvent(input$search_btn, {
    req(input$gene_search)
    results <- data$gene_metadata %>%
      filter(locus_tag %in% input$gene_search) %>%
      select(locus_tag, gene_symbol, module, gene_type)
    
    output$search_results <- renderDT({
      datatable(results, options = list(pageLength = 10), rownames = FALSE)
    })
  })
  
  # =============================================
  # FIXED: Gene search network with consistent colors
  # =============================================
  output$gene_network <- renderVisNetwork({
    req(input$search_btn)
    
    # If less than 2 genes selected, show message
    if(length(input$gene_search) < 2) {
      nodes <- data.frame(id = "need_more", 
                          label = "Select at least 2 genes",
                          title = "Please select 2 or more genes to see connections",
                          color = "#95A5A6",
                          size = 30)
      edges <- data.frame(from = character(0), to = character(0))
      
      return(visNetwork(nodes, edges) %>%
               visPhysics(enabled = FALSE) %>%
               visInteraction(dragNodes = FALSE, dragView = FALSE))
    }
    
    # Get search results
    results <- data$gene_metadata %>%
      filter(locus_tag %in% input$gene_search)
    
    # Find edges between searched genes
    all_edges <- bind_rows(data$network_edges, .id = "module")
    relevant_edges <- all_edges %>%
      filter(gene1 %in% input$gene_search & gene2 %in% input$gene_search) %>%
      filter(correlation >= input$cor_threshold)
    
    if(nrow(relevant_edges) > 0) {
      nodes_df <- results %>%
        mutate(
          id = locus_tag,
          label = ifelse(is.na(gene_symbol) | gene_symbol == "", locus_tag, gene_symbol),
          title = paste("Gene:", ifelse(is.na(gene_symbol) | gene_symbol == "", 
                                        locus_tag, 
                                        paste0(gene_symbol, " (", locus_tag, ")")),
                        "\nType:", gene_type,
                        "\nModule:", module),
          # FIXED: Use consistent colors (same as module preview)
          color = case_when(
            gene_type == "Virulence" ~ "#FF6B6B",
            gene_type == "Resistance" ~ "#4ECDC4",
            gene_type == "Both" ~ "#FFD93D",
            TRUE ~ "#95A5A6"
          ),
          size = 30
        )
      
      edges_df <- relevant_edges %>%
        mutate(
          from = gene1,
          to = gene2,
          width = correlation * 5,
          title = paste("Correlation:", round(correlation, 3))
        )
      
      visNetwork(nodes_df, edges_df) %>%
        visOptions(highlightNearest = TRUE) %>%
        visPhysics(stabilization = TRUE) %>%
        visLayout(randomSeed = 123)
    } else {
      # Return message if no edges
      nodes <- data.frame(id = "no_edges", 
                          label = paste("No correlations at r ≥", input$cor_threshold),
                          title = "Try lowering the correlation threshold",
                          color = "#FFA500",
                          size = 30)
      edges <- data.frame(from = character(0), to = character(0))
      
      visNetwork(nodes, edges) %>%
        visPhysics(enabled = FALSE) %>%
        visInteraction(dragNodes = FALSE, dragView = FALSE)
    }
  })
  
  # =============================================
  # FIXED: Full network with threshold message
  # =============================================
  output$network_title <- renderText({
    if(input$module_select == "All") "Complete Co-expression Network" 
    else paste("Module", input$module_select, "Network")
  })
  
  output$full_network <- renderVisNetwork({
    # Get edges based on selection
    if(input$module_select == "All") {
      all_edges <- bind_rows(data$network_edges, .id = "module") %>%
        filter(correlation >= input$cor_threshold)
      # Limit for performance
      if(nrow(all_edges) > 2000) {
        all_edges <- all_edges %>% arrange(desc(correlation)) %>% head(2000)
      }
    } else {
      # Check if module exists
      if(!(input$module_select %in% names(data$network_edges))) {
        nodes <- data.frame(id = "no_data", 
                            label = "No network data",
                            title = "This module has no edge data",
                            color = "#95A5A6",
                            size = 30)
        edges <- data.frame(from = character(0), to = character(0))
        
        return(visNetwork(nodes, edges) %>%
                 visPhysics(enabled = FALSE) %>%
                 visInteraction(dragNodes = FALSE, dragView = FALSE))
      }
      
      all_edges <- data$network_edges[[input$module_select]] %>%
        filter(correlation >= input$cor_threshold)
    }
    
    # Return message if no edges after filtering
    if(nrow(all_edges) == 0) {
      nodes <- data.frame(id = "no_edges", 
                          label = paste("No edges at r ≥", input$cor_threshold),
                          title = "Try lowering the correlation threshold",
                          color = "#FFA500",
                          size = 30)
      edges <- data.frame(from = character(0), to = character(0))
      
      return(visNetwork(nodes, edges) %>%
               visPhysics(enabled = FALSE) %>%
               visInteraction(dragNodes = FALSE, dragView = FALSE))
    }
    
    # Create nodes dataframe
    nodes <- unique(c(all_edges$gene1, all_edges$gene2))
    nodes_df <- data$gene_metadata %>%
      filter(locus_tag %in% nodes) %>%
      mutate(
        id = locus_tag,
        label = if(input$show_labels) {
          ifelse(is.na(gene_symbol) | gene_symbol == "", locus_tag, gene_symbol)
        } else {
          ""
        },
        title = paste("Gene:", ifelse(is.na(gene_symbol) | gene_symbol == "", 
                                      locus_tag, 
                                      paste0(gene_symbol, " (", locus_tag, ")")),
                      "\nType:", gene_type,
                      "\nModule:", module,
                      "\nLocus:", locus_tag),
        # FIXED: Use consistent colors
        color = case_when(
          gene_type == "Virulence" ~ "#FF6B6B",
          gene_type == "Resistance" ~ "#4ECDC4",
          gene_type == "Both" ~ "#FFD93D",
          TRUE ~ "#95A5A6"
        ),
        size = input$node_size,
        shape = "dot"
      )
    
    # Create edges dataframe
    edges_df <- all_edges %>%
      mutate(
        from = gene1,
        to = gene2,
        width = correlation * 5,
        title = paste("Correlation:", round(correlation, 3))
      )
    
    # Create base network
    network <- visNetwork(nodes_df, edges_df) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
        nodesIdSelection = TRUE
      ) %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE)
    
    # Add layout if available
    if (requireNamespace("igraph", quietly = TRUE)) {
      network <- network %>% visIgraphLayout(layout = input$layout_choice)
    } else {
      network <- network %>% visLayout(randomSeed = 123)
    }
    
    network
  })
  
  # Node click info
  observeEvent(input$clicked_node, {
    req(input$clicked_node)
    node_info <- data$gene_metadata %>% filter(locus_tag == input$clicked_node)
    
    output$node_info <- renderPrint({
      cat("Gene Symbol:", node_info$gene_symbol, "\n")
      cat("Locus Tag:", node_info$locus_tag, "\n")
      cat("Module:", node_info$module, "\n")
      cat("Type:", node_info$gene_type)
    })
  })
  
  # Comparison heatmap
  output$comparison_heatmap <- renderPlotly({
    heatmap_data <- data$module_summary %>%
      select(module, virulence_genes, resistance_genes, both_genes) %>%
      tidyr::pivot_longer(-module, names_to = "type", values_to = "count") %>%
      mutate(type = gsub("_genes", "", type))
    
    p <- ggplot(heatmap_data, aes(x = module, y = type, fill = count)) +
      geom_tile() + geom_text(aes(label = count), color = "white") +
      scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, height = 400)
  })
  
  # Tables
  output$virulence_table <- renderDT({
    data$gene_metadata %>% filter(gene_type %in% c("Virulence", "Both")) %>%
      select(locus_tag, gene_symbol, module) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$resistance_table <- renderDT({
    data$gene_metadata %>% filter(gene_type %in% c("Resistance", "Both")) %>%
      select(locus_tag, gene_symbol, module) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$both_table <- renderDT({
    data$gene_metadata %>% filter(gene_type == "Both") %>%
      select(locus_tag, gene_symbol, module) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Download handlers
  output$download_full <- downloadHandler(
    filename = function() paste0("wgcna_data_", Sys.Date(), ".rds"),
    content = function(file) saveRDS(data, file)
  )
  
  output$download_module_data <- downloadHandler(
    filename = function() paste0("module_", input$download_module, "_", Sys.Date(), ".csv"),
    content = function(file) {
      data$gene_metadata %>% filter(module == input$download_module) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  output$download_network <- downloadHandler(
    filename = function() paste0("network_edges_", Sys.Date(), ".csv"),
    content = function(file) {
      bind_rows(data$network_edges, .id = "module") %>% write.csv(file, row.names = FALSE)
    }
  )
  
  # Data summary
  output$data_summary_detailed <- renderPrint({
    cat("ACINETOBACTER BAUMANNII WGCNA DATA SUMMARY\n")
    cat("===========================================\n\n")
    cat("Total genes:", data$stats$total_genes, "\n")
    cat("Total modules:", data$stats$total_modules, "\n")
    cat("Virulence genes:", data$stats$virulence_genes, "\n")
    cat("Resistance genes:", data$stats$resistance_genes, "\n")
    cat("Both categories:", data$stats$both_genes, "\n\n")
    cat("Network Statistics:\n")
    for(mod in names(data$network_edges)) {
      cat("  -", mod, ":", nrow(data$network_edges[[mod]]), "edges\n")
    }
  })
}

# Run the app
shinyApp(ui, server)
