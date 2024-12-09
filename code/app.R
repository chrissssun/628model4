
library(shiny)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(tm)
library(stringr)
library(factoextra)
library(plotly)
library(cluster)
library(factoextra)
library(tidyr)
library(DT)
library(tidytext)
library(textdata)
library(dbscan)
library(text2vec)
library(fmsb)

# 设置 Spotify API 凭据
client_id <- "03112b55c71c4cea865cad56887736ea"
client_secret <- "2b4d7cb803af4ce1b3cf8eeab2ca4235"
Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

ui <- fluidPage(
  tags$h1("Spotify Podcast Analysis"),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #FFFAF0;
        margin: 0;
        padding: 0;
      }
      .container-fluid {
        padding-left: 20px;
        padding-right: 20px;
      }
      .shiny-container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
      }
      .well-panel {
        background-color: #FFCC99;
        border: none;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .btn-primary {
        background-color: #FF7043;
        border-color: #FF7043;
        color: white;
      }
      .btn-primary:hover {
        background-color: #FF5722;
        border-color: #FF5722;
      }
      .podcast-card {
        border: 1px solid #FFCC99;
        padding: 15px;
        margin: 10px;
        background-color: #FFF8E1;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .podcast-card h4 {
        font-size: 18px;
        font-weight: bold;
        color: #FF5722;
      }
      .podcast-card p {
        font-size: 14px;
        color: #FF7043;
        height: 60px;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .tab-panel {
        background-color: #FFEBEE;
      }
      .tab-panel .nav-link {
        background-color: #FF7043;
        color: white;
      }
      .tab-panel .nav-link.active {
        background-color: #FF5722;
        color: white;
      }
      .table {
        background-color: #FFF8E1;
        border-radius: 5px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .table th, .table td {
        padding: 15px;
        text-align: center;
      }
      .contact-info {
        text-align: center;
        padding-top: 20px;
        font-size: 14px;
        color: black;
      }
      h1 {
        text-align: center;
      }
    "))
  ),
  
  div(class = "shiny-container",
      fluidRow(
        column(3, 
               wellPanel(
                 textInput("keyword", "Search Podcast Keyword(eg. AI):", placeholder = "Enter a keyword...", width = '100%'),
                 numericInput("num_podcasts", "Number of Podcasts:", value = 10, min = 1, max = 50, width = '100%'),
                 
                 numericInput("clusters", "K:", value = 3, min = 2, max = 10, step = 1, width = '100%'),
                 div(
                   style = "font-size: 12px; color: #666666;", 
                   HTML("Adjust this value to control the number of categories for K-means clustering. <br>Different K-values will divide the data into a different number of categories, allowing for more detailed categorization.</br><br>")
                 ),
                 
                 actionButton("search", "Search and Analyze", style = "width: 100%; background-color: #FF7043; color: white; border: none; padding: 10px;"),
                 uiOutput("podcast_selector")
               )
        ),
        
        column(9, 
               tabsetPanel(
                 tabPanel("Podcast List", DTOutput("podcast_table")),
                 tabPanel("Podcast Clustering Overview", 
                          plotlyOutput("cluster_plot"),
                          HTML("<p style='color: black;'>This chart shows the clustering of podcasts based on the similarity of their descriptions. Each point represents a podcast, and its color indicates its category. <b>Mouse over the points to see the podcast name.</b> Adjusting the K value refines the classification:</p>
                              K=3:<br>
                              - Cluster 1: Simple language, neutral sentiment, moderate novelty. Suitable for users who prefer straightforward content.<br>
                              - Cluster 2: Positive sentiment, complex language, highly creative. Ideal for users seeking innovative content.<br>
                              - Cluster 3: Negative sentiment, deep content, intricate structure. Suitable for users interested in in-depth discussions.<br>
                              K=4:<br>
                              - Cluster 1: Simple language, low novelty. Suitable for users who enjoy light and casual content.<br>
                              - Cluster 2: Positive sentiment, rich language, complex structure. Perfect for fans of energetic and dynamic content.<br>
                              - Cluster 3: High novelty, simple language, easy to read. Suitable for users seeking unique but straightforward content.<br>
                              - Cluster 4: Complex language, profound content. Ideal for users interested in academic or socially focused topics.<br>
                               <p style='color: black;'><b>Specific Clustering Characteristics:</b></p>"),
                          uiOutput("cluster_description")
                 ),
                 tabPanel("Podcast Sentiment Polarity Visualization", 
                          plotOutput("sentiment_plot"),
                          HTML("<p style='color: black;'><br>This chart shows the sentiment polarity score of each podcast, representing the emotional tendency of the podcast content. Positive values indicate positive sentiment, negative values indicate negative sentiment, and values close to zero represent neutrality. This chart helps you understand the emotional tone of different podcasts and choose those that align with your emotional preferences.</p><br>")
                 ),
                 tabPanel("Podcast Novelty visualization", 
                          plotOutput("novelty_plot"),
                          HTML("<p style='color: black;'><br>This chart displays the novelty score of each podcast, reflecting the uniqueness of the podcast content. Higher scores indicate that the podcast is more unique compared to others. This chart helps you discover more creative and distinctive podcasts.</p><br>")
                 ),
                 tabPanel("Podcast Hearability Radar", 
                          plotOutput("text_complexity_radar"),
                          HTML("<p style='color: black;'><br>This radar chart illustrates the language complexity of each podcast, helping you assess the difficulty level of the podcast content.</p><br>")
                 ),
                 tabPanel("Nearest Podcasts", 
                          tableOutput("nearest_podcasts"),
                          HTML("<p style='color: black;'><br>This table displays the podcast list.</p>"),
                          HTML("<p style='color: black;'>1. name: Podcast name</p>"),
                          HTML("<p style='color: black;'>2. Sentiment_Polarity: This chart shows the sentiment polarity score of each podcast, representing the emotional tendency of the podcast content.</p>"),
                          HTML("<p style='color: black;'>3. Novelty_Score: This chart displays the novelty score of each podcast, reflecting the uniqueness of the podcast content.</p>"),
                          HTML("<p style='color: black;'>4. Radar Chart - Avg_Word_Length: Average word length in the podcast description.</p>"),
                          HTML("<p style='color: black;'>5. Radar Chart - Sentence_Length: Sentence length in the podcast description.</p>"),
                          HTML("<p style='color: black;'>6. Radar Chart - Flesch_Score: Readability score of the podcast description.</p>")
                 )
                 
               )
        )
      ),
      
      # Contact Information section
      fluidRow(
        column(12,
               div(class = "contact-info",
                   HTML("<p>Contact Information:
                   <br>Contact app maintainer: <a href='mailto:zchen2353@wisc.edu'>zchen2353@wisc.edu</a>
                   <br>Contributor: Zhengyong Chen, Leyan Sun</p>")
               )
        )
      )
  )
)



server <- function(input, output, session) {
  
  podcasts <- reactive({
    req(input$search)
    
    tryCatch({
      access_token <- get_spotify_access_token()
      
      raw_shows <- spotifyr::search_spotify(input$keyword, type = "show", limit = input$num_podcasts) %>%
        as_tibble()
      
      processed_data <- raw_shows %>%
        filter(!is.na(description)) %>%
        select(name, description)
      
      if (nrow(processed_data) == 0) stop("No podcasts found for the given keyword.")
      
      processed_data
    }, error = function(e) {
      showNotification(paste("Error: ", e$message), type = "error")
      return(data.frame(name = character(), description = character()))
    })
  })
  
  
  # DataTable showing podcast listings as pagination
  output$podcast_table <- renderDT({
    data <- podcasts()
    
    if (nrow(data) > 0) {
      datatable(
        data,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20),
          searchHighlight = TRUE
        ),
        rownames = FALSE
      )
    }
  })
  
  # Dynamic podcast selector
  output$podcast_selector <- renderUI({
    data <- cluster_results()$data
    selectInput("selected_podcast", "Choose a Specific Podcast:", choices = data$name, width = '100%')
  })
  
  # Show podcast name and description
  observe({
    data <- podcasts()
    lapply(1:nrow(data), function(i) {
      output[[paste0("podcast_name_", i)]] <- renderText({data$name[i]})
      output[[paste0("podcast_desc_", i)]] <- renderText({
        # Show only the first three lines of description
        description <- data$description[i]
        trimmed_desc <- strsplit(description, "\n")[[1]]
        paste(trimmed_desc[1:min(3, length(trimmed_desc))], collapse = "\n")
      })
    })
  })
  
  # Clustering results and calculation of new indicators
  cluster_results <- reactive({
    data <- podcasts()

    # Text cleaning
    corpus <- Corpus(VectorSource(data$description))
    corpus_clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords("en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus_clean)
    tfidf <- weightTfIdf(dtm)
    tfidf_matrix <- as.matrix(tfidf)
    
    if (nrow(tfidf_matrix) == 0 || ncol(tfidf_matrix) == 0) {
      stop("TF-IDF matrix is empty. Please check the input data.")
    }
    
    # PCA
    pca_result <- prcomp(tfidf_matrix, scale. = TRUE)
    pca_matrix <- pca_result$x[, 1:10]
    
    # Emotional analysis
    sentiment_dict <- get_sentiments("bing")
    sentiment_scores <- data %>%
      unnest_tokens(word, description) %>%
      inner_join(sentiment_dict, by = "word") %>%
      group_by(name) %>%
      summarise(Sentiment_Polarity = sum(ifelse(sentiment == "positive", 1, -1), na.rm = TRUE)) %>%
      ungroup()
    
    # Novelty calculations
    similarity_matrix <- sim2(tfidf_matrix, method = "cosine")
    novelty_scores <- apply(similarity_matrix, 1, function(row) {
      mean(1 - row)
    })
    
    # Text complexity indicators
    data <- data %>%
      mutate(
        Avg_Word_Length = str_length(description) / str_count(description, "\\S+"), # 平均单词长度
        Sentence_Count = pmax(str_count(description, "\\.|\\!|\\?"), 1), # 至少有1个句子
        Sentence_Length = str_count(description, "\\S+") / Sentence_Count, # 平均句子长度
        Flesch_Score = 206.835 - (1.015 * (str_count(description, "\\S+") / Sentence_Count)) -
          (84.6 * (str_count(description, "[aeiouAEIOU]") / str_count(description, "\\S+"))) # Flesch 阅读难度公式
      )
    # Consolidate all indicators
    data <- data %>%
      mutate(Novelty_Score = novelty_scores) %>%
      left_join(sentiment_scores, by = "name") %>%
      mutate(Sentiment_Polarity = replace_na(Sentiment_Polarity, 0))
    
    # K-means clustering
    kmeans_result <- kmeans(data[,c(3:8)], centers = input$clusters)
    
    # Add clustering results to data
    data$Cluster <- as.factor(kmeans_result$cluster)
    
    # Calculate the mean of each cluster
    cluster_means <- data[,c(3:9)] %>%
      group_by(Cluster) %>%
      summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")
    
 
    # Return results
    list(
      data = data,
      kmeans_result = kmeans_result,
      pca_matrix = pca_matrix,
      cluster_means = cluster_means
    )
    
  })
  
  # Cluster visualization
  output$cluster_plot <- renderPlotly({
    result <- cluster_results()
    
    fviz_plot <- fviz_cluster(
      result$kmeans_result,
      data = result$data[, c(3:8)],
      geom = "point",
      ellipse.type = "convex",
      ggtheme = theme_minimal()
    )
    
    
    ggplot_data <- fviz_plot$data
    ellipse_data <- ggplot_build(fviz_plot)$data[[2]]
    ggplot_data$Name <- result$data$name
    
   
    ggplot_data$cluster <- as.character(ggplot_data$cluster)
    ellipse_data$group <- as.character(ellipse_data$group)
    
    
    interactive_plot <- ggplot() +
      geom_polygon(
        data = ellipse_data,
        aes(x = x, y = y, group = group, fill = group),
        alpha = 0.2,
        color = NA,
        show.legend = FALSE
      ) +
      geom_point(
        data = ggplot_data,
        aes(x = x, y = y, color = cluster, text = Name),
        size = 3
      ) +
      theme_minimal() +
      labs(
        title = "K-means Cluster Visualization",
        x = "PC1",
        y = "PC2",
        color = "Cluster"
      ) +
      theme(legend.position = "bottom")
    
    
    interactive_plotly <- ggplotly(interactive_plot, tooltip = "text")
    
    for (i in seq_along(interactive_plotly$x$data)) {
      if (!is.null(interactive_plotly$x$data[[i]]$name)) {
       
        interactive_plotly$x$data[[i]]$name <- sub("\\((\\d+),.*\\)", "\\1", interactive_plotly$x$data[[i]]$name)
      }
    }
    
    
    
    interactive_plotly
})
  
  
  output$cluster_description <- renderUI({
    result <- cluster_results()
    cluster_means <- result$cluster_means
    
    
    dominant_clusters <- apply(cluster_means[-1], 2, function(column) {
      max_cluster <- cluster_means$Cluster[which.max(column)]
      return(max_cluster)
    })
    
    
    column_names <- names(cluster_means)[-1] # 除去 Cluster 列
    feature_names <- c(
      "Avg_Word_Length" = "Complex Vocabulary",
      "Sentence_Length" = "Long Sentences",
      "Flesch_Score" = "Easy to Read",
      "Sentence_Count" = "Detailed",
      "Novelty_Score" = "Highly Novel",
      "Sentiment_Polarity" = "Positive Sentiment"
    )
    
    
    column_descriptions <- mapply(function(col, cluster) {
      paste0(feature_names[col], ": Dominant in Cluster ", cluster)
    }, column_names, dominant_clusters)
    
    
    cluster_descriptions <- lapply(1:nrow(cluster_means), function(i) {
      cluster_id <- cluster_means$Cluster[i]
      mean_values <- round(as.numeric(cluster_means[i, -1]), 2)
      names(mean_values) <- column_names
      
      mean_text <- paste(names(mean_values), mean_values, collapse = ", ")
      paste0("Cluster ", cluster_id, ": ", mean_text)
    })
    
    
    all_descriptions <- c(column_descriptions, cluster_descriptions)
    
    
    description_html <- lapply(all_descriptions, function(desc) {
      HTML(paste0("<p>", desc, "</p>"))
    })
    
    tagList(description_html)
  })
  
  # Sentiment Polarity Visualization
  output$sentiment_plot <- renderPlot({
    data <- cluster_results()$data
    ggplot(data, aes(x = reorder(name, Sentiment_Polarity), y = Sentiment_Polarity, fill = Cluster)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Sentiment Polarity of Podcasts",
           x = "Podcast Name", y = "Sentiment Polarity") +
      theme_minimal()
  })
  
  # Novelty Score Visualization
  output$novelty_plot <- renderPlot({
    data <- cluster_results()$data
    ggplot(data, aes(x = reorder(name, Novelty_Score), y = Novelty_Score, color = Cluster)) +
      geom_point(size = 3) +
      coord_flip() +
      labs(title = "Novelty Score of Podcasts",
           x = "Podcast Name", y = "Novelty Score") +
      theme_minimal()
  })
  
  # Radar map visualization
  output$text_complexity_radar <- renderPlot({
    req(input$selected_podcast)  # 确保用户选择了播客
    data <- cluster_results()$data
    selected <- data %>% filter(name == input$selected_podcast)
    
    if (nrow(selected) == 0) return(NULL) 
    selected<<-selected
    
    radar_data <- selected %>%
      select(Avg_Word_Length, Sentence_Length, Flesch_Score) %>%
      as.data.frame()
    
    
    radar_data <- rbind(rep(max(radar_data), ncol(radar_data)), 
                        rep(min(radar_data), ncol(radar_data)), 
                        radar_data)
    
    radarchart(radar_data, axistype = 1,
               pcol = "orange", pfcol = rgb(1, 0.5, 0, 0.5), plwd = 2,
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, max(radar_data), length.out = 5), cglwd = 0.8,
               vlcex = 0.8)
    title(paste("Radar Chart: Text Complexity of", input$selected_podcast))
  })
  
  # Recent Neighborhood Podcasts
  output$nearest_podcasts <- renderTable({
    selected_name <- input$selected_podcast
    data <- cluster_results()$data
    selected <- data %>% filter(name == selected_name)
    
    if (nrow(selected) == 0) return(NULL)
    
    selected_cluster <- selected$Cluster
    nearest <- data %>%
      filter(Cluster == selected_cluster) %>%
      arrange(abs(Novelty_Score - selected$Novelty_Score))
    
    nearest %>% select(name, Sentiment_Polarity, Novelty_Score,Sentence_Count, Avg_Word_Length, Sentence_Length,
                       Flesch_Score)
  })
}


shinyApp(ui = ui, server = server)
