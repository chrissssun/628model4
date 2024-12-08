library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tm)
library(cluster)
library(factoextra)
library(ggplot2)

get_access_token <- function(client_id, client_secret) {
  res <- POST(
    url = "https://accounts.spotify.com/api/token",
    authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  stop_for_status(res)
  content(res)$access_token
}

# Function to fetch all episodes for a show
get_all_episodes <- function(show_id, access_token, limit = 50) {
  url <- paste0("https://api.spotify.com/v1/shows/", show_id, "/episodes")
  
  episodes <- list()
  offset <- 0  # Pagination offset
  
  repeat {
    res <- GET(
      url = url,
      add_headers(Authorization = paste("Bearer", access_token)),
      query = list(limit = limit, offset = offset)
    )
    stop_for_status(res)
    
    data <- content(res, as = "parsed", simplifyDataFrame = TRUE)
    episodes <- bind_rows(episodes, data$items)
    
    if (nrow(data$items) < limit) break
    offset <- offset + limit  # Update offset for next page
  }
  
  return(episodes)
}

# Function to fetch podcast data
fetch_podcast_data <- function(keyword, num_shows, client_id, client_secret) {
  # Get access token
  access_token <- get_access_token(client_id, client_secret)
  
  # Encode the keyword
  query_keyword <- URLencode(keyword)
  
  # Limit the number of results
  num_shows <- min(num_shows, 50)  # Spotify API allows a maximum of 50 per request
  
  # Search for podcasts matching the keyword
  search_url <- "https://api.spotify.com/v1/search"
  response <- GET(
    search_url,
    add_headers(Authorization = paste("Bearer", access_token)),
    query = list(q = query_keyword, type = "show", limit = num_shows)
  )
  
  # Check for request status
  if (http_status(response)$category != "Success") {
    stop(paste("Request failed with status code:", http_status(response)$status))
  }
  
  search_results <- content(response, as = "parsed", simplifyDataFrame = TRUE)
  shows <- search_results$shows$items
  
  # Check if results are empty
  if (is.null(shows)) {
    stop("No podcasts found for the given keyword.")
  }
  
  # Extract Name and Description
  shows_df <- data.frame(
    id = sapply(shows, function(x) x$id),
    name = sapply(shows, function(x) x$name),
    description = sapply(shows, function(x) x$description),
    stringsAsFactors = FALSE
  )
  
  return(shows_df)
}

# UI
ui <- fluidPage(
  titlePanel("Dynamic Podcast Clustering with Spotify API"),
  sidebarLayout(
    sidebarPanel(
      textInput("keyword", "Search Topic:", placeholder = "Enter a keyword..."),
      numericInput("num_podcasts", "Number of Podcasts:", value = 10, min = 1, max = 50),
      numericInput("clusters", "Number of Clusters:", value = 5, min = 2, max = 10),
      actionButton("search", "Search and Analyze Podcasts")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Podcast List", tableOutput("podcast_table")),
        tabPanel("Cluster Visualization",
                 plotOutput("cluster_plot")),
        tabPanel("Cluster Metrics",
                 plotOutput("metrics_plot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive data fetching and clustering
  cluster_results <- reactive({
    req(input$search)  # Ensure button is clicked
    
    # Spotify credentials
    client_id <- "YOUR_SPOTIFY_CLIENT_ID"
    client_secret <- "YOUR_SPOTIFY_CLIENT_SECRET"
    
    # Fetch podcast data
    podcasts <- fetch_podcast_data(input$keyword, input$num_podcasts, client_id, client_secret)
    
    # Combine name and description for text processing
    podcasts <- podcasts %>%
      mutate(
        Name = ifelse(is.na(name), "", name),
        Description = ifelse(is.na(description), "", description),
        text = paste(Name, Description, sep = " ")
      )
    
    # Create corpus and clean text
    corpus <- Corpus(VectorSource(podcasts$text))
    corpus_clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords("en")) %>%
      tm_map(stripWhitespace) %>%
      tm_map(stemDocument)
    
    # Create DTM and compute TF-IDF
    dtm <- DocumentTermMatrix(corpus_clean)
    tfidf <- weightTfIdf(dtm)
    tfidf_matrix <- as.matrix(tfidf)
    
    # Perform K-means clustering
    set.seed(42)
    kmeans_result <- kmeans(tfidf_matrix, centers = input$clusters)
    
    # Append cluster labels to data
    podcasts$Cluster <- as.factor(kmeans_result$cluster)
    
    list(podcasts = podcasts, kmeans_result = kmeans_result, tfidf_matrix = tfidf_matrix)
  })
  
  # Display podcast list
  output$podcast_table <- renderTable({
    cluster_results()$podcasts
  })
  
  # Cluster visualization
  output$cluster_plot <- renderPlot({
    result <- cluster_results()
    kmeans_result <- result$kmeans_result
    tfidf_matrix <- result$tfidf_matrix
    
    fviz_cluster(kmeans_result, data = tfidf_matrix, geom = "point",
                 ellipse.type = "convex", ggtheme = theme_minimal(),
                 main = "K-Means Cluster Visualization")
  })
  
  # Cluster metrics visualization
  output$metrics_plot <- renderPlot({
    result <- cluster_results()
    podcasts <- result$podcasts
    
    # Calculate metrics for each cluster
    cluster_metrics <- podcasts %>%
      group_by(Cluster) %>%
      summarise(
        Technology_Shareability = mean(nchar(Description), na.rm = TRUE),
        Scientific_Innovativeness = mean(nchar(Name), na.rm = TRUE)
      )
    
    # Convert to long format for plotting
    cluster_metrics_long <- cluster_metrics %>%
      pivot_longer(cols = -Cluster, names_to = "Metric", values_to = "Score")
    
    # Plot metrics for each cluster
    ggplot(cluster_metrics_long, aes(x = Cluster, y = Score, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Cluster Metrics", x = "Cluster", y = "Score") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
