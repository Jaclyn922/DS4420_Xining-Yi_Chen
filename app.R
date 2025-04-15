library(shiny)
library(brms)
library(jsonlite)
library(ggplot2)
library(dplyr)

ui <- navbarPage(
  title = "Sephora In-Store Recommender",
  tabPanel("Home",
           fluidPage(
             titlePanel("Welcome to Sephora In-Store Recommender App"),
             
             # add photo
             div(style = "text-align: center; padding: 20px 0;",
                 tags$img(src = "sephora_store.jpg", 
                          width = "100%", 
                          style = "max-width: 800px; 
                                   border-radius: 8px; 
                                   box-shadow: 0 4px 8px rgba(0,0,0,0.1);")
             ),
             
             br(),
             div(style = "max-width: 800px; margin: 0 auto; padding: 20px;",
                 h4("Why We Built This App", style = "color: #333; margin-bottom: 20px;"),
                 p("Sometimes when we walk into Sephora, we don't have a clear goal. We are just browsing and see if anything suit us. But the moment sales person come over, they will ask 'what are you looking for? do you need a recommendation?' - and the suggestions are almost and always the latest or most popular products.", 
                   style = "font-size: 16px; line-height: 1.6;"),
                 p("That is not personalized. It gets worse when you think about how hard it is for sales associates to remember all the products in the store — the details, the differences — they just can't. So their recommendations are usually based on experience, not a full comparison. That means customers might miss out on great options that actually fit them better.",
                   style = "font-size: 16px; line-height: 1.6;"),
                 p("That is why we built this app - to fill that gap.",
                   style = "font-size: 16px; line-height: 1.6; font-weight: bold;")
             ),
             
             br(),
             h5("Created by Xining Xu, Yi Chen Wu for DS 4420", 
                style = "color: #666; font-style: italic; text-align: right;")
           )
  ),
  tabPanel("Interactive Recommendation",
           sidebarLayout(
             sidebarPanel(
               h4("Your Preferences"),
               selectInput("price_level", "Price Preference",
                           choices = c("Low", "Medium", "High", "Luxury"),
                           selected = "Medium"),
               selectInput("rating_level", "Rating Preference",
                           choices = c("Low", "Medium", "High"),
                           selected = "High"),
               selectInput("loves_level", "Popularity (Loves)",
                           choices = c("Few", "Medium", "Popular", "Hot"),
                           selected = "Popular"),
               br(),
               h4("Browse Products"),
               selectInput("primary_category_select", "Browse Primary Category:",
                           choices = NULL,
                           selected = "Makeup"),
               selectInput("secondary_category_select", "Browse Secondary Category:",
                           choices = NULL,
                           selected = "Lip"),
               selectInput("product_name", "Select a product you like:",
                           choices = NULL,
                           selected = "Dior Addict Lip Glow"),
               br(),
               actionButton("generate", "Generate Recommendations")
             ),
             mainPanel(
               h4("Predicted Categories:"),
               verbatimTextOutput("result"),
               br(),
               h4("Selected Product:"),
               verbatimTextOutput("chosen_product"),
               br(),
               h4("Category Prediction Probabilities:"),
               plotOutput("posterior_plot"),
               br(),
               h4("Top 5 Similar Products:"),
               tableOutput("top_products"),
               br(),
               h4("Top 5 Hybrid Recommendations (CF + Bayesian):"),
               p("These recommendations combine item similarity and category predictions"),
               tableOutput("hybrid_recommendations")
             )
           )
  )
)

server <- function(input, output, session) {
  ## 1. read and pre-clean
  df <- read.csv("product_info.csv")
  df <- df[, c("product_name", "price_usd", "rating", "loves_count", "reviews",
               "online_only", "primary_category", "secondary_category")]
  df <- na.omit(df)
  df$online_only <- as.factor(df$online_only)
  
  # define the range on the website
  df$price_level <- cut(df$price_usd, breaks = c(0, 30, 70, 200, Inf),
                        labels = c("Low", "Medium", "High", "Luxury"))
  df$rating_level <- cut(df$rating, breaks = c(0, 3.5, 4.2, 5),
                         labels = c("Low", "Medium", "High"))
  df$loves_level <- cut(df$loves_count, breaks = c(0, 1000, 5000, 10000, Inf),
                        labels = c("Few", "Medium", "Popular", "Hot"))
  
  # save new user
  price_levels  <- levels(df$price_level)
  rating_levels <- levels(df$rating_level)
  loves_levels  <- levels(df$loves_level)
  online_levels <- levels(df$online_only)
  
  # load baysian modeling result
  fit <- readRDS("brms_model_final.rds")
  
  # initial
  observe({
    updateSelectInput(session, "primary_category_select",
                      choices = unique(df$primary_category),
                      selected = "Makeup")
    
    sec_choices <- unique(df[df$primary_category == "Makeup", "secondary_category"])
    updateSelectInput(session, "secondary_category_select",
                      choices = sec_choices,
                      selected = "Lip")
    
    prod_choices <- df[df$primary_category == "Makeup" &
                         df$secondary_category == "Lip", "product_name"]
    updateSelectInput(session, "product_name",
                      choices = prod_choices,
                      selected = prod_choices[1])
  })
  
  # main change- then to react to show secondary
  observeEvent(input$primary_category_select, {
    sec_choices <- unique(df[df$primary_category == input$primary_category_select, "secondary_category"])
    updateSelectInput(session, "secondary_category_select",
                      choices = sec_choices,
                      selected = sec_choices[1])
  })
  
  # change product name
  observeEvent(input$secondary_category_select, {
    prod_choices <- df[df$primary_category == input$primary_category_select &
                         df$secondary_category == input$secondary_category_select, "product_name"]
    updateSelectInput(session, "product_name",
                      choices = prod_choices,
                      selected = prod_choices[1])
  })
  
  ## 2. CF
  get_similar_products <- function(selected_product, top_n = 5) {
    selected_info <- df[df$product_name == selected_product, ]
    if(nrow(selected_info) == 0) return(NULL)
    if(nrow(selected_info) > 1) selected_info <- selected_info[1, ]
    
    selected_price  <- as.numeric(selected_info$price_usd)
    selected_rating <- as.numeric(selected_info$rating)
    selected_loves  <- as.numeric(selected_info$loves_count)
    selected_subcat <- selected_info$secondary_category
    
    price_range  <- max(df$price_usd) - min(df$price_usd)
    rating_range <- max(df$rating) - min(df$rating)
    loves_range  <- max(df$loves_count) - min(df$loves_count)
    
    similarity_scores <- numeric(nrow(df))
    
    for(i in 1:nrow(df)) {
      price_sim  <- 1 - abs(df$price_usd[i] - selected_price) / price_range
      rating_sim <- 1 - abs(df$rating[i] - selected_rating) / rating_range
      loves_sim  <- 1 - abs(df$loves_count[i] - selected_loves) / loves_range
      subcat_sim <- ifelse(df$secondary_category[i] == selected_subcat, 1, 0)
      
      similarity_scores[i] <- 0.3 * price_sim + 0.3 * rating_sim +
        0.2 * loves_sim + 0.2 * subcat_sim
    }
    
    df$similarity_score <- similarity_scores
    similar_products <- df[df$product_name != selected_product, ]
    similar_products <- similar_products[order(-similar_products$similarity_score), ]
    
    head(similar_products[, c("product_name", "price_usd", "rating", "loves_count",
                              "primary_category", "secondary_category", "similarity_score")], top_n)
  }
  
  ## 2.1 CF base on baysian: combine two
  get_bayesian_cf_recommendations <- function(selected_product, top2_categories, top_n = 5) {
    selected_info <- df[df$product_name == selected_product, ]
    if(nrow(selected_info) == 0) return(NULL)
    if(nrow(selected_info) > 1) selected_info <- selected_info[1, ]
    
    # preselect 
    df_filtered <- df[df$primary_category %in% top2_categories, ]
    if(nrow(df_filtered) == 0) return(NULL)
    
    selected_price  <- as.numeric(selected_info$price_usd)
    selected_rating <- as.numeric(selected_info$rating)
    selected_loves  <- as.numeric(selected_info$loves_count)
    
    price_range  <- max(df_filtered$price_usd) - min(df_filtered$price_usd)
    rating_range <- max(df_filtered$rating) - min(df_filtered$rating)
    loves_range  <- max(df_filtered$loves_count) - min(df_filtered$loves_count)
    
    similarity_scores <- numeric(nrow(df_filtered))
    
    for(i in 1:nrow(df_filtered)) {
      price_sim  <- 1 - abs(df_filtered$price_usd[i] - selected_price) / price_range
      rating_sim <- 1 - abs(df_filtered$rating[i] - selected_rating) / rating_range
      loves_sim  <- 1 - abs(df_filtered$loves_count[i] - selected_loves) / loves_range
      
      similarity_scores[i] <- 0.4 * price_sim + 0.3 * rating_sim + 0.3 * loves_sim
    }
    
    df_filtered$similarity_score <- similarity_scores
    similar_products <- df_filtered[df_filtered$product_name != selected_product, ]
    similar_products <- similar_products[order(-similar_products$similarity_score), ]
    
    head(similar_products[, c("product_name", "price_usd", "rating", "loves_count",
                              "primary_category", "secondary_category", "similarity_score")], top_n)
  }
  
  ## 3. generate
  observeEvent(input$generate, {
    if (input$price_level == "" || input$rating_level == "" || input$loves_level == "" ||
        input$primary_category_select == "" || input$secondary_category_select == "" ||
        input$product_name == "") {
      showNotification("⚠️ Please complete all fields before clicking!", type = "error")
      return(NULL)
    }
    
    ## 3.1 Bayesian 
    new_user <- data.frame(
      price_level  = factor(input$price_level, levels = price_levels),
      rating_level = factor(input$rating_level, levels = rating_levels),
      loves_level  = factor(input$loves_level, levels = loves_levels),
      online_only  = factor("0", levels = online_levels)
    )
    
    bayes_probs <- tryCatch({
      fitted(fit, newdata = new_user, summary = FALSE)
    }, error = function(e) {
      showNotification("❌ This combination is not recognized by the model. Try another.", type = "error")
      return(NULL)
    })
    
    if (is.null(bayes_probs)) return(NULL)
    
    # Posterior Mean
    posterior_means <- colMeans(bayes_probs)
    
    if (length(posterior_means) == 0 || all(is.na(posterior_means)) || all(posterior_means == 0)) {
      showNotification("❌ No valid predictions available. Please try different preferences.", type = "error")
      return(NULL)
    }
    
    # rename
    my_real_names <- c("Bath & Body", "Fragrance", "Hair", "Makeup",
                       "Men", "Mini Size", "Skincare", "Tools & Brushes")
    if (length(my_real_names) == length(posterior_means)) {
      names(posterior_means) <- my_real_names
    }
    
    # posterior_df
    posterior_df <- data.frame(
      category = names(posterior_means),
      prob = as.numeric(posterior_means)
    )
    posterior_df <- posterior_df[posterior_df$prob > 0, ]
    posterior_df <- posterior_df[order(-posterior_df$prob), ]
    top2 <- head(posterior_df$category, 2)
    
    output$result <- renderPrint({ top2 })
    output$chosen_product <- renderPrint({ input$product_name })
    
    output$posterior_plot <- renderPlot({
      ggplot(posterior_df, aes(x = reorder(category, -prob), y = prob)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(title = "Posterior Probability of Each Category",
             x = "Category", y = "Predicted Probability") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # generate
    similar_products <- get_similar_products(input$product_name)
    bayesian_cf <- get_bayesian_cf_recommendations(input$product_name, top2)
    
    if (!is.null(similar_products) && !is.null(bayesian_cf)) {
      
      similar_products$price_usd <- sprintf("$%.2f", similar_products$price_usd)
      similar_products$rating <- sprintf("%.1f", similar_products$rating)
      similar_products$loves_count <- format(similar_products$loves_count, big.mark = ",")
      
      bayesian_cf$price_usd <- sprintf("$%.2f", bayesian_cf$price_usd)
      bayesian_cf$rating <- sprintf("%.1f", bayesian_cf$rating)
      bayesian_cf$loves_count <- format(bayesian_cf$loves_count, big.mark = ",")
      
      names(similar_products) <- c("Product Name", "Price", "Rating", "Loves",
                                   "Primary Category", "Subcategory", "Similarity")
      names(bayesian_cf) <- c("Product Name", "Price", "Rating", "Loves",
                              "Primary Category", "Subcategory", "Similarity")
      
      output$top_products <- renderTable({ similar_products }, align = 'l')
      output$hybrid_recommendations <- renderTable({ bayesian_cf }, align = 'l')
      
      # save JSON
      output_json <- list(
        predicted_categories = as.list(top2),
        selected_product = input$product_name,
        cf_recommendations = split(similar_products, 1:nrow(similar_products)),
        bayesian_cf_recommendations = split(bayesian_cf, 1:nrow(bayesian_cf))
      )
      write(toJSON(output_json, pretty = TRUE, auto_unbox = TRUE), 
            file = "bayes_output.json")
    }
  })
}

shinyApp(ui = ui, server = server)