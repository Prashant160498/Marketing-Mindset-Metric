# To get this working you need to load the rdata into the workspace, then run Marketing_Revised.R in full to obtain the model parameters
# After which you will execute this file to load the dashboard

library(shiny)
library(ggplot2)
library(shinydashboard)

# Assuming df is your dataframe
# Add a Day column if not already present
df$Day <- 1:nrow(df)

# Constants for the awareness calculation
coeffs_a <- coef(response_aware)
gamma_a <- coeffs_a[2]
beta_1_a <- coeffs_a[3]
beta_2_a <- coeffs_a[4]
beta_3_a <- coeffs_a[5]
beta_4_a <- coeffs_a[6]
beta_5_a <- coeffs_a[7]
Intercept_a <- coeffs_a[1]

coeffs_c <- coef(response_consideration)
gamma_c <- coeffs_c[2]
beta_1_c <- coeffs_c[3]
beta_2_c <- coeffs_c[4]
beta_3_c <- coeffs_c[5]
beta_4_c <- coeffs_c[6]
beta_5_c <- coeffs_c[7]
Intercept_c <- coeffs_c[1]

coeffs_l <- coef(response_liking)
gamma_l <- coeffs_l[2]
beta_1_l <- coeffs_l[3]
beta_2_l <- coeffs_l[4]
beta_3_l <- coeffs_l[5]
beta_4_l <- coeffs_l[6]
beta_5_l <- coeffs_l[7]
Intercept_l <- coeffs_l[1]

coeffs_sh <- coef(response_sales)

coeffs_s <- coef(conversion)
gamma_s <- coeffs_s[2]
beta_1_s <- coeffs_s[3]
beta_2_s <- coeffs_s[4]
beta_3_s <- coeffs_s[5]
Intercept_s <- coeffs_s[1]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Dirtea"), skin = "red",
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series Visualization", tabName = "line_plot", icon = icon("line-chart")),
      menuItem("Mindset Metrics", tabName = "awareness_calc", icon = icon("calculator")),
      menuItem("Short-Run & Long-Run Gains", tabName = "short_term_gains", icon = icon("percentage"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML(".content-wrapper { min-height: 1200px; }")),
    tabItems(
      tabItem(tabName = "line_plot",
              h3("Time Series Visualization"),
              fluidRow(
                column(6,
                       sliderInput(
                         inputId = "day_range",
                         label = "Select Day Range:",
                         min = min(df$Day),
                         max = max(df$Day),
                         value = c(min(df$Day), max(df$Day)),
                         step = 1
                       )
                ),
                column(6,
                       selectInput(
                         inputId = "feature",
                         label = "Select a Feature to Plot:",
                         choices = c("Awareness" = "Awareness",
                                     "Consideration" = "Consideration",
                                     "Liking" = "Liking",
                                     "Sales" = "Sales"),
                         selected = "Sales"
                       )
                )
              ),
              box(
                title = "Line Plot",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                plotOutput("linePlot")
              )
      ),
      
      tabItem(tabName = "awareness_calc",
              h3("Mindset Metric Calculator"),
              
              box(
                title = "Instructions",
                status = "info",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,  # Set to TRUE if you want it collapsed by default
                width = 12,
                p("Follow these steps to use the Mindset Metric Calculator effectively:"),
                tags$ol(
                  tags$li("Enter the budgets for Instagram Ads, TikTok Ads, SEA, Point of Sale Promotions, and Influencer Collaborations in the respective fields."),
                  tags$li("Previous values for Sales, Awareness, Consideration, and Liking are updated automatically after each submission."),
                  tags$li("The day number increments automatically when you click the 'Submit Day' button."),
                  tags$li("Click the 'Submit Day' button to calculate and save metrics for the entered values."),
                  tags$li("The calculated metrics (Awareness, Consideration, Liking, and Sales) will be displayed in the respective boxes."),
                  tags$li("Use the 'Sales Trend' plot to visualize the trends in Sales over time. You can toggle between Sales, Awareness, Consideration, and Liking trends using the dropdown."),
                  tags$li("Click the 'Reset' button to clear all inputs and reset the day counter and metrics to their initial values.")
                )
              ),
              
              fluidRow(
                column(6,
                       numericInput(
                         inputId = "day_number",
                         label = "Enter Day Number:",
                         value = 1,
                         min = 1
                       ),
                       numericInput(
                         inputId = "InstagramAds",
                         label = "Instagram Ads Budget:",
                         value = tail(df$InstagramAds, 1),
                         min = 0
                       ),
                       numericInput(
                         inputId = "TikTokAds",
                         label = "TikTok Ads Budget:",
                         value = tail(df$TikTokAds, 1),
                         min = 0
                       ),
                       numericInput(
                         inputId = "SEA",
                         label = "SEA Budget:",
                         value = tail(df$SEA, 1),
                         min = 0
                       ),
                       numericInput(
                         inputId = "PoSPromotions",
                         label = "Point of Sale Promotions:",
                         value = tail(df$PoSPromotions, 1),
                         min = 0
                       ),
                       numericInput(
                         inputId = "InfluencerColab",
                         label = "Influencer Collaborations:",
                         value = tail(df$InfluencerColabs, 1),
                         min = 0
                       )
                ),
                column(6,
                       numericInput(
                         inputId = "PrevSales",
                         label = "Previous Sales:",
                         value = round(tail(df$Sales, 1), 1),
                         min = 0
                       ),
                       numericInput(
                         inputId = "PrevAware",
                         label = "Previous Awareness Value:",
                         value = round(tail(df$Awareness, 1), 1),
                         min = 0
                       ),
                       numericInput(
                         inputId = "PrevConsider",
                         label = "Previous Consideration Value:",
                         value = round(tail(df$Consideration, 1), 1),
                         min = 0
                       ),
                       numericInput(
                         inputId = "PrevLike",
                         label = "Previous Liking Value:",
                         value = round(tail(df$Liking, 1), 1),
                         min = 0
                       ),
                       actionButton("submit_day", "Submit Day"),
                       actionButton("reset_button", "Reset", icon = icon("refresh"))
                )
              ),
              fluidRow(
                box(
                  title = "Calculated Awareness",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  style = "font-size: 24px; text-align: center;",
                  textOutput("awareness_value")
                ),
                box(
                  title = "Calculated Consideration",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  style = "font-size: 24px; text-align: center;",
                  textOutput("consideration_value")
                ),
                box(
                  title = "Calculated Liking",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  style = "font-size: 24px; text-align: center;",
                  textOutput("liking_value")
                ),
                box(
                  title = "Sales",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  style = "font-size: 24px; text-align: center;",
                  textOutput("sales_value")
                )
              ),
              fluidRow(
                column(3, 
                       selectInput(
                         inputId = "trend_metric",
                         label = "Select Metric to Plot:",
                         choices = c("Sales", "Awareness", "Consideration", "Liking"),
                         selected = "Sales"
                       )
                )
              ),
              fluidRow(
                box(
                  title = "Trend Plot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("trend_plot")  # Updated plot ID
                )
              ),
              fluidRow(
                box(
                  title = "Metrics Table",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("metrics_table")
                )
              )
              
      ),
      
      tabItem(tabName = "short_term_gains",
              h3("Mindset Metric & Sales Gain"),
              fluidRow(
                column(6,
                       selectInput(
                         inputId = "marketing_tool",
                         label = "Select Marketing Tool:",
                         choices = c("Instagram Budget" = "InstagramAds",
                                     "TikTok Budget" = "TikTokAds",
                                     "SEA Budget" = "SEA",
                                     "Point of Sale Budget" = "PoSPromotions",
                                     "Influencer Collaboration Budget" = "InfluencerColab"),
                         selected = "InstagramAds"
                       ),
                       numericInput(
                         inputId = "multiplier",
                         label = "Enter Multiplier:",
                         value = 1,
                         min = 0
                       ),
                       actionButton("compute_gain", "Compute Short Term Gains")
                )
              ),
              fluidRow(
                box(
                  title = "Short Term Gains",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("short_term_results")  # Output for short-run results
                ),
                box(
                  title = "Long Term Gains",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("long_term_results")    # Output for long-run results
                ),
                box(
                  title = "Contributions (Awareness, Consideration, Liking)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  tableOutput("contributions_results")
                ),
                box(
                  title = "Sales Gain Breakdown",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("sales_gain_breakdown_results")
                )
                
              )
              
              
      )
      
      
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Line Plot: Reactive filtering of data
  output$linePlot <- renderPlot({
    filtered_data <- df[df$Day >= input$day_range[1] & df$Day <= input$day_range[2], ]
    feature_data <- filtered_data[, c("Day", input$feature)]
    colnames(feature_data) <- c("Day", "Value")
    
    ggplot(feature_data, aes(x = Day, y = Value)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red") +
      labs(
        title = paste("Line Plot for", input$feature),
        x = "Day",
        y = input$feature
      ) +
      theme_minimal()
  })
  # Reactive value to store sales trend
  sales_trend <- reactiveVal(data.frame(Day = numeric(0), Sales = numeric(0)))
  
  # Reactive value to store metrics
  metrics <- reactiveVal(data.frame(Day = numeric(0), InstagramAds = numeric(0), TikTokAds = numeric(0), SEA = numeric(0), PoSPromotions = numeric(0), InfluencerColab = numeric(0), Awareness = numeric(0), Consideration = numeric(0), Liking = numeric(0), Sales = numeric(0)))
  
  # Awareness Calculation: Reactive calculation
  awareness_calculation <- reactive({
    response_aware <- (input$PrevAware+1)^gamma_a *
      (input$InstagramAds+1)^beta_1_a *
      (input$TikTokAds+1)^beta_2_a *
      (input$SEA+1)^beta_3_a *
      (input$PoSPromotions+1)^beta_4_a *
      (input$InfluencerColab+1)^beta_5_a *
      exp(Intercept_a)-1
    return(response_aware)
  })
  
  consideration_calculation <- reactive({
    response_consideration <- (input$PrevConsider+1)^gamma_c *
      (input$InstagramAds+1)^beta_1_c *
      (input$TikTokAds+1)^beta_2_c *
      (input$SEA+1)^beta_3_c *
      (input$PoSPromotions+1)^beta_4_c *
      (input$InfluencerColab+1)^beta_5_c *
      exp(Intercept_c)-1
    return(response_consideration)
  })
  
  liking_calculation <- reactive({
    response_liking <- (input$PrevLike+1)^gamma_l *
      (input$InstagramAds+1)^beta_1_l *
      (input$TikTokAds+1)^beta_2_l *
      (input$SEA+1)^beta_3_l *
      (input$PoSPromotions+1)^beta_4_l *
      (input$InfluencerColab+1)^beta_5_l *
      exp(Intercept_l)-1
    return(response_liking)
  })
  
  sales_calculation <- reactive({
    aware <- awareness_calculation()
    consider <- consideration_calculation()
    like <- liking_calculation()
    
    response_sales <- (input$PrevSales+1)^gamma_s *
      (aware+1)^beta_1_s *
      (consider+1)^beta_2_s *
      (like+1)^beta_3_s *
      exp(Intercept_s)-1
    return(response_sales)
  })
  
  # Update Calculated Outputs
  output$awareness_value <- renderText({
    format(round(awareness_calculation(), 2), big.mark = ",")
  })
  
  output$consideration_value <- renderText({
    format(round(consideration_calculation(), 2), big.mark = ",")
  })
  
  output$liking_value <- renderText({
    format(round(liking_calculation(), 2), big.mark = ",")
  })
  
  output$sales_value <- renderText({
    format(round(sales_calculation(), 2), big.mark = ",")
  })
  
  day_counter <- reactiveVal(1)
  # Update Metrics on Submit
  observeEvent(input$submit_day, {
    new_metrics <- data.frame(
      Day = day_counter(),
      InstagramAds = input$InstagramAds,
      TikTokAds = input$TikTokAds,
      SEA = input$SEA,
      PoSPromotions = input$PoSPromotions,
      InfluencerColab = input$InfluencerColab,
      Awareness = awareness_calculation(),
      Consideration = consideration_calculation(),
      Liking = liking_calculation(),
      Sales = sales_calculation()
    )
    updated_metrics <- rbind(metrics(), new_metrics)
    updated_metrics <- updated_metrics[order(updated_metrics$Day), ]
    metrics(updated_metrics)
    day_counter(day_counter() + 1)
    
    # Update Previous Sales to the Sales value of the last day
    if (nrow(updated_metrics) > 0) {
      last_sales <- updated_metrics$Sales[nrow(updated_metrics)]
      last_aware <- updated_metrics$Awareness[nrow(updated_metrics)]
      last_consider <- updated_metrics$Consideration[nrow(updated_metrics)]
      last_like <- updated_metrics$Liking[nrow(updated_metrics)]
    } 
    updateNumericInput(session, "PrevAware", value = round(last_aware, 2))
    
    updateNumericInput(session, "PrevConsider", value = round(last_consider, 2))
    
    updateNumericInput(session, "PrevLike", value = round(last_like, 2))
    
    updateNumericInput(session, "PrevSales", value = round(last_sales, 2))
    
    updateNumericInput(session, "day_number", value = day_counter())
  })
  
  observeEvent(input$reset_button, {
    # Reset day counter to the initial value
    day_counter(1)
    
    # Reset all input fields to their starting values
    updateNumericInput(session, "day_number", value = 1)
    updateNumericInput(session, "PrevSales", value = round(tail(df$Sales, 1), 1))
    updateNumericInput(session, "PrevAware", value = round(tail(df$Awareness, 1), 1))
    updateNumericInput(session, "PrevConsider", value = round(tail(df$consideration_transformed, 1), 1))
    updateNumericInput(session, "PrevLike", value = round(tail(df$liking_transformed, 1), 1))
    updateNumericInput(session, "InstagramAds", value = tail(df$InstagramAds, 1))
    updateNumericInput(session, "TikTokAds", value = tail(df$TikTokAds, 1))
    updateNumericInput(session, "SEA", value = tail(df$SEA, 1))
    updateNumericInput(session, "PoSPromotions", value = tail(df$PoSPromotions, 1))
    updateNumericInput(session, "InfluencerColab", value = tail(df$InfluencerColabs, 1))
    
    # Clear the metrics table
    metrics(data.frame(Day = numeric(0), InstagramAds = numeric(0), TikTokAds = numeric(0),
                       SEA = numeric(0), PoSPromotions = numeric(0), InfluencerColab = numeric(0),
                       Awareness = numeric(0), Consideration = numeric(0), Liking = numeric(0),
                       Sales = numeric(0)))
  })
  
  
  
  # Render Metrics Table
  output$metrics_table <- renderTable({
    metrics()
  })
  
  # Render Sales Trend Plot
  output$trend_plot <- renderPlot({
    # Get the selected metric from the dropdown
    selected_metric <- input$trend_metric
    
    # Filter data for the selected metric
    trend_data <- metrics()[, c("Day", selected_metric)]
    colnames(trend_data) <- c("Day", "Value")  # Rename columns for ggplot
    
    # Create the trend plot
    ggplot(trend_data, aes(x = Day, y = Value)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 3) +
      labs(
        title = paste(selected_metric, "Trend Over Days"),
        x = "Day",
        y = selected_metric
      ) +
      theme_minimal()
  })
  
  observeEvent(input$compute_gain, {
    # Get the selected marketing tool and multiplier
    tool <- input$marketing_tool
    multiplier <- input$multiplier
    
    # Determine 'z' based on the selected tool for each mindset metric
    z_awareness <- switch(tool,
                          InstagramAds = coeffs_a[3],
                          TikTokAds = coeffs_a[4],
                          SEA = coeffs_a[5],
                          PoSPromotions = coeffs_a[6],
                          InfluencerColab = coeffs_a[7])
    
    z_consideration <- switch(tool,
                              InstagramAds = coeffs_c[3],
                              TikTokAds = coeffs_c[4],
                              SEA = coeffs_c[5],
                              PoSPromotions = coeffs_c[6],
                              InfluencerColab = coeffs_c[7])
    
    z_liking <- switch(tool,
                       InstagramAds = coeffs_l[3],
                       TikTokAds = coeffs_l[4],
                       SEA = coeffs_l[5],
                       PoSPromotions = coeffs_l[6],
                       InfluencerColab = coeffs_l[7])
    
    z_sales <- switch(tool,
                      InstagramAds = coeffs_sh[3],
                      TikTokAds = coeffs_sh[4],
                      SEA = coeffs_sh[5],
                      PoSPromotions = coeffs_sh[6],
                      InfluencerColab = coeffs_sh[7])
    # -------------------------------
    # Short-Run GAIN: (multiplier^z) - 1
    # Store them in numeric form first
    # -------------------------------
    short_run_vals <- c(
      (multiplier^z_awareness - 1),
      (multiplier^z_consideration - 1),
      (multiplier^z_liking - 1),
      (multiplier^z_sales - 1)
    )
    
    # -------------------------------
    # Long-Run GAIN: short-run * coeffs_x[2]
    # -------------------------------
    long_run_vals <- c(
      short_run_vals[1] * 1/(1-coeffs_a[2]),  # Awareness
      short_run_vals[2] * 1/(1-coeffs_c[2]),  # Consideration
      short_run_vals[3] * 1/(1-coeffs_l[2]),  # Liking
      short_run_vals[4] * 1/(1-coeffs_s[2])   # Sales
    )
    
    # Create data frames for short-run and long-run
    short_term_gains <- data.frame(
      Metric = c("Awareness", "Consideration", "Liking", "Sales"),
      Gain = round(short_run_vals, 5)  # Round to 5 decimal places
    )
    
    long_term_gains <- data.frame(
      Metric = c("Awareness", "Consideration", "Liking", "Sales"),
      Gain = round(long_run_vals, 5)
    )
    
    contrib_aware  <- long_run_vals[1] * coeffs_s[3]
    contrib_cons   <- long_run_vals[2] * coeffs_s[4]
    contrib_liking <- long_run_vals[3] * coeffs_s[5]
    
    contributions <- data.frame(
      Metric       = c("Awareness", "Consideration", "Liking"),
      Contribution = round(c(contrib_aware, contrib_cons, contrib_liking), 5)
    )
    
    
    # Render Short-Run Gains Table
    output$short_term_results <- renderTable({
      # Format to show exactly 5 decimals
      short_term_gains$Gain <- format(short_term_gains$Gain, nsmall = 5)
      short_term_gains
    }, rownames = FALSE)
    
    # -------------------------------
    # Render Long-Run Gains Table
    # -------------------------------
    output$long_term_results <- renderTable({
      long_term_gains$Gain <- format(long_term_gains$Gain, nsmall = 5)
      long_term_gains
    }, rownames = FALSE)
    
    output$contributions_results <- renderTable({
      contributions$Contribution <- format(contributions$Contribution, nsmall = 5)
      contributions
    }, rownames = FALSE)
    
    # 1) Long-Term Sales Gain (A)
    A <- long_run_vals[4]
    
    # 2) Sales Gain due to Mindset Metrics (B)
    B <- contrib_aware + contrib_cons + contrib_liking
    
    # 3) Sales Gain due to Transactions = A - B
    sales_gain_txn <- A - B
    
    # Create a small data frame to display these three values
    sales_gain_breakdown <- data.frame(
      Item  = c("Long Term Sales Gain (A)",
                "Sales Gain due to Mindset Metrics (B)",
                "Sales Gain due to Transactions (A - B)"),
      Value = round(c(A, B, sales_gain_txn), 5)
    )
    
    output$sales_gain_breakdown_results <- renderTable({
      # Ensure 5 decimal places
      sales_gain_breakdown$Value <- format(sales_gain_breakdown$Value, nsmall = 5)
      sales_gain_breakdown
    }, rownames = FALSE)
    
  })
  
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)

