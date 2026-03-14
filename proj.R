# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(DT)
library(rpart)
library(rpart.plot)
library(lubridate)
library(leaflet)
library(reshape2)
library(scales)
library(shinythemes)

# Disable scientific notation globally
options(scipen = 999)

# Load data
temp_data <- read.csv("world_temp.csv", stringsAsFactors = FALSE)

# Clean column names
colnames(temp_data) <- gsub("^X", "", colnames(temp_data))

# Ensure inputs are available before proceeding
observe({
  req(input$countrySelect)
  req(input$continentSelect)
})

# Clean and prepare data
temp_data <- temp_data %>%
  rename_all(~ gsub("\\s+", "", .)) %>%  # Remove all spaces from column names
  mutate(Continent = as.factor(Continent),
         Level = as.factor(Level)) %>%
  pivot_longer(cols = where(is.numeric),  # Select numeric columns for pivoting
               names_to = "Year", 
               values_to = "Temperature") %>%
  mutate(Year = as.numeric(Year),
         Temperature = as.numeric(Temperature)) %>%
  filter(!is.na(Temperature))

# Calculate global averages
global_avg <- temp_data %>%
  group_by(Year) %>%
  summarise(Global_Avg = mean(Temperature, na.rm = TRUE))

# UI definition with enhanced styling
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span("Global Temperature Analysis", style = "font-size: 24px; font-weight: bold;"),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("search")),
      menuItem("Statistical Analysis", tabName = "stats", icon = icon("calculator")),
      menuItem("Regression Modeling", tabName = "regression", icon = icon("chart-line")),
      menuItem("Machine Learning", tabName = "ml", icon = icon("robot"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        /* Custom fonts and general styling */
        @import url("https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap");
        
        body {
          font-family: "Roboto", sans-serif;
          background-color: #f8f9fa;
        }
        
        /* Box styling */
        .box {
          border-radius: 8px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          background-color: #ffffff;
        }
        
        /* Value box styling */
        .value-box {
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 20px;
        }
        
        /* Header styling */
        .main-header {
          background: linear-gradient(45deg, #2c3e50, #3498db);
        }
        
        .main-header .logo {
          font-weight: bold;
          font-size: 24px;
          color: white;
        }
        
        /* Sidebar styling */
        .sidebar {
          background-color: #2c3e50;
        }
        
        .sidebar-menu li a {
          color: #ecf0f1;
          font-size: 16px;
          padding: 12px 15px;
          transition: all 0.3s ease;
        }
        
        .sidebar-menu li a:hover {
          background-color: #3498db;
          color: white;
        }
        
        /* Input controls */
        .selectize-input {
          border-radius: 4px;
          border: 1px solid #ced4da;
          padding: 5px;
          z-index: 1000; /* Ensure dropdown appears above other elements */
        }
        
        .selectize-dropdown {
          z-index: 1001; /* Ensure dropdown menu is above other elements */
        }
        
        /* Button styling */
        .btn {
          border-radius: 4px;
          padding: 8px 16px;
          transition: all 0.3s ease;
        }
        
        .btn:hover {
          transform: translateY(-1px);
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        /* Plot container */
        .shiny-plot-output {
          border-radius: 6px;
          overflow: hidden;
        }
        
        /* DataTable styling */
        .dataTable {
          border-radius: 6px;
          background: white;
        }
        
        /* Responsive adjustments */
        @media (max-width: 768px) {
          .box {
            margin-bottom: 20px;
          }
          
          .value-box {
            margin-bottom: 15px;
          }
        }
      '))
    ),
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("avgTempBox"),
                valueBoxOutput("maxTempBox"),
                valueBoxOutput("minTempBox")
              ),
              fluidRow(
                box(plotlyOutput("globalTrendPlot"), width = 12, title = "Global Temperature Trend", status = "primary", solidHeader = TRUE)
              ),
              fluidRow(
                box(plotlyOutput("continentTrendPlot"), width = 6, title = "Continent Trends", status = "info", solidHeader = TRUE),
                box(leafletOutput("tempMap"), width = 6, title = "Temperature Map", status = "success", solidHeader = TRUE)
              )
      ),
      
      # Data Exploration tab
      tabItem(tabName = "explore",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE,
                    column(6, selectInput("countrySelect", "Select Country:", 
                                          choices = c("All", unique(temp_data$Country)))),
                    column(6, selectInput("continentSelect", "Select Continent:", 
                                          choices = c("All", levels(temp_data$Continent))))
                )
              ),
              fluidRow(
                box(width = 6, title = "Temperature Trend (Line Chart)", status = "info", solidHeader = TRUE,
                    plotlyOutput("countryPlot")),
                box(width = 6, title = "Yearly Temperatures (Bar Chart)", status = "info", solidHeader = TRUE,
                    plotlyOutput("countryBarPlot"))
              ),
              fluidRow(
                box(width = 6, title = "Continent Temperature Distribution", status = "success", solidHeader = TRUE,
                    plotlyOutput("continentBoxPlot")),
                box(width = 6, title = "Data Summary", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("dataSummary"))
              ),
              fluidRow(
                box(width = 12, title = "Data Table", status = "primary", solidHeader = TRUE,
                    DTOutput("dataTable"))
              )
      ),
      
      # Statistical Analysis tab
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "Global Filters", width = 12, status = "primary", solidHeader = TRUE,
                    column(6, selectInput("statYear", "Select Year:", 
                                          choices = unique(temp_data$Year))),
                    column(6, selectInput("statContinent", "Select Continent:", 
                                          choices = c("All", levels(temp_data$Continent))))
                )
              ),
              fluidRow(
                box(title = "Descriptive Statistics", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("summaryStats")),
                box(title = "Temperature Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("tempDistribution"))
              ),
              fluidRow(
                box(title = "Confidence Intervals", width = 6, status = "success", solidHeader = TRUE,
                    plotlyOutput("confidenceInterval")),
                box(title = "Probability Distribution", width = 6, status = "success", solidHeader = TRUE,
                    plotlyOutput("probabilityPlot"))
              )
      ),
      
      # Regression Modeling tab
      tabItem(tabName = "regression",
              fluidRow(
                box(selectInput("regCountry", "Select Country:", 
                                choices = unique(temp_data$Country)),
                    width = 12, status = "primary", solidHeader = TRUE, title = "Country Selection")
              ),
              fluidRow(
                box(plotlyOutput("regressionPlot"), width = 12, title = "Regression Analysis", status = "info", solidHeader = TRUE)
              ),
              fluidRow(
                box(verbatimTextOutput("regressionSummary"), width = 6, title = "Model Summary", status = "success", solidHeader = TRUE),
                box(plotlyOutput("residualPlot"), width = 6, title = "Residual Analysis", status = "success", solidHeader = TRUE)
              ),
              fluidRow(
                box(numericInput("forecastYear", "Forecast Year:", 
                                 value = 2030, min = 2023, max = 2100),
                    width = 4, status = "primary", solidHeader = TRUE, title = "Forecast Input"),
                box(actionButton("runForecast", "Run Forecast", 
                                 icon = icon("chart-line"), class = "btn-primary"),
                    width = 4, status = "primary", solidHeader = TRUE, title = "Run Forecast"),
                box(verbatimTextOutput("forecastResult"), width = 4, title = "Forecast Results", status = "success", solidHeader = TRUE)
              )
      ),
      
      # Machine Learning tab
      tabItem(tabName = "ml",
              fluidRow(
                box(selectInput("mlTarget", "Select Target:", 
                                choices = c("Temperature", "Change_from_1990")),
                    width = 4, status = "primary", solidHeader = TRUE, title = "Target Selection"),
                box(selectInput("mlPredictors", "Select Predictors:", 
                                choices = c("Continent", "Latitude", "Longitude", "Year"),
                                selected = c("Continent", "Year"),
                                multiple = TRUE),
                    width = 4, status = "primary", solidHeader = TRUE, title = "Predictor Selection"),
                box(selectInput("mlModel", "Select Model:", 
                                choices = c("Random Forest", "Decision Tree", "Linear Regression")),
                    width = 4, status = "primary", solidHeader = TRUE, title = "Model Selection")
              ),
              fluidRow(
                box(actionButton("runML", "Run Model", 
                                 icon = icon("robot"), class = "btn-primary"),
                    width = 12, status = "primary", solidHeader = TRUE, title = "Execute Model")
              ),
              fluidRow(
                box(verbatimTextOutput("mlSummary"), width = 6, title = "Model Summary", status = "info", solidHeader = TRUE),
                box(plotOutput("mlImportance"), width = 6, title = "Feature Importance", status = "info", solidHeader = TRUE)
              ),
              fluidRow(
                box(plotlyOutput("mlActualVsPredicted"), width = 6, title = "Actual vs Predicted", status = "success", solidHeader = TRUE),
                box(DT::dataTableOutput("mlResults"), width = 6, title = "Prediction Results", status = "success", solidHeader = TRUE)
              )
      )
    )
  )
)

# Server logic (unchanged)
server <- function(input, output, session) {
  
  # Reactive data
  filtered_data <- reactive({
    data <- temp_data
    if (input$continentSelect != "All") {
      data <- data %>% filter(Continent == input$continentSelect)
    }
    data
  })
  
  # Data Exploration - Country Line Chart
  output$countryPlot <- renderPlotly({
    req(input$countrySelect)
    if(input$countrySelect == "All") return()
    
    temp_data %>%
      filter(Country == input$countrySelect) %>%
      ggplot(aes(x = Year, y = Temperature)) +
      geom_line(color = "#1f77b4", size = 1) +
      geom_point(color = "#1f77b4") +
      labs(title = paste(input$countrySelect, "Temperature Trend"),
           x = "Year", y = "Temperature (°C)") +
      theme_minimal()
  })
  
  # Data Exploration - Country Bar Chart
  output$countryBarPlot <- renderPlotly({
    req(input$countrySelect)
    if(input$countrySelect == "All") return()
    
    temp_data %>%
      filter(Country == input$countrySelect) %>%
      ggplot(aes(x = factor(Year), y = Temperature, 
                 fill = Temperature,
                 text = paste("Year:", Year, "<br>Temp:", Temperature, "°C"))) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#6baed6", high = "#ef3b2c") +
      labs(title = paste(input$countrySelect, "Yearly Temperatures"),
           x = "Year", y = "Temperature (°C)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
  })
  
  # Update continent filter when country is selected
  observe({
    if(input$countrySelect != "All") {
      continent <- unique(temp_data$Continent[temp_data$Country == input$countrySelect])
      updateSelectInput(session, "continentSelect", selected = continent)
    }
  })
  
  # Dashboard value boxes
  output$avgTempBox <- renderValueBox({
    avg_temp <- mean(temp_data$Temperature, na.rm = TRUE)
    valueBox(
      round(avg_temp, 2), "Global Average Temperature (°C)", 
      icon = icon("temperature-high"),
      color = "red"
    )
  })
  
  output$maxTempBox <- renderValueBox({
    max_temp <- max(temp_data$Temperature, na.rm = TRUE)
    country <- temp_data %>% 
      filter(Temperature == max_temp) %>% 
      pull(Country) %>% 
      first()
    valueBox(
      paste(round(max_temp, 2), "°C"), 
      paste("Hottest: ", country),
      icon = icon("fire"),
      color = "orange"
    )
  })
  
  output$minTempBox <- renderValueBox({
    min_temp <- min(temp_data$Temperature, na.rm = TRUE)
    country <- temp_data %>% 
      filter(Temperature == min_temp) %>% 
      pull(Country) %>% 
      first()
    valueBox(
      paste(round(min_temp, 2), "°C"), 
      paste("Coldest: ", country),
      icon = icon("snowflake"),
      color = "blue"
    )
  })
  
  # Dashboard plots
  output$globalTrendPlot <- renderPlotly({
    ggplotly(
      ggplot(global_avg, aes(x = Year, y = Global_Avg)) +
        geom_line(color = "red", size = 1) +
        geom_point(color = "red") +
        geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
        labs(title = "Global Average Temperature Trend (1990-2024)",
             x = "Year", y = "Temperature (°C)") +
        theme_minimal()
    )
  })
  
  output$continentTrendPlot <- renderPlotly({
    continent_avg <- temp_data %>%
      group_by(Continent, Year) %>%
      summarise(Avg_Temp = mean(Temperature, na.rm = TRUE))
    
    ggplotly(
      ggplot(continent_avg, aes(x = Year, y = Avg_Temp, color = Continent)) +
        geom_line(size = 1) +
        geom_point() +
        labs(title = "Temperature Trend by Continent",
             x = "Year", y = "Temperature (°C)") +
        theme_minimal()
    )
  })
  
  output$tempMap <- renderLeaflet({
    latest_data <- temp_data %>%
      filter(Year == max(Year)) %>%
      group_by(Country, Continent) %>%
      summarise(Temperature = mean(Temperature, na.rm = TRUE))
    
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = latest_data$Temperature,
      reverse = TRUE
    )
    
    leaflet(latest_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~0, lat = ~0,
        radius = ~sqrt(abs(Temperature)) * 2,
        color = ~pal(Temperature),
        stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste(Country, "<br>Temperature:", round(Temperature, 2), "°C")
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~Temperature,
        title = "Temperature (°C)",
        opacity = 1
      )
  })
  
  # Data Exploration plots
  output$continentBoxPlot <- renderPlotly({
    data <- filtered_data()
    
    ggplotly(
      ggplot(data, aes(x = Continent, y = Temperature, fill = Continent)) +
        geom_boxplot() +
        labs(title = "Temperature Distribution by Continent",
             x = "Continent", y = "Temperature (°C)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$dataSummary <- renderPrint({
    summary(filtered_data())
  })
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Statistical Analysis outputs
  output$summaryStats <- renderPrint({
    data <- temp_data
    if (input$statContinent != "All") {
      data <- data %>% filter(Continent == input$statContinent)
    }
    data <- data %>% filter(Year == input$statYear)
    
    cat("Summary Statistics for", input$statYear, "\n")
    if (input$statContinent != "All") cat("Continent:", input$statContinent, "\n\n")
    
    summary_stats <- data %>%
      summarise(
        Mean = mean(Temperature, na.rm = TRUE),
        Median = median(Temperature, na.rm = TRUE),
        SD = sd(Temperature, na.rm = TRUE),
        Min = min(Temperature, na.rm = TRUE),
        Max = max(Temperature, na.rm = TRUE),
        IQR = IQR(Temperature, na.rm = TRUE),
        N = n()
      )
    
    print(summary_stats)
    
    cat("\n95% Confidence Interval for Mean:\n")
    t_test <- t.test(data$Temperature)
    print(t_test$conf.int)
  })
  
  output$tempDistribution <- renderPlotly({
    data <- temp_data
    if (input$statContinent != "All") {
      data <- data %>% filter(Continent == input$statContinent)
    }
    data <- data %>% filter(Year == input$statYear)
    
    ggplotly(
      ggplot(data, aes(x = Temperature)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
        geom_density(color = "red", size = 1) +
        labs(title = paste("Temperature Distribution for", input$statYear),
             x = "Temperature (°C)", y = "Density") +
        theme_minimal()
    )
  })
  
  output$confidenceInterval <- renderPlotly({
    data <- temp_data
    if (input$statContinent != "All") {
      data <- data %>% filter(Continent == input$statContinent)
    }
    
    ci_data <- data %>%
      group_by(Year) %>%
      summarise(
        Mean = mean(Temperature),
        SE = sd(Temperature)/sqrt(n()),
        CI_Lower = Mean - qt(0.975, df = n()-1) * SE,
        CI_Upper = Mean + qt(0.975, df = n()-1) * SE
      )
    
    ggplotly(
      ggplot(ci_data, aes(x = Year, y = Mean)) +
        geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "lightblue", alpha = 0.5) +
        geom_line(color = "blue", size = 1) +
        labs(title = "Mean Temperature with 95% Confidence Intervals",
             x = "Year", y = "Temperature (°C)") +
        theme_minimal()
    )
  })
  
  output$probabilityPlot <- renderPlotly({
    data <- temp_data
    if (input$statContinent != "All") {
      data <- data %>% filter(Continent == input$statContinent)
    }
    data <- data %>% filter(Year == input$statYear)
    
    mean_temp <- mean(data$Temperature)
    sd_temp <- sd(data$Temperature)
    
    x <- seq(min(data$Temperature), max(data$Temperature), length.out = 100)
    y <- dnorm(x, mean = mean_temp, sd = sd_temp)
    
    plot_data <- data.frame(x = x, y = y)
    
    ggplotly(
      ggplot(data, aes(x = Temperature)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
        geom_line(data = plot_data, aes(x = x, y = y), color = "red", size = 1) +
        labs(title = "Temperature Distribution with Normal Fit",
             x = "Temperature (°C)", y = "Density") +
        theme_minimal()
    )
  })
  
  # Regression Modeling outputs
  regression_model <- reactiveVal(NULL)
  
  output$regressionPlot <- renderPlotly({
    data <- temp_data %>% filter(Country == input$regCountry)
    
    p <- ggplot(data, aes(x = Year, y = Temperature)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Linear Temperature Trend for", input$regCountry),
           x = "Year", y = "Temperature (°C)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$regressionSummary <- renderPrint({
    data <- temp_data %>% filter(Country == input$regCountry)
    
    model <- lm(Temperature ~ Year, data = data)
    
    regression_model(model)
    summary(model)
  })
  
  output$residualPlot <- renderPlotly({
    req(regression_model())
    model <- regression_model()
    
    residuals <- resid(model)
    fitted <- fitted(model)
    
    plot_data <- data.frame(Fitted = fitted, Residuals = residuals)
    
    ggplotly(
      ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
        geom_point(color = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Fitted Values",
             x = "Fitted Values", y = "Residuals") +
        theme_minimal()
    )
  })
  
  output$forecastResult <- renderPrint({
    req(regression_model())
    req(input$forecastYear)
    
    model <- regression_model()
    new_data <- data.frame(Year = input$forecastYear)
    
    pred <- predict(model, newdata = new_data, interval = "confidence")
    
    cat("Forecast for", input$forecastYear, ":\n")
    print(pred)
  })
  
  # Machine Learning outputs
  ml_results <- reactiveVal(NULL)
  
  observeEvent(input$runML, {
    req(input$mlTarget, input$mlPredictors, input$mlModel)
    
    showModal(modalDialog("Building machine learning model...", footer = NULL))
    
    tryCatch({
      data <- temp_data
      
      if (input$mlTarget == "Change_from_1990") {
        baseline <- data %>%
          filter(Year == 1990) %>%
          select(Country, Baseline_Temp = Temperature)
        
        data <- data %>%
          left_join(baseline, by = "Country") %>%
          mutate(Change_from_1990 = Temperature - Baseline_Temp) %>%
          filter(!is.na(Change_from_1990))
      }
      
      model_data <- data %>%
        select(all_of(c(input$mlTarget, input$mlPredictors))) %>%
        na.omit()
      
      if(nrow(model_data) < 10) {
        stop("Not enough complete cases for modeling")
      }
      
      set.seed(123)
      trainIndex <- createDataPartition(model_data[[input$mlTarget]], 
                                        p = 0.7, 
                                        list = FALSE)
      trainData <- model_data[trainIndex, ]
      testData <- model_data[-trainIndex, ]
      
      formula <- as.formula(paste(input$mlTarget, "~", 
                                  paste(input$mlPredictors, collapse = " + ")))
      
      if (input$mlModel == "Random Forest") {
        model <- randomForest(formula, data = trainData, importance = TRUE)
      } else if (input$mlModel == "Decision Tree") {
        model <- rpart(formula, data = trainData, method = "anova")
      } else if (input$mlModel == "Linear Regression") {
        model <- lm(formula, data = trainData)
      }
      
      predictions <- predict(model, newdata = testData)
      
      ml_results(list(
        model = model,
        actual = testData[[input$mlTarget]],
        predicted = predictions,
        testData = testData,
        trainData = trainData
      ))
      
      removeModal()
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$mlSummary <- renderPrint({
    req(ml_results())
    model <- ml_results()$model
    
    if (inherits(model, "randomForest")) {
      print(model)
      cat("\nVariable Importance:\n")
      print(importance(model))
    } else if (inherits(model, "rpart")) {
      print(model)
      cat("\nVariable Importance:\n")
      print(model$variable.importance)
    } else if (inherits(model, "lm")) {
      summary(model)
    }
  })
  
  output$mlImportance <- renderPlot({
    req(ml_results())
    model <- ml_results()$model
    
    if (inherits(model, "randomForest")) {
      varImpPlot(model, main = "Variable Importance")
    } else if (inherits(model, "rpart")) {
      rpart.plot(model, main = "Decision Tree")
    } else if (inherits(model, "lm")) {
      coefs <- summary(model)$coefficients
      coefs <- coefs[-1, ]
      plot_data <- data.frame(
        Variable = rownames(coefs),
        Estimate = coefs[, "Estimate"],
        SE = coefs[, "Std. Error"]
      )
      
      ggplot(plot_data, aes(x = Variable, y = Estimate)) +
        geom_point() +
        geom_errorbar(aes(ymin = Estimate - 1.96*SE, ymax = Estimate + 1.96*SE), width = 0.2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        coord_flip() +
        labs(title = "Coefficient Estimates with 95% CI") +
        theme_minimal()
    }
  })
  
  output$mlActualVsPredicted <- renderPlotly({
    req(ml_results())
    results <- ml_results()
    
    plot_data <- data.frame(
      Actual = results$actual,
      Predicted = results$predicted
    )
    
    ggplotly(
      ggplot(plot_data, aes(x = Actual, y = Predicted)) +
        geom_point(color = "blue", alpha = 0.6) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Actual vs Predicted Values",
             x = "Actual", y = "Predicted") +
        theme_minimal()
    )
  })
  
  output$mlResults <- DT::renderDataTable({
    req(ml_results())
    results <- ml_results()
    
    result_df <- data.frame(
      Actual = results$actual,
      Predicted = results$predicted,
      Difference = results$actual - results$predicted
    )
    
    DT::datatable(result_df, options = list(pageLength = 5)) %>%
      formatRound(columns = c("Actual", "Predicted", "Difference"), digits = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)