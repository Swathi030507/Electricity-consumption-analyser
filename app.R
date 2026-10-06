# ============================================================
# ELECTRICITY CONSUMPTION ANALYZER
# COMPLETE R SHINY PROJECT
# ============================================================

# ============================================================
# 1. INSTALL REQUIRED PACKAGES
# ============================================================

packages <- c("shiny", "ggplot2", "dplyr", "DT")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

# ============================================================
# 2. LOAD PACKAGES
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# ============================================================
# 3. CREATE ELECTRICITY DATA
# ============================================================

set.seed(1234)

households <- paste0("House-", sprintf("%02d", 1:12))

regions <- c(
  "North",
  "South",
  "East",
  "West"
)

months <- seq(
  as.Date("2025-01-01"),
  as.Date("2026-09-01"),
  by = "month"
)

# Create all household-month combinations
electricity_data <- expand.grid(
  Household = households,
  Month = months,
  stringsAsFactors = FALSE
)

# Assign region
region_map <- data.frame(
  Household = households,
  Region = rep(
    regions,
    length.out = length(households)
  ),
  stringsAsFactors = FALSE
)

electricity_data <- electricity_data %>%
  left_join(region_map, by = "Household")

# Household usage factor
usage_factor <- c(
  0.85, 1.00, 1.15, 0.92,
  1.20, 0.88, 1.08, 1.25,
  0.80, 1.12, 0.97, 1.18
)

names(usage_factor) <- households

# Month number
electricity_data$Month_Number <- as.numeric(
  format(electricity_data$Month, "%m")
)

# Seasonal factor
electricity_data$Season_Factor <- ifelse(
  electricity_data$Month_Number %in% c(4, 5, 6),
  1.30,
  ifelse(
    electricity_data$Month_Number %in% c(7, 8, 9),
    1.12,
    ifelse(
      electricity_data$Month_Number %in% c(10, 11, 12),
      0.95,
      0.88
    )
  )
)

# Electricity consumption in kWh
electricity_data$Units <- round(
  600 *
    usage_factor[electricity_data$Household] *
    electricity_data$Season_Factor *
    runif(
      nrow(electricity_data),
      0.85,
      1.15
    ),
  0
)

electricity_data$Units <- pmax(
  electricity_data$Units,
  200
)

# Electricity tariff
electricity_data$Rate <- round(
  runif(
    nrow(electricity_data),
    6.0,
    8.5
  ),
  2
)

# Fixed charge
electricity_data$Fixed_Charge <- 150

# Bill
electricity_data$Bill <- round(
  electricity_data$Units *
    electricity_data$Rate +
    electricity_data$Fixed_Charge,
  2
)

# CO2 estimation
electricity_data$CO2_kg <- round(
  electricity_data$Units * 0.82,
  2
)

# Year
electricity_data$Year <- format(
  electricity_data$Month,
  "%Y"
)

# Month name
electricity_data$Month_Name <- format(
  electricity_data$Month,
  "%b %Y"
)

# Energy category
electricity_data$Energy_Category <- case_when(
  electricity_data$Units < 500 ~ "Low",
  electricity_data$Units < 800 ~ "Medium",
  TRUE ~ "High"
)

# ============================================================
# 4. APPLIANCE DATA
# ============================================================

appliances <- c(
  "Air Conditioner",
  "Refrigerator",
  "Washing Machine",
  "Television",
  "Water Heater",
  "Lights",
  "Fans",
  "Other Appliances"
)

appliance_share <- c(
  0.30,
  0.15,
  0.10,
  0.08,
  0.14,
  0.07,
  0.10,
  0.06
)

appliance_data <- expand.grid(
  Household = households,
  Month = months,
  Appliance = appliances,
  stringsAsFactors = FALSE
)

appliance_data <- appliance_data %>%
  left_join(
    electricity_data %>%
      select(
        Household,
        Month,
        Units
      ),
    by = c("Household", "Month")
  )

appliance_data$Share <- appliance_share[
  match(
    appliance_data$Appliance,
    appliances
  )
]

appliance_data$Appliance_Units <- round(
  appliance_data$Units *
    appliance_data$Share *
    runif(
      nrow(appliance_data),
      0.90,
      1.10
    ),
  1
)

# ============================================================
# 5. USER INTERFACE
# ============================================================

ui <- navbarPage(
  
  title = "⚡ Electricity Analyzer",
  
  id = "tabs",
  
  header = tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f4f7fb;
        font-family: Arial, sans-serif;
      }

      .navbar {
        background-color: #123b56;
        border: none;
      }

      .navbar-brand {
        color: white !important;
        font-weight: bold;
      }

      .navbar-nav > li > a {
        color: white !important;
        font-weight: bold;
      }

      .navbar-nav > .active > a {
        background-color: #285c78 !important;
      }

      .well {
        background-color: white;
        border-radius: 12px;
        border: none;
        box-shadow: 0px 3px 12px rgba(0,0,0,0.08);
      }

      .kpi {
        background-color: white;
        border-radius: 12px;
        padding: 22px;
        text-align: center;
        box-shadow: 0px 3px 12px rgba(0,0,0,0.10);
        margin-bottom: 15px;
      }

      .kpi h2 {
        margin-top: 5px;
        font-weight: bold;
      }

      .kpi p {
        color: #666;
        margin-bottom: 0;
      }

      .page-title {
        font-weight: bold;
        margin-bottom: 25px;
      }

      .recommendation {
        background-color: white;
        padding: 20px;
        margin-bottom: 15px;
        border-radius: 10px;
        border-left: 5px solid #285c78;
        box-shadow: 0px 3px 10px rgba(0,0,0,0.08);
      }

      .footer {
        text-align: center;
        padding: 25px;
        color: #777;
      }

    "))
  ),
  
  # ==========================================================
  # HOME
  # ==========================================================
  
  tabPanel(
    "🏠 Home",
    
    br(),
    
    fluidRow(
      
      column(
        12,
        
        div(
          class = "well",
          
          h1(
            "⚡ Electricity Consumption Analyzer",
            style = "text-align:center;"
          ),
          
          h3(
            "Smart Household Energy Monitoring System",
            style = "text-align:center;"
          ),
          
          br(),
          
          p(
            "This project analyzes electricity consumption,
             electricity bills, appliance usage, household
             performance and energy-saving opportunities.",
            style = "text-align:center; font-size:17px;"
          )
        )
      )
    ),
    
    br(),
    
    fluidRow(
      
      column(
        3,
        div(
          class = "kpi",
          h2(textOutput("home_units")),
          p("Total Consumption")
        )
      ),
      
      column(
        3,
        div(
          class = "kpi",
          h2(textOutput("home_bill")),
          p("Total Electricity Bill")
        )
      ),
      
      column(
        3,
        div(
          class = "kpi",
          h2(textOutput("home_houses")),
          p("Households")
        )
      ),
      
      column(
        3,
        div(
          class = "kpi",
          h2(textOutput("home_average")),
          p("Average Monthly Usage")
        )
      )
    ),
    
    br(),
    
    div(
      class = "footer",
      "Electricity Consumption Analyzer | R Shiny Project"
    )
  ),
  
  # ==========================================================
  # DASHBOARD
  # ==========================================================
  
  tabPanel(
    "📊 Dashboard",
    
    br(),
    
    fluidRow(
      
      column(
        4,
        
        selectInput(
          "dash_house",
          "Household:",
          choices = c(
            "All",
            households
          )
        )
      ),
      
      column(
        4,
        
        selectInput(
          "dash_region",
          "Region:",
          choices = c(
            "All",
            regions
          )
        )
      ),
      
      column(
        4,
        
        selectInput(
          "dash_year",
          "Year:",
          choices = c(
            "All",
            sort(
              unique(
                electricity_data$Year
              )
            )
          )
        )
      )
    ),
    
    br(),
    
    fluidRow(
      
      column(
        3,
        
        div(
          class = "kpi",
          h2(textOutput("dash_units")),
          p("Total Units")
        )
      ),
      
      column(
        3,
        
        div(
          class = "kpi",
          h2(textOutput("dash_bill")),
          p("Total Bill")
        )
      ),
      
      column(
        3,
        
        div(
          class = "kpi",
          h2(textOutput("dash_average")),
          p("Average Units")
        )
      ),
      
      column(
        3,
        
        div(
          class = "kpi",
          h2(textOutput("dash_co2")),
          p("CO2 Emission")
        )
      )
    ),
    
    br(),
    
    fluidRow(
      
      column(
        6,
        
        div(
          class = "well",
          
          h3("Monthly Consumption"),
          
          plotOutput(
            "monthly_units",
            height = "350px"
          )
        )
      ),
      
      column(
        6,
        
        div(
          class = "well",
          
          h3("Monthly Bill"),
          
          plotOutput(
            "monthly_bill",
            height = "350px"
          )
        )
      )
    ),
    
    br(),
    
    div(
      class = "well",
      
      h3("Household Comparison"),
      
      plotOutput(
        "household_chart",
        height = "400px"
      )
    )
  ),
  
  # ==========================================================
  # CONSUMPTION
  # ==========================================================
  
  tabPanel(
    "⚡ Consumption",
    
    br(),
    
    h2(
      "Consumption Analysis",
      class = "page-title"
    ),
    
    fluidRow(
      
      column(
        4,
        
        selectInput(
          "cons_house",
          "Household:",
          choices = c(
            "All",
            households
          )
        )
      ),
      
      column(
        4,
        
        selectInput(
          "cons_region",
          "Region:",
          choices = c(
            "All",
            regions
          )
        )
      ),
      
      column(
        4,
        
        selectInput(
          "cons_year",
          "Year:",
          choices = c(
            "All",
            sort(
              unique(
                electricity_data$Year
              )
            )
          )
        )
      )
    ),
    
    br(),
    
    div(
      class = "well",
      
      h3("Consumption Trend"),
      
      plotOutput(
        "consumption_chart",
        height = "450px"
      )
    ),
    
    br(),
    
    div(
      class = "well",
      
      h3("Consumption Records"),
      
      DTOutput(
        "consumption_table"
      )
    )
  ),
  
  # ==========================================================
  # APPLIANCE
  # ==========================================================
  
  tabPanel(
    "🔌 Appliances",
    
    br(),
    
    h2(
      "Appliance Analysis",
      class = "page-title"
    ),
    
    fluidRow(
      
      column(
        6,
        
        selectInput(
          "app_house",
          "Household:",
          choices = c(
            "All",
            households
          )
        )
      ),
      
      column(
        6,
        
        selectInput(
          "app_year",
          "Year:",
          choices = c(
            "All",
            sort(
              unique(
                electricity_data$Year
              )
            )
          )
        )
      )
    ),
    
    br(),
    
    fluidRow(
      
      column(
        6,
        
        div(
          class = "well",
          
          h3("Appliance Consumption"),
          
          plotOutput(
            "appliance_chart",
            height = "450px"
          )
        )
      ),
      
      column(
        6,
        
        div(
          class = "well",
          
          h3("Appliance Summary"),
          
          DTOutput(
            "appliance_table"
          )
        )
      )
    )
  ),
  
  # ==========================================================
  # BILL ANALYSIS
  # ==========================================================
  
  tabPanel(
    "💰 Bill Analysis",
    
    br(),
    
    h2(
      "Electricity Bill Analysis",
      class = "page-title"
    ),
    
    fluidRow(
      
      column(
        6,
        
        div(
          class = "well",
          
          h3("Bill by Household"),
          
          plotOutput(
            "bill_household",
            height = "400px"
          )
        )
      ),
      
      column(
        6,
        
        div(
          class = "well",
          
          h3("Average Bill by Region"),
          
          plotOutput(
            "bill_region",
            height = "400px"
          )
        )
      )
    ),
    
    br(),
    
    div(
      class = "well",
      
      h3("Monthly Bill Trend"),
      
      plotOutput(
        "bill_trend",
        height = "400px"
      )
    )
  ),
  
  # ==========================================================
  # PEAK USAGE
  # ==========================================================
  
  tabPanel(
    "📈 Peak Usage",
    
    br(),
    
    h2(
      "Peak Usage Analysis",
      class = "page-title"
    ),
    
    fluidRow(
      
      column(
        4,
        
        div(
          class = "kpi",
          
          h3(
            textOutput(
              "peak_units"
            )
          ),
          
          p(
            "Highest Monthly Consumption"
          )
        )
      ),
      
      column(
        4,
        
        div(
          class = "kpi",
          
          h3(
            textOutput(
              "peak_month"
            )
          ),
          
          p(
            "Highest Usage Month"
          )
        )
      ),
      
      column(
        4,
        
        div(
          class = "kpi",
          
          h3(
            textOutput(
              "peak_bill"
            )
          ),
          
          p(
            "Highest Bill"
          )
        )
      )
    ),
    
    br(),
    
    div(
      class = "well",
      
      h3("Peak Monthly Consumption"),
      
      plotOutput(
        "peak_chart",
        height = "450px"
      )
    )
  ),
  
  # ==========================================================
  # RECOMMENDATIONS
  # ==========================================================
  
  tabPanel(
    "💡 Recommendations",
    
    br(),
    
    h2(
      "Energy Saving Recommendations",
      class = "page-title"
    ),
    
    div(
      class = "recommendation",
      
      h3("❄️ Air Conditioner"),
      
      p(
        "Set the AC temperature around 24–26°C and
         clean the filter regularly to reduce energy use."
      )
    ),
    
    div(
      class = "recommendation",
      
      h3("💡 Lighting"),
      
      p(
        "Use LED bulbs and switch off lights when
         rooms are not being used."
      )
    ),
    
    div(
      class = "recommendation",
      
      h3("🔥 Water Heater"),
      
      p(
        "Avoid unnecessary water-heater usage and
         switch it off after use."
      )
    ),
    
    div(
      class = "recommendation",
      
      h3("🧊 Refrigerator"),
      
      p(
        "Keep the refrigerator properly maintained
         and avoid opening it frequently."
      )
    ),
    
    div(
      class = "recommendation",
      
      h3("🔌 Standby Appliances"),
      
      p(
        "Switch appliances off at the socket when
         they are not being used."
      )
    ),
    
    br(),
    
    div(
      class = "well",
      
      h3("Personalized Analysis"),
      
      textOutput(
        "personal_tip"
      )
    )
  ),
  
  # ==========================================================
  # DATA
  # ==========================================================
  
  tabPanel(
    "📋 Data",
    
    br(),
    
    h2(
      "Electricity Data",
      class = "page-title"
    ),
    
    fluidRow(
      
      column(
        6,
        
        textInput(
          "search_data",
          "Search Household / Region:",
          placeholder = "Type here..."
        )
      ),
      
      column(
        6,
        
        selectInput(
          "energy_filter",
          "Energy Category:",
          choices = c(
            "All",
            "Low",
            "Medium",
            "High"
          )
        )
      )
    ),
    
    br(),
    
    DTOutput(
      "data_table"
    ),
    
    br(),
    
    downloadButton(
      "download_csv",
      "⬇ Download CSV"
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  # ==========================================================
  # HOME
  # ==========================================================
  
  output$home_units <- renderText({
    
    paste0(
      format(
        round(
          sum(
            electricity_data$Units
          )
        ),
        big.mark = ","
      ),
      " kWh"
    )
  })
  
  output$home_bill <- renderText({
    
    paste0(
      "₹",
      format(
        round(
          sum(
            electricity_data$Bill
          )
        ),
        big.mark = ","
      )
    )
  })
  
  output$home_houses <- renderText({
    
    length(
      unique(
        electricity_data$Household
      )
    )
  })
  
  output$home_average <- renderText({
    
    paste0(
      round(
        mean(
          electricity_data$Units
        )
      ),
      " kWh"
    )
  })
  
  # ==========================================================
  # DASHBOARD FILTER
  # ==========================================================
  
  dashboard_filtered <- reactive({
    
    data <- electricity_data
    
    if (
      input$dash_house != "All"
    ) {
      
      data <- data %>%
        filter(
          Household ==
            input$dash_house
        )
    }
    
    if (
      input$dash_region != "All"
    ) {
      
      data <- data %>%
        filter(
          Region ==
            input$dash_region
        )
    }
    
    if (
      input$dash_year != "All"
    ) {
      
      data <- data %>%
        filter(
          Year ==
            input$dash_year
        )
    }
    
    data
  })
  
  # ==========================================================
  # DASHBOARD KPIs
  # ==========================================================
  
  output$dash_units <- renderText({
    
    paste0(
      format(
        round(
          sum(
            dashboard_filtered()$Units
          )
        ),
        big.mark = ","
      ),
      " kWh"
    )
  })
  
  output$dash_bill <- renderText({
    
    paste0(
      "₹",
      format(
        round(
          sum(
            dashboard_filtered()$Bill
          )
        ),
        big.mark = ","
      )
    )
  })
  
  output$dash_average <- renderText({
    
    round(
      mean(
        dashboard_filtered()$Units
      ),
      1
    )
  })
  
  output$dash_co2 <- renderText({
    
    paste0(
      format(
        round(
          sum(
            dashboard_filtered()$CO2_kg
          )
        ),
        big.mark = ","
      ),
      " kg"
    )
  })
  
  # ==========================================================
  # MONTHLY UNITS
  # ==========================================================
  
  output$monthly_units <- renderPlot({
    
    data <- dashboard_filtered() %>%
      
      group_by(
        Month
      ) %>%
      
      summarise(
        Units =
          sum(Units),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = Month,
        y = Units
      )
    ) +
      
      geom_line(
        linewidth = 1
      ) +
      
      geom_point(
        size = 3
      ) +
      
      labs(
        x = "Month",
        y = "Consumption (kWh)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
  })
  
  # ==========================================================
  # MONTHLY BILL
  # ==========================================================
  
  output$monthly_bill <- renderPlot({
    
    data <- dashboard_filtered() %>%
      
      group_by(
        Month
      ) %>%
      
      summarise(
        Bill =
          sum(Bill),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = Month,
        y = Bill
      )
    ) +
      
      geom_col() +
      
      labs(
        x = "Month",
        y = "Bill (₹)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
  })
  
  # ==========================================================
  # HOUSEHOLD COMPARISON
  # ==========================================================
  
  output$household_chart <- renderPlot({
    
    data <- dashboard_filtered() %>%
      
      group_by(
        Household
      ) %>%
      
      summarise(
        Units =
          sum(Units),
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(Units)
      )
    
    ggplot(
      data,
      aes(
        x = reorder(
          Household,
          Units
        ),
        y = Units
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "Household",
        y = "Total Consumption (kWh)"
      ) +
      
      theme_minimal()
  })
  
  # ==========================================================
  # CONSUMPTION FILTER
  # ==========================================================
  
  consumption_filtered <- reactive({
    
    data <- electricity_data
    
    if (
      input$cons_house != "All"
    ) {
      
      data <- data %>%
        filter(
          Household ==
            input$cons_house
        )
    }
    
    if (
      input$cons_region != "All"
    ) {
      
      data <- data %>%
        filter(
          Region ==
            input$cons_region
        )
    }
    
    if (
      input$cons_year != "All"
    ) {
      
      data <- data %>%
        filter(
          Year ==
            input$cons_year
        )
    }
    
    data
  })
  
  # ==========================================================
  # CONSUMPTION CHART
  # ==========================================================
  
  output$consumption_chart <- renderPlot({
    
    data <- consumption_filtered() %>%
      
      group_by(
        Month
      ) %>%
      
      summarise(
        Units =
          sum(Units),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = Month,
        y = Units
      )
    ) +
      
      geom_line(
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 3
      ) +
      
      labs(
        x = "Month",
        y = "Consumption (kWh)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
  })
  
  # ==========================================================
  # CONSUMPTION TABLE
  # ==========================================================
  
  output$consumption_table <- renderDT({
    
    datatable(
      consumption_filtered(),
      rownames = FALSE,
      
      options = list(
        pageLength = 12,
        scrollX = TRUE
      )
    )
  })
  
  # ==========================================================
  # APPLIANCE FILTER
  # ==========================================================
  
  appliance_filtered <- reactive({
    
    data <- appliance_data
    
    if (
      input$app_house != "All"
    ) {
      
      data <- data %>%
        filter(
          Household ==
            input$app_house
        )
    }
    
    if (
      input$app_year != "All"
    ) {
      
      data <- data %>%
        filter(
          format(
            Month,
            "%Y"
          ) ==
            input$app_year
        )
    }
    
    data
  })
  
  # ==========================================================
  # APPLIANCE CHART
  # ==========================================================
  
  output$appliance_chart <- renderPlot({
    
    data <- appliance_filtered() %>%
      
      group_by(
        Appliance
      ) %>%
      
      summarise(
        Units =
          sum(
            Appliance_Units
          ),
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(Units)
      )
    
    ggplot(
      data,
      aes(
        x = reorder(
          Appliance,
          Units
        ),
        y = Units
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "Appliance",
        y = "Consumption (kWh)"
      ) +
      
      theme_minimal()
  })
  
  # ==========================================================
  # APPLIANCE TABLE
  # ==========================================================
  
  output$appliance_table <- renderDT({
    
    data <- appliance_filtered() %>%
      
      group_by(
        Appliance
      ) %>%
      
      summarise(
        
        Total_Units =
          round(
            sum(
              Appliance_Units
            ),
            1
          ),
        
        Average_Units =
          round(
            mean(
              Appliance_Units
            ),
            1
          ),
        
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(Total_Units)
      )
    
    datatable(
      data,
      rownames = FALSE
    )
  })
  
  # ==========================================================
  # BILL BY HOUSEHOLD
  # ==========================================================
  
  output$bill_household <- renderPlot({
    
    data <- electricity_data %>%
      
      group_by(
        Household
      ) %>%
      
      summarise(
        Bill =
          sum(Bill),
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(Bill)
      )
    
    ggplot(
      data,
      aes(
        x = reorder(
          Household,
          Bill
        ),
        y = Bill
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "Household",
        y = "Total Bill (₹)"
      ) +
      
      theme_minimal()
  })
  
  # ==========================================================
  # BILL BY REGION
  # ==========================================================
  
  output$bill_region <- renderPlot({
    
    data <- electricity_data %>%
      
      group_by(
        Region
      ) %>%
      
      summarise(
        Average_Bill =
          mean(Bill),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = Region,
        y = Average_Bill
      )
    ) +
      
      geom_col() +
      
      labs(
        x = "Region",
        y = "Average Bill (₹)"
      ) +
      
      theme_minimal()
  })
  
  # ==========================================================
  # BILL TREND
  # ==========================================================
  
  output$bill_trend <- renderPlot({
    
    data <- electricity_data %>%
      
      group_by(
        Month
      ) %>%
      
      summarise(
        Bill =
          sum(Bill),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = Month,
        y = Bill
      )
    ) +
      
      geom_line(
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 3
      ) +
      
      labs(
        x = "Month",
        y = "Bill (₹)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
  })
  
  # ==========================================================
  # PEAK DATA
  # ==========================================================
  
  monthly_summary <- reactive({
    
    electricity_data %>%
      
      group_by(
        Month
      ) %>%
      
      summarise(
        Units =
          sum(Units),
        
        Bill =
          sum(Bill),
        
        .groups = "drop"
      )
  })
  
  # ==========================================================
  # PEAK KPIs
  # ==========================================================
  
  output$peak_units <- renderText({
    
    max(
      monthly_summary()$Units
    ) %>%
      round() %>%
      paste0(
        " kWh"
      )
  })
  
  output$peak_month <- renderText({
    
    data <- monthly_summary()
    
    data$Month[
      which.max(
        data$Units
      )
    ] %>%
      format(
        "%B %Y"
      )
  })
  
  output$peak_bill <- renderText({
    
    paste0(
      "₹",
      format(
        round(
          max(
            electricity_data$Bill
          )
        ),
        big.mark = ","
      )
    )
  })
  
  # ==========================================================
  # PEAK CHART
  # ==========================================================
  
  output$peak_chart <- renderPlot({
    
    data <- monthly_summary()
    
    ggplot(
      data,
      aes(
        x = Month,
        y = Units
      )
    ) +
      
      geom_col() +
      
      labs(
        x = "Month",
        y = "Consumption (kWh)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
  })
  
  # ==========================================================
  # PERSONALIZED TIP
  # ==========================================================
  
  output$personal_tip <- renderText({
    
    average_usage <-
      mean(
        electricity_data$Units
      )
    
    highest_appliance <-
      appliance_data %>%
      
      group_by(
        Appliance
      ) %>%
      
      summarise(
        Units =
          sum(
            Appliance_Units
          ),
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(Units)
      ) %>%
      
      slice(1)
    
    if (
      average_usage > 750
    ) {
      
      paste(
        "Average electricity usage is high at",
        round(average_usage),
        "kWh.",
        "The highest energy-consuming appliance is",
        highest_appliance$Appliance,
        ".",
        "Reducing usage of this appliance can help lower",
        "electricity consumption and monthly bills."
      )
      
    } else {
      
      paste(
        "Average electricity usage is",
        round(average_usage),
        "kWh.",
        "The highest energy-consuming appliance is",
        highest_appliance$Appliance,
        ".",
        "Continue using energy-efficient appliances and",
        "avoid unnecessary electricity usage."
      )
    }
  })
  
  # ==========================================================
  # DATA SEARCH AND FILTER
  # ==========================================================
  
  filtered_full_data <- reactive({
    
    data <- electricity_data
    
    search_value <-
      tolower(
        trimws(
          input$search_data
        )
      )
    
    if (
      search_value != ""
    ) {
      
      data <- data[
        grepl(
          search_value,
          
          tolower(
            paste(
              data$Household,
              data$Region,
              data$Year,
              data$Month_Name
            )
          )
        ),
      ]
    }
    
    if (
      input$energy_filter != "All"
    ) {
      
      data <- data %>%
        filter(
          Energy_Category ==
            input$energy_filter
        )
    }
    
    data
  })
  
  # ==========================================================
  # DATA TABLE
  # ==========================================================
  
  output$data_table <- renderDT({
    
    datatable(
      filtered_full_data(),
      rownames = FALSE,
      
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  # ==========================================================
  # DOWNLOAD CSV
  # ==========================================================
  
  output$download_csv <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "electricity_consumption_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write.csv(
        electricity_data,
        file,
        row.names = FALSE
      )
    }
  )
}

# ============================================================
# CREATE AND RETURN SHINY APP OBJECT
# ============================================================

app <- shinyApp(
  ui = ui,
  server = server
)

app