#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(magrittr)
library(dplyr)
library(bigrquery)
library(ggplot2)


library(tidyr)


ui <- fluidPage(
  titlePanel("Explore the ICU cohort data"),
  fluidRow(
    column(4,  
           # 创建一个下拉框，让用户选择要展示的 care unit
           selectInput("unit_selector", "Variable of interest:", 
                       choices = c("First Care Unit" = "first_careunit", 
                                   "Last Care Unit" = "last_careunit",
                                   "Admission Type" = "admission_type",
                                   "Admission Location" = "admission_location",
                                   "Discharge Location" = "discharge_location",
                                   "Insurance" = "insurance",
                                   "Language" = "language",
                                   "Marital Status" = "marital_status",
                                   "Race" = "race",
                                   "Gender" = "gender",
                                   "Lab events" = "lab_events",
                                   "Chart events" = "chart_events"
                       ),
                       selected = "first_careunit") # 设置默认选择
    )
  ),
  column(8,
         tabsetPanel(
           tabPanel("Summary",
                    h5('subtitle1'),
                    verbatimTextOutput("summary")
           ),
           tabPanel("Table",
                    h5('subtitle2'),
                    plotOutput("table_plot")  # 在这里放置 plotOutput
           )
         )
  )
)

server <- function(input, output) {
  
  # 使用 reactive 读取数据框
  dataset <- reactive({
    readRDS("/Users/zhangli/203b_hw_new/hw4/mimiciv_shiny/mimic_icu_cohort.rds")
  })
  
  # 创建 summary_data reactive 对象
  summary_data <- reactive({
    data <- dataset()
    if (!is.null(data)) {
      data %>%
        filter(!is.na(.data[[input$unit_selector]])) %>%
        group_by(.data[[input$unit_selector]]) %>%
        summarise(
          mean_los = mean(los, na.rm = TRUE),
          median_los = median(los, na.rm = TRUE),
          min_los = min(los, na.rm = TRUE),
          max_los = max(los, na.rm = TRUE),
          sum_los = sum(los, na.rm = TRUE),
          sd_los = sd(los, na.rm = TRUE)
        )
    }
  })
  
  # 输出 summary 数据
  output$summary <- renderPrint({
    summary_data()
  })
  
  
  
  # 响应用户选择的 care unit，并生成柱状图
  output$table_plot <- renderPlot({  # 修改输出的 ID 为 table_plot
    # 获取数据框
    data <- dataset()
    
    if (input$unit_selector == "lab_events") {
      # 选择绘制箱线图
      data %>%
        select(
          Bicarbonate, Chloride, Creatinine,
          Glucose, Potassium, Sodium,
          Hematocrit, `White Blood Cells`
        ) %>%
        pivot_longer(
          cols = everything(),
          names_to = "measurement",
          values_to = "value"
        ) %>%
        ggplot(aes(
          x = value, y = measurement
        )) +
        geom_boxplot() +
        labs(
          x = "Value", y = "Variable",
          title = "Last Available Lab Measurements before ICU stay"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_cartesian(xlim = c(0, 250))
    } else 
      if (input$unit_selector == "chart_events"){
        # 选择绘制箱线图
        data %>%
          select(
            "Heart Rate", "Non Invasive Blood Pressure systolic",
            "Non Invasive Blood Pressure diastolic",
            "Respiratory Rate",
            "Temperature Fahrenheit"
          ) %>%
          pivot_longer(
            cols = everything(),
            names_to = "measurement",
            values_to = "value"
          ) %>%
          ggplot(aes(
            x = value, y = measurement
          )) +
          geom_boxplot() +
          labs(
            x = "Value", y = "Variable",
            title = " First Vital Measurements within the ICU Stay"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_cartesian(xlim = c(0, 250))
      } else {
        # 选择绘制柱状图
        data %>%
          ggplot(aes(x = .data[[input$unit_selector]])) +
          geom_bar(stat = "count", fill = "skyblue") +
          geom_text(stat = "count", aes(label = ..count..), hjust = -0.3, size = 3) +
          theme_minimal() +
          labs(title = "Patient count by stay or patient variable group", x = "", y = "Count") +
          coord_flip()
      }
  })
}

shinyApp(ui, server)

