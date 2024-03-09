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
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)

dataset <- readRDS("/Users/zhangli/203b_hw_new/hw4/mimiciv_shiny/mimic_icu_cohort.rds")

# path to the service account token
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

sid <- as.character(input$patient_id)
sid_adt <- tbl(con_bq, "transfers")
sid_lab <- tbl(con_bq, "labevents")
sid_procedure <- tbl(con_bq, "procedures_icd")
sid_patients <- tbl(con_bq, "patients")

sid_admissions <- tbl(con_bq, "admissions")

sid_diagnoses <- tbl(con_bq, "diagnoses_icd")

diagnoses_info <- tbl(con_bq, "diagnoses_icd")
diagnoses_id <- tbl(con_bq, "d_icd_diagnoses")



ui <- fluidPage(
  titlePanel("Explore the ICU cohort data"),
  fluidRow(
    column(
      4,
      selectInput("unit_selector", "Variable of interest:",
        choices = c(
          "First Care Unit" = "first_careunit",
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
        selected = "first_careunit"
      )
    )
  ),
  column(
    8,
    tabsetPanel(
      tabPanel(
        "Summary",
        h5("Length of ICU stay(los) vs variables"),
        verbatimTextOutput("summary")
      ),
      tabPanel(
        "Table",
        h5("Patient count"),
        plotOutput("table_plot")
      ),
      tabPanel(
        "Patient's ADT and ICU stay information",
        sidebarLayout(
          sidebarPanel(
            helpText("Select a patient"),
            selectizeInput("patient_id", "Patient ID", 
                           choices = NULL, options = list(maxItems = 1)),
            selectInput("type", "which kind of information",
              choices = c("ADT", "ICU stay"),
              selected = "ADT"
            ),
          ),
          mainPanel(
            plotOutput("adt_plot")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # observe({
  #   updateSelectizeInput(session, "patient_id", 
  # choices = unique(dataset$subject_id))
  # })



  summary_data <- reactive({
    data <- dataset
    if (!is.null(data)) {
      if (input$unit_selector %in% c("lab_events", "chart_events")) {
        selected_variables <- if (input$unit_selector == "lab_events") {
          c(
            "Bicarbonate", "Chloride", "Creatinine",
            "Glucose", "Potassium", "Sodium",
            "Hematocrit", "White Blood Cells"
          )
        } else if (input$unit_selector == "chart_events") {
          c(
            "Heart Rate", "Non Invasive Blood Pressure systolic",
            "Non Invasive Blood Pressure diastolic",
            "Respiratory Rate", "Temperature Fahrenheit"
          )
        }


        data %>%
          summarise_at(vars(all_of(selected_variables)), list(
            mean = ~ mean(., na.rm = TRUE),
            median = ~ median(., na.rm = TRUE),
            min = ~ min(., na.rm = TRUE),
            max = ~ max(., na.rm = TRUE),
            sd = ~ sd(., na.rm = TRUE),
            Q1 = ~ quantile(., 0.25, na.rm = TRUE),
            Q3 = ~ quantile(., 0.75, na.rm = TRUE)
          ))
      } else {
        data %>%
          filter(!is.na(.data[[input$unit_selector]])) %>%
          group_by(.data[[input$unit_selector]]) %>%
          summarise(
            mean_los = mean(los, na.rm = TRUE),
            median_los = median(los, na.rm = TRUE),
            min_los = min(los, na.rm = TRUE),
            max_los = max(los, na.rm = TRUE),
            sd_los = sd(los, na.rm = TRUE),
            Q1_los = quantile(los, 0.25, na.rm = TRUE),
            Q3_los = quantile(los, 0.75, na.rm = TRUE)
          )
      }
    }
  })


  output$summary <- renderPrint({
    summary_data() %>%
      knitr::kable()
  })


  output$table_plot <- renderPlot({
    data <- dataset

    if (input$unit_selector == "lab_events") {
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
    } else if (input$unit_selector == "chart_events") {
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
      data %>%
        ggplot(aes(x = .data[[input$unit_selector]])) +
        geom_bar(stat = "count", fill = "skyblue") +
        geom_text(
          stat = "count", aes(label = ..count..),
          hjust = -0.3, size = 3
        ) +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          x = "", y = "Count"
        ) +
        coord_flip()
    }
  })
}




adt_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  sid_adt |>
    filter(subject_id == sub_id & eventtype != "discharge") |>
    collect()
})

labevents_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  labevents_info |>
    filter(subject_id == sub_id) |>
    select(charttime) |>
    collect()
})

procedures_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  procedures_info |>
    filter(subject_id == sub_id) |>
    select(chartdate, icd_code, icd_version) |>
    left_join(procedures_id, by = c("icd_code" = "icd_code", 
                                    "icd_version" = "icd_version")) |>
    select(chartdate, long_title) |>
    collect()
})

diagnoses_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  diagnoses_info |>
    filter(subject_id == sub_id) |>
    select(seq_num, icd_code, icd_version) |>
    left_join(diagnoses_id, by = c("icd_code" = "icd_code", 
                                   "icd_version" = "icd_version")) |>
    select(long_title) |>
    collect() |>
    slice(1:3)
})

adt_title_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  plot_title <- paste("Patient", sub_id, "ICU stays Vitals")
})

chartevents_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  chartevents_info |>
    filter(subject_id == sub_id & itemid %in% c(220045, 220179, 
                                                220180, 220210, 223761)) |>
    select(stay_id, charttime, itemid, value) |>
    collect() |>
    mutate(
      itemid = case_when(
        itemid == 220045 ~ "HR",
        itemid == 220180 ~ "NBPd",
        itemid == 220179 ~ "NBPs",
        itemid == 220210 ~ "RR",
        itemid == 223761 ~ "Temperature F"
      )
    ) |>
    mutate(charttime = as.POSIXct(charttime), value = as.numeric(value))
})

plot1_title_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  patients_info <- mimic_icu_cohort[mimic_icu_cohort$subject_id == sub_id, ]
  gender <- patients_info$gender[1]
  age <- patients_info$anchor_age[1]
  race <- patients_info$race[1]
  plot_title <- paste("Patient", sub_id, ",", gder, ",", age, "years old", 
                      ",", tolower(race))
})


plot2_title_reactive <- reactive({
  sub_id <- as.integer(input$patient_id)
  plot_title <- paste("Patient", sub_id, "ICU stays Vitals")
})




# # ADT plot
# output$adt_plot <- renderPlot({
#   req(input$patient_id, input$type)
#   if (is.null(input$patient_id) || input$patient_id == "") {
#     return(NULL)
#   }
#   if (input$type == "ADT") {
#     adt <- adt_reactive()
#     labevents <- labevents_reactive()
#     procedures <- procedures_reactive()
#     diagnoses <- diagnoses_reactive()
#     plot_title <- plot1_title_reactive()
#     plot_subtitle <- paste(diagnoses$long_title[1], "\n", 
#                            diagnoses$long_title[2], "\n",
#                            diagnoses$long_title[3])
# 
#     ggplot() +
#       # admission, discharge, and transfer (ADT)
#       geom_segment(
#         data = adt,
#         mapping = aes(
#           x = intime,
#           xend = outtime,
#           y = "ADT",
#           yend = "ADT",
#           color = careunit,
#           linewidth = str_detect(careunit, "(ICU|CCU)")
#         ),
#       ) +
#       # labs
#       geom_point(
#         data = labevents |> distinct(charttime, keep_all = TRUE),
#         mapping = aes(x = charttime, y = "Lab"),
#         shape = "+",
#         size = 5
#       ) +
#       # procedures
#       geom_jitter(
#         # only keep the 1st procedure on the same day
#         data = procedures,
#         mapping = aes(
#           x = chartdate + hours(12),
#           y = "Procedure",
#           shape = str_sub(long_title, 1, 25)
#         ),
#         size = 3,
#         height = 0
#       ) +
#       labs(
#         title = plot_title,
#         subtitle = plot_subtitle,
#         x = "Calendar Time",
#         y = "",
#         color = "Care Unit",
#         shape = "Procedure"
#       ) +
#       guides(linewidth = "none") +
#       scale_y_discrete(limits = rev) +
#       theme_light() +
#       theme(legend.position = "bottom", legend.box = "vertical")
#   } else {
#     plot_title <- plot2_title_reactive()
#     chartevents <- chartevents_reactive()
#     ggplot(chartevents, aes(x = charttime, y = value, color = itemid)) +
#       geom_point() +
#       geom_line() +
#       facet_grid(itemid ~ stay_id, scales = "free") +
#       labs(
#         title = plot_title,
#         x = "",
#         y = ""
#       ) +
#       theme_light() +
#       theme(legend.position = "none")
#   }
# })


shinyApp(ui = ui, server = server)
