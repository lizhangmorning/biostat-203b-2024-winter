
install.packages("remotes")

# Install using the remotes package
remotes::install_github("rstudio/shinyuieditor")

library(shiny)
library(shinyuieditor)

# shinyuieditor::launch_editor(app_loc = "existing-app/")

# 读取数据集
data <- readRDS("mimiciv_shiny/mimic_icu_cohort.rds")

# 提取变量名
variable_names <- names(data)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("ICU Cohort Data"),
  
  # Sidebar layout with input and output definitions ----
  
  fluidRow(
    column(4,
           tabsetPanel(
             tabPanel("Caption",
                      # Input: Text for providing a caption
                      textInput(inputId = "caption",
                                label = "Patient ID",
                                value = "Data Summary")
             ),
             tabPanel("Variable",
                      # Input: Selector for choosing variable
                      selectInput(inputId = "variable",
                                  label = "Choose a variable:",
                                  choices = variable_names)
             )
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
                      tableOutput("view")
             )
           )
    )
  )
)



# Define server logic to summarize and view selected dataset
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
