#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(DT)
library(RPostgreSQL)
library(DBI)



ui<- fluidPage(
  sidebarPanel(
    titlePanel("Data File Editor:"),
    wellPanel(
      #helpText("Changes to the table will be automatically saved to the source file."),
      # uncomment line below to use action button to commit changes
      #actionButton("saveBtn", "Save"),
      wellPanel(
        h4("Upload File Here:"),
        fileInput("file", label = h3("File input: CSV/XLSX",multiple = FALSE,accept = NULL,width=NULL),
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.xlsx'))),
      br(),
      br(),
      wellPanel(
        h4("Download File Here:"),
        textInput(inputId = "filename",label = "File Name:",value = "file"),
        hr(),
        downloadButton(outputId = "download",label = "Download CSV")),
        br(),
        wellPanel(
          h4("Write Table to Database:"),
          textInput(inputId = "url",label = "Database url:",placeholder = "New"),
          textInput(inputId = "user",label = "User Name:",placeholder = "New"),
          passwordInput(inputId = "password",label = "Password",placeholder = "New"),
          textInput(inputId = "database",label = "Database Name:",placeholder = "New"),
        textInput(inputId = "tablename",label = "Table Name",placeholder = "New"),
        br(),
        actionButton("savetable", "Save"))
    )),
  mainPanel(id = "null",
            tabsetPanel(
              tabPanel(title = "Data Editor:", rHandsontableOutput("data")),
              tabPanel(title = "Data Viewer:", DT::dataTableOutput("data2"))
              
            )))

# Define server logic for interactive data frame session a.k.a rhandsontable.session
server <- shinyServer(function(input, output, session) {
  
  
  ##Create dummy data frame to show when user starts app
  DF <- data.frame(Value = 1:10, Status = TRUE, Name = LETTERS[1:10],
                   Date = seq(from = Sys.Date(), by = "days", length.out = 10),
                   stringsAsFactors = FALSE)
  
  ##Create Reactive Values Expression to store data in ####################
  values <- reactiveValues()
  
  
  ##Observe and read the data into the reactiveValues expression
  
  
  ##Loads uploaded file into the app, if no file loads the dummy dataset ###################
  observe({
    
    withProgress(message = 'Loading Data',
                 value = 0, {
                   for (i in 1:2) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file
    
    if (is.null(inFile)){
      DF() }
    
    else{
      DF <- read.csv(inFile$datapath,sep = ",")
    }
    values[["DF"]] <- DF
    
  })
  
  
  ##Returns the data to be shown in the file editor ###################################################
  ##If the data is not null the uploaded data frame is returned
  ##If the data is null then a dummy dataframe is uploaded
  observe({
    if (!is.null(input$data)) {
      values[["previous"]] <- isolate(values[["DF"]])
      DF = hot_to_r(input$data)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  
  
  ##Output the data to the Render R Handsontable ##############################################################
  output$data <-  renderRHandsontable({
    
    DF <- values[["DF"]]
    
    DF<- rhandsontable(DF,width = 600, height = 600, stretchH = "all") 
    # %>% 
    # hot_validate_numeric(cols = 2, min = 5, max = 50) %>% 
    # hot_validate_numeric(cols = 3, min = 3, max = 12)
    return(DF)
    
  })
  
  
  
  ##Reactive to create uploaded data frame view #############
  
  DF2 <- reactive({
      req(input$file) ## ?req #  require that the input is available
      
      inFile <- input$file
      
      df <- read.csv(inFile$datapath,sep = ",")
      
      return(df)
  })
  
  ##Ouput the data to the dataviewer tab ##########################################################
  output$data2 <- DT::renderDataTable({
    
    DT::datatable(
      DF2(),extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
    
  })
  
  ##Create the reactive expression to save the data to ##################################
  finalDF <- reactive({isolate(values[["DF"]])})
  
  
  ##Button to Download the data #########################################################
  output$download <- downloadHandler(
    filename = function() { paste0(input$filename,".csv") },
    content = function(file) {
      write.csv(finalDF(), file)
      
    })
  
  ##Write the data to a database table ###################################################
  
  observeEvent(input$savetable,{
    
    pg = dbDriver("PostgreSQL")
    
    # Local Postgres.app database; no password by default
    # Of course, you fill in your own database information here.
    con = dbConnect(pg, user= input$user, password=input$password,
                    host=input$url, port=5432, dbname=input$database)
    dbWriteTable(conn = con,name = input$tablename ,value = finalDF() )
    
    })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)

