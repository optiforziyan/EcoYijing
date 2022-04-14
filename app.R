#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

database <- read.csv("database.csv")
species_list_output = NULL

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("EcoYijing - A comprehensive Chinese Species Information Enquiry System (CSIES) V1.0"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose a CSV File. Note: one colume should be 'species_name' ",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            

            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Button
            downloadButton("downloadData", "Download")
            
        ),
            
        
        # Main panel for displaying outputs ----
        mainPanel(
           
            
            tableOutput("table")
            
            
        )
        
    )
)
  
    
    
    
    


# Define server logic to read selected file ----
server <- function(input, output) {
    
   
   datasetInput <- reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                species_list_output <- merge(read.csv(input$file1$datapath), database, 
                                                      by.x = "species_name", by.y = "species_name",
                                                      all.x = TRUE)
                species_list_output[order(species_list_output$No),]
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(species_list_output))
        }
        else {
            return(species_list_output)
        }
        
    })
    
   # Table of selected dataset ----
   output$table <- renderTable({
     datasetInput()
   })
      

   # Downloadable csv of selected dataset ----
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("species_list_final.csv", sep = "")
     },
     content = function(file) {
       write.csv(datasetInput(), file, row.names = FALSE)
     }
   )
}
    


# Run the application 
shinyApp(ui = ui, server = server)

