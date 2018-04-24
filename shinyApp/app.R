library(shiny)
library(jsonlite)
library(sqldf)
library(stringr)

df0 <- read.csv('match_compare.csv')
df1 <- sqldf("select METRIC_LABEL || '_TREATED' AS METRIC_LABEL, BEFORE_ENROLLED,AFTER_ENROLLED from df0
        UNION
        select METRIC_LABEL || '_UNTREATED' AS METRIC_LABEL, BEFORE_NOT_ENROLLED,AFTER_NOT_ENROLLED from df0
        ")
myf2 <- function(x) { sprintf('["%s",%s,%s]', x[1],x[2],x[3])}

ui <- 


fluidPage(

mainPanel(
        
       fluidRow(
        includeHTML("Capstone_DWH_Part1.html")),
    
        fluidRow(
                selectInput("Covariate","Covariate:", df0$METRIC_LABEL, multiple = FALSE, selected = NULL),
           
                       # make sure your billboard.min.css and billboard.min.js files are in the www directory
                        tags$script(src="//d3js.org/d3.v4.min.js"),
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "billboard.min.css")),
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                        tags$script(src="billboard.min.js"),
 
                        # the id here is the same as the id in the first line of the js code
                        tags$div(id="CombinationChart"),
                        tags$script(src="hideOutput.js"),

                        # load your javascript code
                        tags$script(src="my_script.js"),
                        textOutput("text")      
            
        
    
  ),
    
    fluidRow(
        includeHTML("Capstone_DWH_Part2.html")),
    
    width = 12
    )

    )

server <- function(input, output, session) {
    
    output$text <- renderText({
    query <- paste("select * from df1 where METRIC_LABEL like '%",input$Covariate,"%'",sep="")
    df2 <- sqldf(query)
    data <- apply(df2,1,myf2)
    msg_str <- paste0('[',toString(data),']')
    session$sendCustomMessage(type="get_data_from_shiny", msg_str)   
             })
    
      getPage<-function() {
      return(includeHTML("Capstone_DWH_Part1.html"))
      return(includeHTML("Capstone_DWH_Part2.html"))
  }
  output$inc<-renderUI({getPage()})
    
}

shinyApp(ui=ui, server=server)