library(shiny)
library(httr)
library(jsonlite)
library(data.table)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Bowlingtest"),
    sidebarLayout(
        sidebarPanel(
           
            actionButton(inputId = "hentKnap", label="Hent data"),
            actionButton(inputId = "validerKnap", label="Valider data"),
            
            ),
        
        mainPanel(
            uiOutput("tabel"),
            uiOutput("text")
        )
        )
)

server <- function(input, output,session) {
    
    hide("validerKnap")
    msg_txt <- reactiveVal("")
    ver <- reactiveVal()

    
    output$tabel <- renderUI({
        
        tableOutput("test")
   })
    
    output$text <- renderUI({
        
        textOutput("valid")
    })

    observeEvent( input$hentKnap, {
        show("validerKnap")
        msg_txt("")
        withProgress(message = "Henter data",{
        link <- "http://13.74.31.101/api/points"
        get <- fromJSON(txt=link) # hent data
        points <- as.data.frame(get$points) # gem data som dataframe
        
        points$presum <- points$V1+points$V2 #lav simpel sum
        
       points$sum <- ifelse(points$V1==10, shift(points$presum+10, 1L, type="lead"),
                             ifelse(points$presum==10, shift(points$V1+10, 1L, type="lead"), 
                                    points$presum)) #udregn strike og spare
       points$Callout <- ifelse(points$V1==10, "Strike!", ifelse(points$V1+points$V2==10, "Spare!", "")) #angiv hvilker runder har strike og spares
        
        points$Points <- ifelse(is.na(points$sum), points$presum,points$sum) #genindfør simpel sum på linje 1
       points <- within(points, Points <- cumsum(Points)) #beregn kumulativ sum
        points$Runde <- seq.int(nrow(points)) 
        
        body <- list(token = get$token, points=points$Points) #lav liste med token og resultater
        
        ver(POST(link, body = body, encode = "json",verbose())) #send til POST og gem output i ver

        output$valid <- renderText("")
        
        printvenlig <- points[,c("Runde","Points","Callout")]
        output$test <- renderTable(printvenlig,digits = 0)
        output$valid <- renderText({ msg_txt() })

   })
        })
    
    observeEvent( input$validerKnap, {
        verification <- fromJSON(content(ver(),type="text"))$succes #verificer at pointscoren er korrekt
        
        if(verification==TRUE){
            msg_txt("Resultat valideret i API og er korrekt")
        }else{
            msg_txt("Resultat er ikke valideret")
        }
        output$valid <- renderText({ msg_txt() })
        
    })
    
    

}
shinyApp(ui, server)



# Run the application 
shinyApp(ui = ui, server = server)
