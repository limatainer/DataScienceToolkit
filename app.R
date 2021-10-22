#CREATED WITH NO SERVER SIDE RENDERING
#TO MAKE THE PAGE TO LOAD FASTER I NEED TO CREATE A VERSION TO RENDER THE CONTENT ON THE SERVER SIDE
#https://rstudio.github.io/shinydashboard/structure.html <-- documentation to follow

library(shinydashboard)
library(googlesheets)
library(googlesheets4)

#Data for the dynamic output table at 'Overview'
top5_data <- read.csv("samurais.csv")

ui <- dashboardPage(
    skin = "black",
    #HEADER------------------------------------------
    dashboardHeader(title = span("CI&Solutioning",style = "color: #d50d16;"),
                    tags$li(a(href = 'https://www.sitel.com/',
                              tags$img(src = 'sitelLogo.png',
                                  title = "Sitel", height = "50px"),
                              style = "padding-top:30px; padding-bottom:30px;"),
                            class = "dropdown")),
    
    #MENU SIDEBAR------------------------------------
    dashboardSidebar(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        sidebarMenu(#6 Tab Contents
            menuItem("Overview", tabName = "overview", icon = icon("columns")),
            menuItem("VSM",tabName = "vsm", icon = icon("chart-line"),
                     badgeLabel = "Lean", badgeColor = "green"),
            menuItem("RCA Diagrams", tabName = "diagrams", icon = icon("columns"),
                     badgeLabel = "Lean", badgeColor = "green"),
            menuItem("Dataset cleaner", tabName = "clean", icon = icon("th")),
            menuItem("Tools", tabName = "links", icon = icon("file-code-o")),
            sidebarMenuOutput("maxconnect")
        )
    ),
    
    #BODY--------------------------------------------
    dashboardBody(
        tags$body(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # First tab content***********
            tabItem(tabName = "overview",
                    fluidRow(
                        # A static valueBox
                        valueBox(10 * 2, "New Projects", icon = icon("tree")),
                        
                        # Dynamic valueBoxes
                        valueBoxOutput("progressBox"),
                        
                        valueBoxOutput("approvalBox"),
                        br(),
                        
                        #CSV Table Dynamic output
                        box(title = "Performance"
                            , status = "success", solidHeader = T
                            , collapsible = T, width = 12
                            , column(12, align="center", tableOutput('top5')))
                    ),
            ),
            
            # Second tab content*********
            tabItem(tabName = "vsm",
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls",
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        )
                    ),
            ),
            # Third tab content**********
            tabItem(tabName = "diagrams",
                    align='center',
                    h1(
                        "5 why analysis", 
                        style = "font-size: 50px; color:maroon"
                    ),
                    br(),
                    fluidRow(
                        column(
                            align="center", width = 10, 
                            box(
                                title = "Problem:", width = 12, solidHeader = TRUE, status = "primary",
                                textInput("text", label = "",placeholder = "The Problem is..."),
                                textOutput("value")
                            ),
                            box(
                                title = "WHY:", width = 12, status = "warning",
                                "Box content"
                            ),
                            box(
                                title = "WHY:", width = 12, status = "warning",
                                "Box content"
                            ),
                            box(
                                title = "WHY:", width = 12, status = "warning",
                                "Box content"
                            ),
                            box(
                                title = "Root Cause:", width = 12, solidHeader = TRUE, status = "success",
                                "Box content"
                            )
                        )
                    )
            ),
            # Fourth tab content*********
            tabItem(tabName = "clean",
                    fluidRow(
                        box(title = "Histogram", status = "primary",solidHeader = TRUE,collapsible = TRUE, plotOutput("plot2", height = 250)),
                        
                        box(
                            title = "Inputs", status = "warning", solidHeader = TRUE,
                            "Box content here", br(), "More box content",
                            sliderInput("slides", "Slider input:", 1, 100, 50),
                            textInput("texto", "Text input:"),
                            br(),
                            textOutput("boxout")
                        )
                    ),
                    #Google Sheet
                    fluidPage(
                        titlePanel("Embedded Google Sheet"),
                        htmlOutput("googleSheet"))
                    
            ),
            # Fifith tab content**********
            tabItem(tabName = "links",
                    align='center',
                    h1(
                        "Online Converter", 
                        style = "font-size: 70px"
                    ),
                    br(),
                    a(href = "https://csvjson.com/csv2json", "Convert CSV -> JSON", 
                      style = "font-size: 30px"),
                    br(),
                    br(),
                    a(href = "https://anyconv.com/xlsx-to-csv-converter/", "XLSX to CSV", 
                      style = "font-size: 30px"),
                    br(),
                    br(),
                    h1(
                        "AutoDraw", 
                        style = "font-size: 70px"
                    ),
                    br(),
                    br(),
                    a(href = "https://www.autodraw.com/", "Drawning Toll", 
                      style = "font-size: 30px")
            )
        )
    )
)

server <- function(input, output) {
    
    # google sheet
    googleSheet_embed_link <- "https://docs.google.com/spreadsheets/d/19XOABGNbWhahs_uJXF9Sh47zrxXO5CEqxDj3TiGk9HM/edit?usp=sharing"
    
    output$googleSheet <- renderUI({
        tags$iframe(id = "googleSheet",
                    src = googleSheet_embed_link,
                    width = 1024,
                    height = 768,
                    frameborder = 0,
                    marginheight = 0)
    })
    # First tab content***********
    output$progressBox <- renderValueBox({
        valueBox(
            "45%", "Projects Progress", icon = icon("list"),
            color = "purple"
        )
    })
    
    output$approvalBox <- renderValueBox({
        valueBox(
            "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$top5 = renderTable({
        top5_data
    })

    # Second tab content*********
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    #Third tab content***********
    output$value <- renderPrint({ input$text })
    
    # Fourth tab content*********
    output$plot2 <- renderPlot({
        data <- histdata[seq_len(input$slides)]
        hist(data)
    })
    output$boxout <- renderPrint({input$texto})
    
    #6 sidebarMenu
    output$maxconnect <- renderMenu({
        sidebarMenu(
            menuItem(a(href = "https://maxconnect.sitel.com/maxconnect/Account/Login?ReturnUrl=%2Fmaxconnect%2F", "Max Connect"))
        )
    })
}

shinyApp(ui, server)