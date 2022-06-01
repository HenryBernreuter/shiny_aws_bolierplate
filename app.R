library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(dplyr)
library(data.table)
library(RMySQL)
library(odbc)
library(rsconnect)






# host <- "database-biomarin.cse6sl9rluei.us-east-2.rds.amazonaws.com"
# port <- 3306
# dbname <- "biomarin"
# user <- "admin"
# password <- "Biomarin22"
# 
# con <- DBI::dbConnect(
#     RMySQL::MySQL(),
#     host = host , port = port , dbname = dbname,
#     user = user, password = password
# )

# dbCreateTable(con,"iris", iris)
# 
#dbWriteTable(con,"iris",iris, row.names = FALSE, overwrite = TRUE)

# dbRemoveTable(con,"iris")
# # 
# dbListFields(con, "iris")
# # 
# dbListObjects(con)

#metadata <- as.data.table(dbGetInfo(con))
my_remote_iris_manager_perm <- as.data.table(iris)
my_remote_iris_dev_perm <- as.data.table(iris)
# browser()
# unique(my_remote_iris_dev_perm$Species)
# unique(my_remote_iris_manager_perm$Species)
# sql <- paste0("SELECT Sepal.Length, Sepal.Width, Petal.Length,Petal.Width,Species FROM iris")
# res <- dbSendQuery(con, sql)
# dbFetch(res)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br(),
                         br(),
                         #tags$code("Username: developer  Password: mypass"),
                         br()#,
                         #tags$code("Username: manager  Password: mypass1")
                     ))
)

credentials = data.frame(
    username_id = c("developer", "manager"),
    passod   = sapply(c("mypass", "mypass1"),password_store),
    permission  = c("basic", "advanced"), 
    stringsAsFactors = F
)

header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    
    login = FALSE
    USER <- reactiveValues(login = login)
    
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){ 
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
                sidebarMenu(
                    menuItem("Data", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("Meta Data", tabName = "About", icon = icon("th"))
                )
            }
            else{
                sidebarMenu(
                    menuItem("Data", tabName = "dashboard", icon = icon("dashboard"))
                )
                
            }
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
                tabItems(
                    tabItem(
                        tabName ="dashboard", class = "active",
                        fluidRow(
                            box(width = 12, dataTableOutput('results'))
                        ))
                    ,
                    tabItem(
                        tabName ="About",
                        fluidRow(
                            box(width = 12, dataTableOutput('results2')),
                            box(width = 12, dataTableOutput('meta_data_out')),
                            textOutput("sessionId")
                            
                        ))
                    )
                
            } 
            else {
                tabItem(
                    tabName ="dashboard", class = "active",
                    fluidRow(
                        box(width = 12, dataTableOutput('results'))
                    ))
                
            }
            
        }
        else {
            loginpage
        }
    })
    
    output$results <-  DT::renderDataTable({
        datatable(my_remote_iris_dev_perm, options = list(editable = TRUE, autoWidth = TRUE,
                                       searching = TRUE))
    })
    
    output$results2 <-  DT::renderDataTable({
        datatable(my_remote_iris_manager_perm, options = list(autoWidth = TRUE,
                                         searching = TRUE))
    })
    
    output$meta_data_out <-  DT::renderDataTable({
        datatable(metadata, options = list(autoWidth = TRUE))
    })
    
    
    
    # values <- reactiveValues(sessionId = NULL)
    # values$sessionId <- as.integer(runif(1, 1, 100000))
    # output$sessionId <- renderText(paste0("Database Session id: ", values$sessionId))
    # session$onSessionEnded(function() {
    #     observe(cat(paste0("Ended: ", values$sessionId)))
    # })
      
}

runApp(list(ui = ui, server = server, onStop(function() {dbDisconnect(con)})))