#setwd("/well/brc/Edoardo/github.com/BRC_group_db")
library(shiny)
library(shinydashboard)
library(shinyalert)
library(DBI)

#FUNCTIONS --------
now <- function(fmt="long") {
  switch(fmt,
         long = {
           return(format(Sys.time(), "%Y%m%d_%H%M%S"))
         },
         short = {
           return(format(Sys.time(), "%Y%m%d"))
         }
  )
}

#SETUP ENVIRONMENT ---------------
RV <- reactiveValues(lastaccess=NULL)
config <- jsonlite::read_json("config/db_config.json")
timestamp <- now()

users <- config$users
masterdb <- config$db
backup_dir <- config$backup_dir
log_dir <- config$log_dir
log_file <- paste0(log_dir,"/log_",now("short"),".log")

db <- dbConnect(RSQLite::SQLite(), masterdb)
dbExecute(db, "CREATE TABLE IF NOT EXISTS user_last_access (username TEXT UNIQUE NOT NULL, last_access DATETIME DEFAULT CURRENT_TIMESTAMP)")
tables_schema <- parseTablesSchema(db, config$username_columns, config$ignore_tables)
views <- parseViews(db)
message(paste(views, collapse="\n"))

#BACKUP DB AND CLEAN LOGS ---------
file.copy(masterdb, paste0(backup_dir,"/backup_",timestamp,".db"))
cleanOldFiles(backup_dir, ".db", config$max_backups)
cleanOldFiles(log_dir, ".log", config$max_logs)

#UI ----------
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "BRC group DB", dropdownMenuOutput("messageMenu")),
  dashboardSidebar(
    selectInput("user","UserID",users,selected = "No user",multiple = F),
    uiOutput("sidebar")
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
      tabItem("views_tab",
              fluidRow(
                column(1),
                column(3, 
                       selectInput("data_select", "Data", 
                                   list(tables=names(tables_schema),
                                        views=views),
                                   multiple = F)
                ),
                column(2,
                       actionButton("loaddata_btn", "load data",icon = icon("file-upload"), style = 'margin-top:25px')
                )
              ),
              fluidRow(align="center", h3("Data table")),
              fluidRow(dataTableOutput("view_dt"))
      ),
      tabItem("newparticipant_tab",
              newParticipantUI("newParticipantControls", db)
              ),
      tabItem("newrecord_tab",
              newRecordUI("newRecordControls", tables_schema)
              #newRecordUI("newRecordControls", tables_schema)
      ),
      tabItem("bulkinsert_tab",
              newRecordUI("bulkInsertControls", tables_schema)
              #bulkInsertUI("bulkInsertControls", tables_schema)
      ),
      tabItem("edit_tab",
              editRecordUI("editRecordControls", tables_schema)
      ),
      tabItem("template_tab",
              templatesUI("templatesControls", tables_schema)
      ),
      tabItem("sql_tab",
              executeSqlUI("executeSqlControls")
      )
    )
  )
)

#SERVER ----------
server <- function(input, output, session) { 
  output$messageMenu <- renderMenu({
    req(RV$lastaccess, input$user)
    msgs <- list(
      messageItem(from="DB", message=paste0("Last access: ", RV$lastaccess),icon=icon("calendar-check")),
      messageItem(from="DB", message=paste0("Active user: ", input$user),icon=icon("user"))
    )
    dropdownMenu(type="messages", .list = msgs)
  })
  
  output$sidebar <- renderUI({
    validate(
      need(input$user != "No user", "Please select a user")
    )
    sidebarMenu(
      menuItem("View data", icon = icon("table"), tabName="views_tab"),
      menuItem("New participant", icon = icon("user-plus"), tabName="newparticipant_tab"),
      menuItem("Insert record",icon = icon("pencil-alt"),
        menuSubItem("Insert single record", tabName = "newrecord_tab", icon = icon("feather")),
        menuSubItem("Insert from TSV", tabName = "bulkinsert_tab", icon = icon("file-csv"))
      ),
      menuItem("Modify record", icon = icon("edit"), tabName = "edit_tab"),
      menuItem("Table templates", icon = icon("magic"), tabName = "template_tab"),
      menuItem("Execute SQL", icon = icon("terminal"), tabName = "sql_tab")
    )  
  })
  
  observeEvent(input$user, {
    if (input$user != "No user") {
      sql <- sqlInterpolate(db, "SELECT last_access FROM user_last_access WHERE username = ?", input$user)
      RV$lastaccess <- dbGetQuery(db, sql)
      sql <- sqlInterpolate(db, "REPLACE INTO user_last_access (username) VALUES (?)", input$user)
      dbExecute(db, sql)
    }
  })
  
  output$view_dt <- renderDT({
    sql <- sqlInterpolate(db, "SELECT * FROM ?", .dots=c(input$data_select))
    df <- dbGetQuery(db, sql)
    
    datatable(
      df, 
      rownames = F,
      editable = F,
      extensions = 'Buttons',
      selection = 'none',
      filter='top',
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE,
        dom = 'Bfrti',
        buttons = c('copy', 'csv', 'excel','pdf'),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  }) %>% bindEvent(input$loaddata_btn)    

  
  insertStatement <- newRecordServer("newRecordControls", db, tables_schema, "single", input$user, config$map_columns)
  bulkInsertStatement <- newRecordServer("bulkInsertControls", db, tables_schema, "bulk")
  editStatement <- editRecordServer("editRecordControls",db, tables_schema)
  sqlStatement <- executeSqlServer("executeSqlControls", db)
  saveTemplate <- templatesServer("templatesControls", tables_schema)
  newParticipantStatement <- newParticipantServer("newParticipantControls", db, tables_schema,input$user, config$map_columns)

  toLog <- reactive({
    list(insertStatement(), bulkInsertStatement(), editStatement(), saveTemplate())
  })
  
  observeEvent(toLog() ,{
    for (x in toLog()) {
      if(!is.null(x)) {
        log_item=paste0("## ", now(), " - ", input$user, "\n", x)
        write(log_item,file=log_file,append=TRUE)
      }
    }
  })
  
  #session$onSessionEnded(function() {
  onStop(function() {
    dbDisconnect(db)
    print('disconnected from db')
  })
}

shinyApp(ui, server)