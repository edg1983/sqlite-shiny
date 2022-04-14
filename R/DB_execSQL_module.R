library(RSQLite)
library(DBI)
library(stringr)
library(DT)
library(shinyFiles)

#db <- dbConnect(RSQLite::SQLite(), "")

## User interface -----------
executeSqlUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(align="center", 
      column(2, actionButton(ns("execute_btn"), label = "Execute", icon=icon("play"))),
      column(2, actionButton(ns("clear_btn"), label = "Clear", icon=icon("trash-alt"))),
      column(1),
      column(4,verbatimTextOutput(ns("results_txt"), placeholder=T))
    ),
    fluidRow(
      tags$div(
        textAreaInput(ns("sql_txt"),label = "SQL statement",
                    placeholder = "Enter SQL statement here",
                    rows = 5, width = "100%", resize = "vertical"),
        style = 'padding-right:25px; padding-left:25px')
    ),
    fluidRow(tags$div(h3("Query results (if any)", style = 'padding-left:25px'))),
    fluidRow(
      tags$div(dataTableOutput(ns("query_result_dt")), style = 'padding-right:25px; padding-left:25px') 
    )
  )
}

## Server -------------
executeSqlServer <- function(id, mydb) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(sql_statement=NULL, results=NULL, results_df=NULL)
      
      output$results_txt <- renderText({
        rv$results
      })
      
      output$query_result_dt <- renderDT({
        req(rv$results_df)
        
        datatable(
          rv$results_df, 
          rownames = F,
          editable = F,
          selection = "none",
          options = list(
            scrollX = TRUE,
            scrollY = "300px",
            scrollCollapse = TRUE
          )
        )
      }) %>% bindEvent(input$execute_btn, input$clear_btn)
      
      observeEvent(input$execute_btn, {
        tryCatch({
          if(startsWith(input$sql_txt, "SELECT")) {
            rv$results_df <- dbGetQuery(mydb, input$sql_txt) 
            rv$results <- paste0(nrow(rv$results_df), " records retrieved")
          } else {
            n <- dbExecute(mydb, input$sql_txt)
            rv$results <- paste0(n, " records affected")
            rv$results_df <- NULL
          }
          shinyalert("SQL execution", "Execution of SQL code completed", type="success",closeOnEsc = T)
        }, error=function(cond) {
          rv$results_df <- NULL
          rv$results <- "Error in SQL statement"
          shinyalert("SQL execution", "Execution of SQL code failed", type="error",closeOnEsc = T)
        })
      })
      
      observeEvent(input$clear_btn, {
        rv$results_df <- NULL
        rv$results <- NULL
        updateTextAreaInput(session, "sql_txt", value = "")
      })
      
      return(reactive(rv$sql_statement))
      
    }
  )
}

