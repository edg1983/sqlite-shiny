# README -------------
#This module manage the control of record creation for either 
# - single record insertion using a mask
# - bulk record insertion from TSV file
# The actual record processing is managed by insertControls sub-module
#
#MODULE UI
#Input arguments:
# - tables_schema
#   a tables schema created by parseTablesSchema (see util_functions)
#MODULE SERVER
#Input arguments:
# - mydb
#   database connection
# - tables_schema
#   a tables schema created by parseTablesSchema (see util_functions)
# - insert_type ["single", "bulk]
#   trigger the corresponding sub-module function for single or bulk insertion
# When insert_type is "single", 2 additinal arguments are needed:
# - username
#   character defining the user name 
# - map_columns
#   a named list defining columns mapping for various tables
#   like table_name$value_col = mapped_col
#   the value of mapped_col is displayed as an alias of value_col in selectInput referencing the table

#Libraries ------------
library(RSQLite)
library(DBI)
library(stringr)
library(DT)
library(shinyFiles)

#db <- dbConnect(RSQLite::SQLite(), "")

# User interface -----------
newRecordUI <- function(id, tables_schema) {
  ns <- NS(id)
  tagList(
    fluidRow(align="center",
             selectInput(ns("table_id"),"Insert in",names(tables_schema),multiple = FALSE)
    ),
    tags$hr(),
    uiOutput(ns("newRecordUI"))
  )
}

# Server -------------
newRecordServer <- function(id, mydb, tables_schema, insert_type = "single", username=NULL, map_columns=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(insert_statement=NULL)
      
      observeEvent(input$table_id, {
        if(insert_type == "single") {
          output$newRecordUI <- renderUI({
            ns <- session$ns
            insertSingleUI(ns("newRecordControls"))
          })
        } else if (insert_type == "bulk") { 
          output$newRecordUI <- renderUI({
            ns <- session$ns
            insertBulkUI(ns("newRecordControls"))
          })
        }      
      })
        
      if(insert_type == "single") {
        rv$insert_statement <- insertSingleServer("newRecordControls", mydb, tables_schema, reactive(input$table_id), username, map_columns)
      } else if (insert_type == "bulk") { 
        rv$insert_statement <- insertBulkServer("newRecordControls", mydb, tables_schema, reactive(input$table_id))
      }
    
    
      return(reactive(rv$insert_statement()))
    }
  )
}