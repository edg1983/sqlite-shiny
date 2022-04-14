# README -------------
#This module manage create controls for input data for
# - single record insertion using a mask
# - bulk record insertion from TSV file
#The module contains insertSingle and inserBulk UI/Server 
#MODULE SERVER
#Input arguments:
# - mydb
#   database connection
# - tables_schema
#   a tables schema created by parseTablesSchema (see util_functions)
# - table_id_rv
#   a reactive containing the table_id for insertion
#   this is usually passed using reactive(value)

#Libraries ---------------
library(RSQLite)
library(DBI)
library(stringr)
library(DT)
library(shinyFiles)

#db <- dbConnect(RSQLite::SQLite(), "")

# INSERT SINGLE ---------------------
## User interface -----------
insertSingleUI <- function(id) {
  ns <- NS(id)
  #tagList(
    uiOutput(ns("newRecordBox"))
  #)
}

## Server -------------
insertSingleServer <- function(id, mydb, tables_schema, table_id_rv, username, map_columns) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(insert_statement=NULL)
      output$newRecordBox <- renderUI({
        ns <- session$ns
        table_id <- table_id_rv()
    
        outcontrols <- c()
        tbl_schema <- tables_schema[[table_id]]
        for(col in names(tbl_schema$columns)) {
          #for primary, timestamp and username new record must take a default value so we do not create a control for these columns
          if (!(tbl_schema$columns[[col]]$primary || tbl_schema$columns[[col]]$timestamp || tbl_schema$columns[[col]]$username)) {
            if (!is.null(tbl_schema$checks[[col]])) {
              outcontrols <- c(outcontrols,
                               tagList(selectInput(ns(col), col, tbl_schema$checks[[col]], multiple = FALSE))
              )          
            } else if (!is.null(tbl_schema$foreignkeys[[col]])) {
              foreign_col <- tbl_schema$foreignkeys[[col]][2]
              foreign_table <- tbl_schema$foreignkeys[[col]][1]
              if(!is.null(map_columns[[foreign_table]][[foreign_col]])){
                map_col <- map_columns[[foreign_table]][[foreign_col]]
                values <- getMappedIDs(mydb, foreign_col, map_col, foreign_table)
              } else {
                query <- paste0("SELECT ", foreign_col, " FROM ", foreign_table)
                values <- unique(dbGetQuery(mydb,query)[,1])
              }
              
              values <- c("", values)
              outcontrols <- c(outcontrols,
                               tagList(selectizeInput(ns(col), col, values, selected="", multiple = FALSE, options = list(showEmptyOptionInDropdown=T, emptyOptionLabel="0")))
              )
            } else if (tbl_schema$columns[[col]]$unique) {
              query <- paste0("SELECT ", col, " FROM ", table_id)
              values <- unique(dbGetQuery(mydb,query)[,1]) 
              outcontrols <- c(outcontrols,
                               tagList(selectizeInput(ns(col), label = col, choices = values, multiple = FALSE, options = list(create = TRUE)))
              )
            } else {
              datatype <- tbl_schema$columns[[col]]$type
              outcontrols <- c(outcontrols,
                               tagList(textInput(ns(col), col, placeholder=paste0("Enter ", datatype)))
              )
            }
          }
        }
        col1_n_elements <- floor(length(outcontrols)/2)
        col1 <- tagList()
        col2 <- tagList()
        for (n in 1:col1_n_elements) {
          col1 <- tagList(col1,outcontrols[n])
        }
        for (n in (col1_n_elements+1):length(outcontrols)) {
          col2 <- tagList(col2,outcontrols[n])
        }
        
        tagList(
          box(title = paste0("New record for ", table_id),
              solidHeader = T, 
              background = "orange",
              collapsible = F,
              width=12,
              
              fluidRow(
                column(width = 6, col1),
                column(width = 6, col2)
              ),
              fluidRow(align="center",
                       actionButton(ns("submit_btn"),"Submit",icon = icon("pen"))
              )
          )
        )
      })
      
      observeEvent(input$submit_btn, {
        allvalidate = TRUE
        table_id <- table_id_rv()
        
        for(col in names(tables_schema[[table_id]]$columns)) {
          switch(validateInputField(input[[col]],mydb,table_id,col,tables_schema[[table_id]]$columns[[col]]),
                 NOTUNIQUE = {
                   msg <- paste0(input[[col]], " for ", col, " is already present in ", table_id,"\nPlease choose a unique value")
                   shinyalert("Input validation", msg, type = "error")
                   allvalidate = FALSE
                 },
                 ISNULL = {
                   msg <- paste0("Value for ", col, " can not be empty or null")
                   shinyalert("Input validation", msg, type = "error")
                   allvalidate = FALSE
                 },
                 OK = {
                   next
                 })
        }
        if (allvalidate) {
          insert_values <-list()
          for(col in names(tables_schema[[table_id]]$columns)) {
            #If this is a username column, then the username defined in the general app is inserted as value
            if (tables_schema[[table_id]]$columns[[col]]$username) {
              insert_values[[col]] <- username
            } else {
              insert_values[[col]] <- input[[col]]  
            }
          }
          sql_statement <- makeInsertStatement(
            mydb, 
            table_id,
            insert_values)
        } else {
          sql_statement <- NULL
        }
        
        rv$insert_statement <- executeStatement(mydb, sql_statement, "Insert new record", "Insert single record")
        #message("single submodule: ", rv$insert_statement)
      })
      
      return(reactive(rv$insert_statement))
      
    }
  )
}

# INSERT BULK --------------
## User interface --------------
insertBulkUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2),
      column(3,
             shinyFilesButton(ns("tsvFile"), label="Open file", title="Load records from TSV", "", multiple=F, icon = icon("file-upload"), style = 'margin-top:25px')
             ),
      column(2, actionButton(ns("insert_btn"), "Insert records", icon = icon("plus"), style = 'margin-top:25px')),
      column(4, tags$div(verbatimTextOutput(ns("n_records_txt"), placeholder = T), style = 'padding-top:25px; padding-right:25px'))
      ),
    fluidRow(align = "center", h3("Revise records")),
    fluidRow(align = "center", tags$div(dataTableOutput(ns("loaded_records_DT")), style = 'padding-right:25px; padding-left:25px'))
  )
}

## Server ---------------
insertBulkServer <- function(id, mydb, tables_schema, table_id_rv) {
  moduleServer(
    id,
    function(input, output, session) {
      shinyFileChoose(input,'tsvFile', session=session, roots=c(wd='.'))
      rv <- reactiveValues(insert_statement=NULL)

      data_to_insert <- reactive({
        table_id <- table_id_rv()
        
        file <- parseFilePaths(roots=c(wd='.'), input$tsvFile)
        req(file$datapath)
          mandatory_columns <- c()
          for(col in names(tables_schema[[table_id]]$columns)) {
            if(!(tables_schema[[table_id]]$columns[[col]]$primary || tables_schema[[table_id]]$columns[[col]]$timestamp)) {
              mandatory_columns <- c(mandatory_columns, col)
            }
          }
          
          df <- read.csv(file$datapath, sep="\t", header=T, stringsAsFactors = F)
          missing_cols <- setdiff(mandatory_columns, colnames(df))
          unknown_cols <- setdiff(colnames(df), mandatory_columns)
          if (length(missing_cols)) {
            shinyalert("Import table", paste0("Mandatory column(s) missing:\n", paste(missing_cols, collapse=",")),type = "error", closeOnEsc = T)
            return(NULL)
          }
          if (length(unknown_cols)) {
            shinyalert("Import table", paste0("Unknown columns found, these will be ignored:\n", paste(unknown_cols, collapse=",")),type = "warning", closeOnEsc = T)
          }
          return(df)
      }) %>% bindEvent(input$tsvFile)
      
      output$n_records_txt <- renderText({
        if(is.null(data_to_insert())) {
          res <- "No records"
        } else {
          res <- paste0(nrow(data_to_insert()), " records loaded")
        }
        res
      })
      
      output$loaded_records_DT <- DT::renderDataTable({
        validate(need(data_to_insert(), "Please load records from a file"))
        datatable(
          data_to_insert(),
          selection = "none",
          options = list(
            scrollX = TRUE,
            scrollY = "300px",
            scrollCollapse = TRUE,
            autoWidth = TRUE))
      }) 
      
      observeEvent(input$insert_btn, {
        req(data_to_insert())
        table_id <- table_id_rv()
        df <- data_to_insert()
        col_names <- colnames(df)
        
        #Validate rows
        allvalidate=TRUE
        notunique_rows <- list()
        isnull_rows <- list()
        
        #Check if there are dup values in unique columns
        dup_values <- list()
        for(col in col_names) {
          if(tables_schema[[table_id]]$columns[[col]]$unique) {
            counts <- table(df[,col])
            if (sum(counts > 1)) {
              dup_values[[col]] <- names(counts[counts > 1])
            }
          }
        }
        if(length(dup_values) > 0) {
          msg <- paste0("Duplicated values found in the following columns and not allowed in ", table_id,"\n",
                        paste(makeVecFromList(dup_values), collapse="\n"))
          shinyalert("Input validation", msg, type = "error")
          allvalidate=FALSE
        }
        
        #Validate each rows according to table schema constraints
        for(i in 1:nrow(df)) {
          for(col in col_names) {
            value <- df[i,col]
            switch(validateInputField(value,mydb,table_id,col,tables_schema[[table_id]]$columns[[col]]),
                   NOTUNIQUE = {
                     notunique_rows[[col]] <- c(notunique_rows[[col]], i)
                   },
                   ISNULL = {
                     isnull_rows[[col]] <- c(isnull_rows[[col]], i)
                   },
                   OK = {
                     next
                   })
          }
        }
        
        if(length(notunique_rows) > 0) {
          msg <- paste0("We found values already present in ", table_id," in some rows for the following unique columns\n",
                        paste(makeVecFromList(notunique_rows), collapse="\n"))
          shinyalert("Input validation", msg, type = "error")
          allvalidate = FALSE
        }
        
        if(length(isnull_rows) > 0) {
          msg <- paste0("We found empty values in some rows for the following mandatory columns in ", table_id, "\n",
                        paste(makeVecFromList(notunique_rows), collapse="\n"))
          shinyalert("Input validation", msg, type = "error")
          allvalidate = FALSE
        }
        
        #If all validation passed then create insert statement
        if (allvalidate) {
          sql_statement <- makeBulkInsertStatement(mydb, table_id, df)
        } else {
          sql_statement <- NULL
        }
        
        rv$insert_statement <- executeStatement(mydb, sql_statement, "Insert multiple records", paste0("Insert ", nrow(df), " records"))
      })
      
      return(reactive(rv$insert_statement))
    }
  )
}

