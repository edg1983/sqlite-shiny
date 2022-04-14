library(RSQLite)
library(DBI)
library(stringr)
library(DT)
library(shinyFiles)

## User interface --------------
editRecordUI <- function(id, tables_schema) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             tags$div(selectInput(ns("table_id"),"Insert in",names(tables_schema),multiple = FALSE), style = 'padding-left:25px')
      ),
      column(2,
             actionButton(ns("loadtable_btn"),"load table",icon = icon("file-upload"), style = 'margin-top:25px')
      ),
      column(2, actionButton(ns("edit_btn"),"edit rows",icon = icon("check"), style = 'margin-top:25px')),
      column(2, actionButton(ns("delete_btn"),"delete rows",icon = icon("eraser"), style = 'margin-top:25px'))
    ),
    box("Current table",width = 12, solidHeader = T, collapsed = F, status = "primary",
    fluidRow(align = "center", uiOutput(ns("tablename_txt"))),
    fluidRow(align = "center", tags$div(dataTableOutput(ns("table_DT")), style = 'padding-right:25px; padding-left:25px'))),
    box("Editing values in these records",width = 12, solidHeader = T, collapsed = F, status = "primary",
        fluidRow(align="center",
          actionButton(ns("cleanedit_btn"),"cancel changes",icon = icon("trash-alt"), style = 'margin-top:25px'),
          actionButton(ns("submitedit_btn"),"submit changes",icon = icon("check"), style = 'margin-top:25px')
        ),
        fluidRow(tags$div(dataTableOutput(ns("edittable_DT")), style = 'padding-right:25px; padding-left:25px'))
    )
  )
}


## Server ---------------
editRecordServer <- function(id, mydb, tables_schema) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(edit_statement=NULL, table_id=NULL, df=NULL, edit_df=NULL)
      
      observeEvent(input$loadtable_btn, {
        rv$table_id <- input$table_id
        rv$df <- getWholeTable(mydb, rv$table_id, tables_schema)
        rv$edit_df <- NULL
      })
      
      output$tablename_txt <- renderUI({
        h3(rv$table_id)
      })
      
      output$table_DT <- renderDT({
        validate(need(rv$df, "Please select a table and press load"))
        datatable(
          isolate(rv$df), 
          rownames = F,
          editable = F,
          options = list(
            scrollX = TRUE,
            scrollY = "300px",
            scrollCollapse = TRUE,
            columnDefs = list(list(visible=FALSE, targets=c(0)))
          )
        )
      }) 
      proxyDT <- DT::dataTableProxy('table_DT')
      observe({ replaceData(proxyDT, rv$df) })
      
      observeEvent(input$delete_btn, {
        req(input$table_DT_rows_selected)
        placeholder <- makePlaceholder(length(input$table_DT_rows_selected))
        statement <- paste0("DELETE FROM ? WHERE rowid IN (", placeholder, ")")
        values <- rv$df$rowid[input$table_DT_rows_selected]
        sql <- sqlInterpolate(mydb, statement, .dots=c(rv$table_id, values))
        
        rv$edit_statement <- executeStatement(mydb, sql, "Delete records", paste0("Remove ", length(input$table_DT_rows_selected), " records"))
        rv$df <- rv$df[-input$table_DT_rows_selected,]
      })
      
      output$edittable_DT <- renderDT({
        validate(need(rv$edit_df, "Now rows selected for editing"))
        datatable(
          rv$edit_df, 
          rownames = F,
          editable = TRUE,
          selection = "none",
          options = list(
            scrollX = TRUE,
            scrollY = "300px",
            scrollCollapse = TRUE,
            columnDefs = list(list(visible=FALSE, targets=c(0)))
          )
        )
      }) 
      
      observeEvent(input$edittable_DT_data_cell_edit, {
        rv$edit_df[input$edittable_DT_data_cell_edit$row,input$edittable_DT_data_cell_edit$col] <- input$edittable_DT_data_cell_edit$value
      })
      
      observeEvent(input$edit_btn, {
        req(input$table_DT_rows_selected)
        rv$edit_df <- rbind(rv$edit_df, rv$df[input$table_DT_rows_selected,])
        rv$df <- rv$df[-input$table_DT_rows_selected,]
      })
      
      observeEvent(input$cleanedit_btn, {
        rv$df <- getWholeTable(mydb, rv$table_id, tables_schema)
        rv$edit_df <- NULL
      })
      
      observeEvent(input$submitedit_btn, {
        allvalidate <- validateRecordTable(rv$edit_df, mydb, rv$table_id, tables_schema)
        
        #If all validation passed then create insert statement
        if (allvalidate) {
          sql_statement <- makeUpdateStatement(mydb, rv$table_id, "rowid", df)
          message(sql_statement)
        } else {
          sql_statement <- NULL
        }
        
        rv$edit_statement <- executeStatement(mydb, sql_statement, "Edit records", paste0("Edit ", nrow(df), " records"))
        
        rv$df <- getWholeTable(mydb, rv$table_id, tables_schema)
        rv$edit_df <- NULL
      })
      
      return(reactive(rv$edit_statement))
    }
  )
}

