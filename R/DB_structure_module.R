library(RSQLite)
library(DBI)
library(stringr)
library(DT)
library(shinyFiles)

#db <- dbConnect(RSQLite::SQLite(), "")

## User interface -----------
templatesUI <- function(id, tables_schema) {
  ns <- NS(id)
  tagList(
    fluidRow(align="center", 
             actionButton(ns("download_btn"), "download template", icon = icon("download"))),
    tags$hr(),
    fluidRow(
      column(1),
      column(3, fluidRow(selectInput(ns("table_select"), label = "Table", choices = names(tables_schema), multiple = F))),
      column(8, tags$div(uiOutput(ns("table_description"))), style = 'padding-top:25px')
    )
  )
}

## Server -------------
templatesServer <- function(id, tables_schema) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(df=list(), template=NULL)
      
      output$table_description <- renderUI({
        tab_id <- input$table_select
        tab <- tables_schema[[tab_id]]
        col_list <- list()
        rv$df[[tab_id]] <- list()
        n <- 0
        for (col in names(tab$columns)) {
          n <- n + 1
          
          col_tags <- list(primary = NULL, non_null = NULL, unique = NULL)
          if (tab$columns[[col]]$primary) {col_tags$primary <- "PRIMARY"}
          if (tab$columns[[col]]$not_null) {col_tags$not_null <- "NOT NULL"}
          if (tab$columns[[col]]$unique) {col_tags$unique <- "UNIQUE"}
          col_name <- HTML(paste0("<b>", col, "</b>"))
          col_title <- paste0(col_name, ":")
          col_description <- paste(c(tab$columns[[col]]$type,col_tags$primary,col_tags$unique,col_tags$not_null),
                                   collapse=" ")
          if (!(tab$columns[[col]]$primary || tab$column[[col]]$timestamp)) {rv$df[[tab_id]][[col]] <- col_description}
          col_list[[n]] <- tags$li(HTML(paste0(col_title, " " , col_description)))
        }
        
        box(title = input$table_select, status = "primary", solidHeader = T, width = 12, 
            h4("Columns"),
            tags$ol(col_list))
      }) %>% bindEvent(input$table_select)
      
      observeEvent(input$download_btn, {
        df <- data.frame()
        tryCatch({
          for(col in names(rv$df[[input$table_select]])) {
            df[1,col] <- rv$df[[input$table_select]][[col]]
          }
          filename <- paste0("templates/",input$table_select,"_template.tsv")
          write.table(df, file=filename, sep="\t", row.names=F, quote=F)
          rv$template <- paste0("TEMPLATE DOWNLOAD: ", filename, " FOR TABLE ", input$table_select)
          shinyalert("Template generation", paste0("Template file generated: ", filename), type = "success", closeOnEsc = T )
        }, error=function(cond) {
          shinyalert("Template generation", paste0("Failed to generate template file ", filename), type = "error", closeOnEsc = T )
          rv$template <- NULL
        })
      })
      
      return(reactive(rv$template))
      
    }
  )
}