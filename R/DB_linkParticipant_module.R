# README -------------
#This module is used to add link for a participant to a family or cohort
#For family only one link can be defined, while a participant can belong to multiple cohorts
#The role within the family/cohort is defined by drop-down box

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
# - username
#   character defining the user name 
# - map_columns
#   a named list defining columns mapping for various tables
#   like table_name$value_col = mapped_col
#   the value of mapped_col is displayed as an alias of value_col in selectInput referencing the table

#Libraries ------------
library(RSQLite)
library(DBI)
library(shiny)
library(shinyWidgets)

#db <- dbConnect(RSQLite::SQLite(), "")
tables <- list(participant = "tblParticipant",
               family = "tblFamily",
               cohort = "tblCohort")

# User interface -----------
newParticipantUI <- function(id, mydb) {
  ns <- NS(id)
  
  tags$head(tags$style("selectedparticipant_text#{color:red; font-size:12px; font-style:italic; 
    overflow-y:scroll; max-height: 50px; background: ghostwhite;}"))
  
  values <- list(
    family = getMappedIDs(mydb, "familyID", "familyLocalID", "tblFamily"),
    cohort = getMappedIDs(mydb, "cohortID", "cohortLocalID", "tblCohort"),
    participant = getMappedIDs(mydb, "participantID", "participantLocalID", "tblParticipant")
  )
 
  tagList(
    fluidRow(align="center", h3("First select participant(s)")),
    fluidRow(
      column(4, 
             fluidRow(selectInput(ns("participant_select"),"Participant", choices = values$participant, size=10, multiple = T, selectize=F)),
             fluidRow(align="center",
                      actionButton(ns("refreshparticipant_btn"), "Refresh", icon=icon("sync"), style = 'margin-top:2px'))
      ),
      column(2, style="padding-top:40px",
             actionButton(ns("addparticipant_btn"),label = "", icon=icon("angle-double-right")),
             actionButton(ns("clearparticipant_btn"), label = "", icon=icon("broom"), style = 'margin-top:2px')
      ),
      column(4,
             fluidRow(selectInput(ns("selectedparticipant_select"),"Selected", choices=NULL,size=10, multiple = F, selectize=F)),
             fluidRow(actionButton(ns("OKparticipant_btn"), "Confirm", icon=icon("check"), style = 'margin-top:2px'))
      )
    ),
    fluidRow(align="center", h3("Then assign participant(s) to a family and/or cohort(s)")),
    box(title = "Family options", status = "primary", width = 12, solidHeader = T, collapsible = T, collapsed = F,
      fluidRow(
        column(3, selectInput(ns("family_select"),"Family ID", choices = values$family,multiple = F)),
        column(2, actionButton(ns("refreshfamily_btn"), "Refresh", icon=icon("sync"), style = 'margin-top:25px')),
        column(6, uiOutput(ns("familyroles")))
      ),
      fluidRow(align="center",
        actionButton(ns("OKfamily_btn"), "Confirm", icon=icon("check"), style = 'margin-top:25px')
      )
    ),
    box(title = "Cohort options", status = "primary", width = 12, solidHeader = T, collapsible = T, collapsed = F,
      fluidRow(
        column(3, selectInput(ns("cohort_select"),"Cohort ID", choices = values$cohort, multiple = T)),
        column(2, actionButton(ns("refreshcohort_btn"), "Refresh", icon=icon("sync"), style = 'margin-top:25px')),
        column(6, 
          fluidRow(align="center",
            materialSwitch(ns("allsame_switch"), "Apply same role to all", value = T, status = "info")
          ), 
          fluidRow(
            uiOutput(ns("cohortroles"))
          )
        )
      ),
      fluidRow(align="center",
        actionButton(ns("OKcohort_btn"), "Confirm", icon=icon("check"), style = 'margin-top:25px')
      )
    )
  )
}

# Server -------------
newParticipantServer <- function(id, mydb, tables_schema, username, map_columns) {
  moduleServer(
    id,
    function(input, output, session) {
      relationships=dbGetQuery(mydb, "SELECT probandRelationship FROM tblProbandRelationship")[,1]
      cohortRoles=dbGetQuery(mydb, "SELECT cohortPClass FROM tblCohortPClass")[,1]
      
      rv <- reactiveValues(
        cohorts=getMappedIDs(mydb, "cohortID", "cohortLocalID", "tblCohort"),
        families=getMappedIDs(mydb, "familyID", "familyLocalID", "tblFamily"),
        participants=getMappedIDs(mydb, "participantID", "participantLocalID", "tblParticipant"),
        selected_participants=c(),
        confirmed = F,
        link_statemenet=NULL
      )
      
      observeEvent(input$refreshparticipant_btn,{
        rv$participants <- getMappedIDs(mydb, "participantID", "participantLocalID", "tblParticipant")
        updateSelectInput(session, "participant_select", choices = rv$participants)
        updateSelectInput(session, inputId = "selectedparticipant_select", choices = "")
        rv$confirmed <- F
      })
      
      observeEvent(input$addparticipant_btn, {
        req(input$participant_select)
        rv$selected_participants <- c(rv$selected_participants,
                                      input$participant_select)
        
        names(rv$selected_participants) <- names(rv$participants[rv$participants %in% rv$selected_participants])
        
        remaining_participants <- setdiff(rv$participants, rv$selected_participants)
        names(remaining_participants) <- names(rv$participants[rv$participants %in% remaining_participants])
        updateSelectInput(session, inputId = "participant_select", choices = remaining_participants)
        updateSelectInput(session, inputId = "selectedparticipant_select", choices = rv$selected_participants)
      })
      
      observeEvent(input$clearparticipant_btn,{
        rv$selected_participants <- c()
        updateSelectInput(session, inputId = "participant_select", choices = rv$participants)
        updateSelectInput(session, inputId = "selectedparticipant_select", choices = "")
        rv$confirmed <- F
      })
      
      observeEvent(input$OKparticipant_btn,{
        rv$confirmed <- T
      })
      
      observeEvent(input$refreshcohort_btn,{
        rv$cohorts <- getMappedIDs(mydb, "cohortID", "cohortLocalID", "tblCohort")
        updateSelectInput(session, "cohort_select", choices = rv$cohorts)
      })
      
      observeEvent(input$refreshfamily_btn,{
        rv$families <- getMappedIDs(mydb, "familyID", "familyLocalID", "tblFamily")
        updateSelectInput(session, "family_select", choices = rv$families)
      })
      
      output$familyroles <- renderUI({
        req(rv$confirmed)
        #ns <- session$ns
        
      }) %>% bindEvent(input$OKparticipant_btn)
      
      output$cohortroles <- renderUI({
        req(rv$confirmed)
        #ns <- session$ns
        
      }) %>% bindEvent(input$OKparticipant_btn)
        
      
      observeEvent(input$OKfamily_btn,{
        rv$selected_family = input$family_select
      })

      observeEvent(input$OKcohort_btn,{
        rv$selected_cohort = input$cohort_select
      })
      
      #observeEvent(rv$participant_statement, {
      #  req(rv$participant_statement)
      #  newParticipantID <- dbGetQuery(mydb, "SELECT last_insert_rowid()")[,1]
      #  if(!is.null(rv$selected_family)) {
      #    sql <- makeInsertStatement(mydb, "tblParticipant2family", 
      #                               list(p2fParticipantID = newParticipantID,
      #                                    p2fFamilyID = rv$selected_family,
      #                                    p2fRelationToProband = input$relation,
      #                                    p2fEditBy = usernam))
      #    res <- dbExecute(mydb, sql)
      #    if (res > 0) {
      #      rv$linker_statement <- sql
      #    }
      #  }
      #  if(!is.null(rv$selected_cohort)) {
      #    n_cohorts <- length(rv$selected_cohort)
      #    df <- data.frame(
      #      p2cParticipantID = rep(newParticipantID, n_cohorts),
      #      p2cCohortID = rv$selected_cohort,
      #      p2cCohortParticipantClass = rep(input$cohortclass, n_cohorts),
      #      p2cEditBy = rep(username, n_cohorts)
      #    )
      #    sql <- makeBulkInsertStatement(mydb, "tblParticipant2cohort", df)
      #    res <- dbExecute(mydb, sql)
      #    if (res > 0) {
      #      rv$linker_statement <- sql
      #    }
      #  }
      #})
      
      return(reactive(rv$link_statement))
    }
  )
}