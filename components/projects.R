#' A module for creating new projects, and listing and managing existing projects within the application.

library(shiny)
library(tidyverse)


#------------------------------------ UI SIDE --------------------------------------


#' Creates the UI components to build the projects list page.
#' 
#' @param id
#' @param label
#' 
#' @return list of shiny UI components
listProjectsPageWidget <- function(id, label = "ProjectsPage") {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(1,
             div(style="display:inline-block; margin-bottom:0px;",dropdownButton(
               textInput(inputId = ns("newProjectName"), placeholder = "Project name", label = "", width = "500px"),
               textAreaInput(inputId = ns("newProjectDescription"), placeholder = "Description", label = "", width = "500px"),
               actionButton(ns("createProject"), label = "Add", title = "Add a new project"),
               circle = TRUE, status = "primary", icon = icon("plus"), width = "550px",
               tooltip = tooltipOptions(title = "Add Porject")
             ))
             
      )
    ),
    fluidRow(
      column(12,
             uiOutput(ns("projectList"))
      ))
  )
}


#------------------------------------ SERVER SIDE --------------------------------------


#' Server-side logic for listing the existing projects for the specific user.
#' 
#' @param input
#' @param output
#' @param session
#' 
listProjectsPage <- function(input, output, session){
  
  current_projects_list <- reactiveVal(NULL)
  
  #' Creates the UI components needed to present a list of projects with radio select buttons.
  #' 
  #' @return a shiny UI component
  output$projectList <- renderUI({
    ns <- session$ns
    
    current_projects_list(getProjects(current_user_id()))
    if (is.null(current_projects_list())){
      return()
    }
    
    df <- current_projects_list() %>% 
      mutate(title = paste(name, description, sep = " - "))
    df.list <- as.list(df$title)
    
    return(
      awesomeRadio(12,
                   inputId = ns("project_select_radio"),
                   label = "", 
                   choices = df.list,
                   selected = df.list[1],
                   inline = FALSE, 
                   status = "primary"
      )
    )
  })
  
  
  
  # Observes to see when there are changes to the project selection radio button. 
  # Upon any changes it will update the current_project_id reactive variable.
  observeEvent(input$project_select_radio,{
    df <- getProjects(current_user_id()) %>% mutate(title = paste(name, description, sep = " - "))
    current_project_id(which(df$title == input$project_select_radio))
  }, ignoreInit = TRUE)
  
  
  
  #' Observer for when the "add new project" button is clicked. 
  #' It adds a new project to database and updates the stored list of projects.
  #' 
  observeEvent({
    input$createProject
  },{
    project_name <- input$newProjectName
    project_description <- input$newProjectDescription
    
    if (project_name=="" | project_description==""){
      sendSweetAlert(session = session, 
                     title = "Missing Information", 
                     text = "While adding a new project we noticed that some important information is missing. Please add the missing information and try again.",
                     type = "error")
    }
    else {
      proj_id = addNewProject(current_user_id(), project_name, project_description)
      user_projects <- getProjects(current_user_id())
      current_projects_list(user_projects)
      current_project_id(proj_id)
    }
  })
}