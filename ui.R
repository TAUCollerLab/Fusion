#' The main UI file of the system. It will load the different UI modules as needed.
#' Each component in fusion is built as a module (typically consists of a single R file). 
#' Each component has its UI and Server code as a way to maintain modularity but have each component self-contained as much as possible.


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)


# The name of the application
header <- dashboardHeader(
    title = "FUSION",
    disable = FALSE
)

#' This is the sidebar menu of the UI.
#' 
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar_tabs",
        uiOutput("userInfo"),
        br(),
        br(),
        menuItemOutput("projectName"), 
        menuItem("Data", tabName = "data", icon = icon("th")),
        menuItem("Lexicon", tabName = "lexicon-building", icon = icon("flask")),
        menuItem("Model Generation", tabName = "model", icon = icon("dashboard")),
        menuItem("Reflection", tabName = "reflection", icon = icon("chart-area")),
        menuItem("New Insights", tabName = "new-insights", icon = icon("chart-area")),
        menuItem("Tools", tabName = "useful-tools", icon = icon("toolbox"),
                 menuSubItem("Adv. Model Generation", tabName = "adv-model", icon = icon("dashboard"))
        ),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
)



#' The main view of the Shiny application, not including the sidebar menu, nor the header.
#' 
body <- dashboardBody(
    ## add an external CSS file
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ## These correspond to the sidebar menu pages/categories.
    tabItems(
        ## project page
        tabItem(tabName = "project",
                listProjectsPageWidget(id = "loadProjects")
        ),
        
        
        ## Data acquisition page
        tabItem(tabName = "data",
                loadWorkEnvMenu("loadData", "DataLoadingMenu")
        ),

        
        ## Model generation and running page
        tabItem(tabName = "model",
                simpleModelWidget("createSimpleModels", "ModelsMenu")
        ),
        
        
        ## Advanced Model generation and running page
        tabItem(tabName = "adv-model",
                listAllModelsWidget("listModels", "RunningModelsMenu")
        ),
        
        
        ## Model assisted analysis and exploration page
        tabItem(tabName = "reflection",
                reflectionEngineWidget("reflectionUI")
        ),
        
        
        ## Model assisted analysis and exploration page
        tabItem(tabName = "new-insights",
                insightsEngineWidget("insightsUI")
        ),
        
        
        # Lexicon Tweaking page
        tabItem(tabName = "lexicon-building",
                buildLexiconWidget("lexiconTab")
        ),
        
        
        # About screen
        tabItem(tabName = "about",
                aboutScreenUI("aboutScreen")
        )
    )
)



#' This defines the UI of the Shiny app. It splits it into: header, sidebar, and body.
ui <- dashboardPage(
    title="Fusion: Supercharging Text-Classification",
    header,
    sidebar,
    body,
    skin = "blue"
)