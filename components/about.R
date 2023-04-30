
#------------------------------------ UI SIDE --------------------------------------



aboutScreenUI <- function(id, label = "about-screen") {
  ns <- NS(id)
  fluidPage(
          column(12, style='padding:100px;',
                 h3("About"),
                 br(),
                 HTML("Made with <span style='color: #e25555; font-size: 20px;'>&hearts;</span> by Alexey Zagalsky, Inbal Yahav Shenberger, and Dov Te'Eni &#127279; 2020-2023")
          )
        )
}







aboutScreen <- function(input, output, session){
  
}