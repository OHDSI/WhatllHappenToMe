# modules
# input componenet
inputSelectionViewer <- function(id = "input-selection") {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    # UI for inputs
    # summary table
    shinydashboard::box(
      collapsible = TRUE,
      title = "Tell Us About Your History",
      width = "100%",
      shiny::uiOutput(ns("inputs"))
    ),
    
    # displayed inputs
    shiny::conditionalPanel(
      condition = "input.generate != 0",
      ns = ns,
      
      shinydashboard::box(
        status = 'warning', 
        width = "100%",
        title = 'Selected History: ', 
        collapsible = T,
        shiny::uiOutput(ns("inputsText"))
      )
    )
    
  )
  
}


# UI
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "WhatllHappenToMe.org"
  ),
  shinydashboard::dashboardSidebar(),
  shinydashboard::dashboardBody(
    # Boxes need to be put in a row (or column)
    inputSelectionViewer('inputs'),
    
    # only show results if generated
    shiny::conditionalPanel(
      condition = "input.generate != 0",
      ns = shiny::NS('inputs'),
      shinydashboard::box(
        width = '100%',
        title = 'Personalised Predicted Risks', 
        solidHeader = T, 
        status = 'primary',
        shiny::div(
          reactable::reactableOutput("predictions")
        )
      )
    )
  )
  
)