require(dplyr)

createInputSetting <- function(
    rowNumber,                           
    columnWidth = 4,
    varName = '',
    inputReturn = T,
    uiFunction = 'shinyWidgets::pickerInput',
    uiInputs = list(
      label = 'Input: ',
      choices = list(),
      multiple = F,
      options = shinyWidgets::pickerOptions()
    ),
    updateFunction = NULL,
    collapse = F
){
  
  result <- list(
    rowNumber = rowNumber,
    columnWidth = columnWidth,
    varName = varName,
    inputReturn = inputReturn,
    uiFunction = uiFunction,
    uiInputs = uiInputs,
    updateFunction = updateFunction,
    collapse = collapse
  )
  
  class(result) <- 'inputSetting'
  return(
    result
  )
}

inputSelectionServer <- function(
    id, 
    inputSettingList
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      if(inherits(inputSettingList, 'inputSetting')){
        inputSettingList <- list(inputSettingList)
      }
      
      rowNumbers <- unlist(lapply(inputSettingList, function(x){x$rowNumber}))
      inputNames <- unlist(lapply(inputSettingList, function(x){x$varName}))
      rows <- list()
      for(i in 1:max(rowNumbers)){
        rows[[i]] <- shiny::fluidRow(
          lapply(which(rowNumbers == i), function(x){
            
            inputs <- inputSettingList[[x]]$uiInputs
            if(inputSettingList[[x]]$inputReturn){
              # if using a function that has no return (e.g., div) set 
              # inputReturn = F
              inputs$inputId <- session$ns(paste0('input_',x))
            }
            
            shiny::column(
              width = inputSettingList[[x]]$columnWidth,
              do.call(eval(parse(text = inputSettingList[[x]]$uiFunction)), inputs)
            )
          }
          )
        )
      }
      rows[[length(rows)+1]] <- shiny::actionButton(
        inputId = session$ns('generate'), 
        label = 'Generate Report'
      )
      
      # add reset here
      rows[[length(rows)+1]] <- shiny::actionButton(
        inputId = session$ns('reset'), 
        label = 'Reset'
      )
      
      output$inputs <- shiny::renderUI({
        shiny::fluidPage(rows)
      })
      
      selectedInput <- shiny::reactiveVal()
      selectedInputText <- shiny::reactiveVal()
      output$inputsText <- shiny::renderUI(selectedInputText())
      
      # when generate is pressed update the selected values and text
      shiny::observeEvent(
        eventExpr = input$generate,
        {
          
          # get the input values and store in reactiveval
          inputList <- lapply(
            1:length(inputNames), 
            function(x){
              input[[paste0('input_', x)]]
            }
          )
          names(inputList) <- inputNames
          selectedInput(inputList)
          
          # create the text output
          
          otext <- list()
          for(i in 1:max(rowNumbers)){
            otext[[i]] <- shiny::fluidRow(
              lapply(which(rowNumbers == i), function(x){
                shiny::column(
                  width = inputSettingList[[x]]$columnWidth,
                  shiny::tags$b(paste0(inputSettingList[[x]]$uiInputs$label)),
                  if(!is.null(inputSettingList[[x]]$uiInputs$choices)){
                    # adding below incase a vector with no names is used
                    if(is.null(names(inputSettingList[[x]]$uiInputs$choices))){
                      names(inputSettingList[[x]]$uiInputs$choices) <- inputSettingList[[x]]$uiInputs$choices
                    }
                    
                    # add selections on new row unless collapse is F
                    if(!inputSettingList[[x]]$collapse){
                      shiny::HTML(
                        paste("<p>", names(inputSettingList[[x]]$uiInputs$choices)[inputSettingList[[x]]$uiInputs$choices %in% input[[paste0('input_',x)]]], '</p>')
                      )
                    } else{
                      paste(names(inputSettingList[[x]]$uiInputs$choices)[inputSettingList[[x]]$uiInputs$choices %in% input[[paste0('input_',x)]]], collapse = ', ')
                    }
                  } else{
                    
                    # add selections on new row unless collapse is F
                    if(!inputSettingList[[x]]$collapse){
                      shiny::HTML(
                        paste("<p>", input[[paste0('input_',x)]], '</p>')
                      )
                    } else{
                      paste(input[[paste0('input_',x)]], collapse = ', ')
                    }
                  }
                )
              }
              )
            )
          }
          selectedInputText(shiny::div(otext))
        })
      
      
      # do the reset stuff
      shiny::observeEvent(
        eventExpr = input$reset,
        {
          # code to reset to default
          
          for(i in 1:length(inputSettingList)){
            if(!is.null(inputSettingList[[i]]$updateFunction)){
              
              # need to test for non-picker inputs
              do.call(eval(parse(text = inputSettingList[[i]]$updateFunction)), 
                      list(
                        session = session, 
                        inputId = paste0('input_',i), 
                        selected = inputSettingList[[i]]$uiInputs$selected
                      ))
              
            }
          }
        })
      
      return(selectedInput)
      
    }
  )
}

withTooltip <- function(value, tooltip, ...) {
  shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
             tippy::tippy(value, tooltip, ...))
}


# function to extract all predictors used by models
getPredictors <- function(
    connectionHandler,
    schema
){
  
  sql <- "SELECT distinct
  covariate_id,
  covariate_name
  
  FROM
  @schema.covariates;
  "
  
  sql <- SqlRender::render(
    sql = sql, 
    schema = schema
    )
  result <- DatabaseConnector::querySql(
    connection = connectionHandler, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  predictors <- result$covariateId
  names(predictors) <- result$covariateName
  
  #order
  predictors <- predictors[sort(names(predictors))]
  
  ind <- c(
    grep('gender', tolower(names(predictors))),
    grep('age group', tolower(names(predictors)))
  )
  
  return(predictors[-ind])
}

# function for displaying risk in reactable
bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  # format the label to be % to 2pd
  labelPercent <- paste0(signif(label*100, digits = 3), '%')
  
  # conditional fill based on value
  fillFunction <- function(value) {
    if (value > 0.3) {
      color <- "#e00000"
    } else if (value < 0.01) {
      color <- "#008000"
    } else {
      color <- "#777"
    }
    return(color)
  }
  
  fill <- fillFunction(label)
  
  bar <- shiny::div(style = list(background = fill, width = width, height = height))
  chart <- shiny::div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), labelPercent, chart)
}

getOptions <- function(
    connectionHandler,
    schema = 'main'
){
  
  sql <- "select distinct
  tar_id, 
  tar_start_day,
  tar_start_anchor,
  tar_end_day,
  tar_end_anchor
  
  from 
  
  @schema.tars 
  ;"
  
  sql <- SqlRender::render(
    sql = sql, 
    schema = schema
  )
  result <- DatabaseConnector::querySql(
    connection = connectionHandler, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  tars <- result$tarId
  names(tars) <- paste0(
    '(',result$tarStartAnchor,' + ', result$tarStartDay,
    ') - (',
    result$tarEndAnchor,' + ', result$tarEndDay,')'
  )
  
  sql <- "select distinct
  c.cohort_id,
  c.cohort_name
  
  from 
  
  @schema.cohorts c 
  inner join  
  @schema.model_details md
  on c.cohort_id = md.target_id
  ;"
  
  sql <- SqlRender::render(
    sql = sql, 
    schema = schema
  )
  result <- DatabaseConnector::querySql(
    connection = connectionHandler, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  targets <- result$cohortId
  names(targets) <- result$cohortName
  
  return(
    list(
      tars = tars,
      targets = targets
    )
  )
}

# apply models
# input: predictor values + target+id + connection
# return: data.frame with model name and predicted value for all outcomes
getPrediction <- function(
    predictorValue, 
    targetId,
    tarId,
    connectionHandler,
    schema = 'main'
){
  
  # get model intercepts: model_id, intercept
  sql <- "select 
  i.model_id, 
  i.intercept from 
  
  @schema.model_intercept i
  inner join
  @schema.model_details md
  on md.model_id = i.model_id
  
  where md.target_id = @target_id and md.tar_id = @tar_id
  ;"
  
  sql <- SqlRender::render(
    sql = sql, 
    schema = schema,
    target_id = targetId,
    tar_id = tarId
  )
  intercept <- DatabaseConnector::querySql(
    connection = connectionHandler, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  # get normalised coefficients
  
  sql <- "SELECT 
  mc.model_id, 
  outcome.cohort_name as outcome_name,
  mc.covariate_id, 
  cast(mc.coefficient as float)/cast(p.max_value as float) as norm_coefficient
  
  from
  @schema.model_coefficients mc inner join
  @schema.processing p
  on mc.model_id = p.model_id and 
  cast(mc.covariate_id as integer) = cast(p.covariate_id as integer)
  
  inner join
  (select md.model_id, c.cohort_name
  from 
  @schema.model_details md inner join
  @schema.cohorts c
  on c.cohort_id = md.outcome_id
  where md.target_id = @target_id 
  and md.tar_id = @tar_id
  ) as outcome
  on outcome.model_id = mc.model_id
  ;
  "
  
  sql <- SqlRender::render(
    sql = sql, 
    schema = schema,
    target_id = targetId,
    tar_id = tarId
  )
  modelCoef <- DatabaseConnector::querySql(
    connection = connectionHandler, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  # join predictor df with modelCoef by covariateId
  prediction <- merge(
    x = predictorValue, 
    y = modelCoef, 
    by = "covariateId"
  ) %>% 
    dplyr::group_by(.data$modelId, .data$outcomeName) %>%
    dplyr::summarise(value = sum(.data$normCoefficient))
  
  prediction <- merge(
    x = intercept, 
    y = prediction, 
    all.x = T,
    by = c('modelId')
  )
  
  prediction[is.na(prediction)] <- 0
  prediction$predictedValue <- 1/(1+exp(-prediction$value - prediction$intercept))
  
  return(prediction)
}


server <- function(input, output) {
  
  schema <- Sys.getenv("whatllhappentomedbSchema")
  # Create connection details to results database ---------------
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = Sys.getenv("whatllhappentomedbServer"),
    user = Sys.getenv("whatllhappentomedbUser"),
    password = Sys.getenv("whatllhappentomedbPw")
  )
  
  connectionHandler <- DatabaseConnector::connect(
    connectionDetails = connectionDetails
  )
  
  options <- getOptions(
    connectionHandler = connectionHandler,
    schema = schema
  )
  # options$tars, options$targets
  
  # get a data.frame with the covariate names and ids
  predictors <- getPredictors(
    connectionHandler = connectionHandler,
    schema = schema
  )
  
  ages <- (0:20)*1000 + 3
  names(ages) <- paste(seq(0, 100, 5), (seq(0, 100, 5)+4), sep = '-')
  
  sexs <- c(8507, 8532)*1000 + 1
  names(sexs) <- c('Male', 'Female')
  
  # create target selector
  # create an age group selection input
  # create a sex input
  # create an picker selection to select which covariates a person has 
  
  inputSelected <- inputSelectionServer(
    id = 'inputs', 
    inputSettingList = list(
      createInputSetting(
        rowNumber = 1,                           
        columnWidth = 8,
        varName = 'targetId',
        inputReturn = T,
        uiFunction = 'shinyWidgets::pickerInput',
        uiInputs = list(
          label = 'Target: ',
          choices = options$targets, #list(`General population` = 1),
          selected = options$targets[1],
          multiple = F,
          options = shinyWidgets::pickerOptions()
        ),
        updateFunction = NULL,
        collapse = F
      ),
      createInputSetting(
        rowNumber = 1,                           
        columnWidth = 4,
        varName = 'tarId',
        inputReturn = T,
        uiFunction = 'shinyWidgets::pickerInput',
        uiInputs = list(
          label = 'Time-at-risk: ',
          choices = options$tars, #list(`General population` = 1),
          selected = options$tars[1],
          multiple = F,
          options = shinyWidgets::pickerOptions()
        ),
        updateFunction = NULL,
        collapse = F
      ),
      createInputSetting(
        rowNumber = 2,                           
        columnWidth = 6,
        varName = 'ageGroup',
        inputReturn = T,
        uiFunction = 'shinyWidgets::pickerInput',
        uiInputs = list(
          label = 'Age Range: ',
          choices = ages, 
          selected = ages[10],
          multiple = F,
          options = shinyWidgets::pickerOptions()
        ),
        updateFunction = 'shinyWidgets::updatePickerInput',
        collapse = F
      ),
      createInputSetting(
        rowNumber = 2,                           
        columnWidth = 6,
        varName = 'sex',
        inputReturn = T,
        uiFunction = 'shinyWidgets::pickerInput',
        uiInputs = list(
          label = 'Sex: ',
          choices = sexs,
          selected = sexs[1],
          multiple = F,
          options = shinyWidgets::pickerOptions()
        ),
        updateFunction = 'shinyWidgets::updatePickerInput',
        collapse = F
      ),
      createInputSetting(
        rowNumber = 3,                           
        columnWidth = 12,
        varName = 'history',
        inputReturn = T,
        uiFunction = 'shinyWidgets::pickerInput',
        uiInputs = list(
          label = 'History of: ',
          choices = predictors,
          selected = character(0), #NULL,
          multiple = T,
          options = shinyWidgets::pickerOptions(liveSearch = T)
        ),
        updateFunction = 'shinyWidgets::updatePickerInput',
        collapse = F
      )
    )
  )
  
  predictorReactive <- shiny::reactive(
    {
      # adding a 0 with a real predictor so you get results when inputSelected() is NULL
      data.frame(
        covariateId = c(predictors[1], inputSelected()$ageGroup, inputSelected()$sex, inputSelected()$history),
        value = c(0,rep(1, (length(inputSelected()$ageGroup)+length(inputSelected()$sex)+length(inputSelected()$history))))
      )
    }
  )
  
  predictionTable <- shiny::reactive({
    
    if(!inherits(x = predictorReactive(), what = 'data.frame')){
      data.frame(
        modelId = 1,
        outcomeName = 'None',
        value = 0,
        intercept = 0,
        predictedValue = 0
      )
    }
    
    getPrediction(
      predictorValue = predictorReactive(), 
      targetId = ifelse(is.null(inputSelected()$targetId), options$targets[1], inputSelected()$targetId),
      tarId = ifelse(is.null(inputSelected()$tarId), options$tars[1], inputSelected()$tarId), 
      connectionHandler = connectionHandler,
      schema = schema
    )
  })
  
  # add reactable showing the predictionTable
  output$predictions <- reactable::renderReactable(
    reactable::reactable(
      data = predictionTable(), 
      columns = list(
        modelId = reactable::colDef( 
          show = F
        ),
        outcomeName = reactable::colDef( 
          filterable = TRUE,
          sortable = T,
          header = withTooltip(
            "Outcome", 
            "The risk shows your predicted risk of having this outcome"
          )
        ),
        value = reactable::colDef( 
          show = F
        ),
        intercept = reactable::colDef( 
          show = F
        ),
        #predictedValue = reactable::colDef( 
        #  filterable = F, 
        #  sortable = T, 
        #  format = reactable::colFormat(percent = T),
        #  style = function(value) {
        #    if (value > 0.3) {
        #      color <- "#e00000"
        #    } else if (value < 0.01) {
        #      color <- "#008000"
        #    } else {
        #      color <- "#777"
        #    }
        #    list(color = color, fontWeight = "bold")
        #  },
        #  header = withTooltip(
        #    "Risk", 
        #    "Your risk as a percentage of having the outcome giving your selections"
        #  )
        #),
        
        predictedValue = reactable::colDef(
          filterable = F, 
          sortable = T, 
          format = reactable::colFormat(percent = T),
          header = withTooltip(
            "Risk", 
            "Your risk as a percentage of having the outcome giving your selections"
          ),
          #name = "Risk", 
          align = "left", 
          cell = function(value) {
            width <- paste0(value*100, "%")
            bar_chart(value, width = width, fill = "#e00000", background = "#e1e1e1")
          })
        
      )
    )
  )
  
}