########################################################################
###### Help Functions
### Axe choice
axeChoice = function(axe){
  if (is.null(axe) || axe == "") {
    return(NULL)
  }else{
    return(axe)
  }
}
### Axe Results
axeResults = function(axeOutput){
  if (is.null(axeOutput) || axeOutput == "") {
    return(NULL)
  }else{
    return(paste(axeOutput, collapse = "','"))
  }
}
### Axe not null or empty 
axeHasVal = function(axe){
  return(!is.null((axe)) && length(axe) > 0 && !identical(axe,""))
}
########################################################################

library(car)

data(Moore)
ContextElements = colnames(Moore)
ContextElements[1] = 'partner_status'
colnames(Moore) = ContextElements

library(shiny)

shinyServer(function(input, output) {
  
  ######### reactive axis
  ###Axe1
  recAxe1c = reactive({
    axeChoice(input$firstAxe)
  })
  recAxe1r = reactive({
    axeResults(input$firstAxeOutput)
  })
  ###Axe2
  recAxe2c = reactive({
    axeChoice(input$secondAxe)
  })
  recAxe2r = reactive({
    axeResults(input$secondAxeOutput)
  })
  ###Axe3
  recAxe3c = reactive({
    axeChoice(input$thirdAxe)
  })
  recAxe3r = reactive({
    axeResults(input$thirdAxeOutput)
  })
  ###Axe4
  recAxe4c = reactive({
    axeChoice(input$fourthAxe)
  })
  recAxe4r = reactive({
    axeResults(input$fourthAxeOutput)
  })
  ############################################################################
  ############################################################################
  ### Query String Building
  queryString <- function(axeName, NumAxe) {
    
    query = "select distinct"
    query = paste(query, axeName, sep = " ")
    query = paste(query, "FROM Moore WHERE 1=1", sep = " ")
    
    if (axeHasVal(recAxe1r())){ 
      query = paste(query, " AND ", recAxe1c() , " IN ('", recAxe1r() , "') ")
    }
    if (NumAxe > 1) {
      if (axeHasVal(recAxe2r())){
        query = paste(query, " AND ", recAxe2c() , " IN ('", recAxe2r() , "') ")
      }
    }
    if (NumAxe > 2) {
      if (axeHasVal(recAxe3r())) {
        query = paste(query, " AND ", recAxe3c() , " IN ('", recAxe3r() , "') ")
      }
    }
    if (NumAxe > 3) {
      if (axeHasVal(recAxe4r())) {
        query = paste(query, " AND ", recAxe4c() , " IN ('", recAxe4r() , "') ")
      }
    }
    
    query = paste(query, "ORDER BY ", axeName)
    return(query)
  }
  
  
  
  
  buildQuery = reactive({
    if (is.null(recAxe1c())) {
      query = ""
    }
    if (axeHasVal(recAxe1c())) {
      query = queryString(axeName = recAxe1c(), NumAxe = 1)
    }
    if (axeHasVal(recAxe2c())) {
      query = queryString(axeName = recAxe2c(), NumAxe = 2)
    }
    if (axeHasVal(recAxe3c())) {
      query = queryString(axeName = recAxe3c(), NumAxe = 3)
    }
    if (axeHasVal(recAxe4c())) {
      query = queryString(axeName = recAxe4c(), NumAxe = 4)
    }
    return(query)
  })
  
  output$mainQuery = renderPrint(buildQuery())
  
  ### Data display
  data = reactive({
    df= Moore
    if (axeHasVal(input$firstAxeOutput)){ 
      query = paste("select * from Moore WHERE ", input$firstAxe , " IN ('", recAxe1r() , "') ")
      df = sqldf(query)
    }
    
    if (axeHasVal(input$secondAxeOutput)){
      query = paste("select * from Moore WHERE ", input$secondAxe , " IN ('", recAxe2r() , "') ")
      df = sqldf(query)
    }
    
    if (axeHasVal(input$thirddAxeOutput)){
      query = paste("select * from Moore WHERE ", input$thirddAxe , " IN ('", recAxe3r() , "') ")
      df = sqldf(query)
    }
    
    if (axeHasVal(input$fourthAxeOutput)){
      query = paste("select * from Moore WHERE ", input$fourthAxe , " IN ('", recAxe4r() , "') ")
      df = sqldf(query)
    }
    
    return(df)
  })
  output$donnees <- renderDataTable(data(), options = list(pageLength=15))  
  ############################################################################
  ############################################################################
  
  
  ###############
  ### first Axis
  output$firstAxe = renderUI({
    selectInput(
      inputId = "firstAxe", label = h4("First Axis"),
      choices = c(Choose = '',ContextElements), multiple = FALSE
    )
  })
  # output first axe
  output$firstAxeOutput = renderUI({
    if (axeHasVal(recAxe1c())) {
      query = queryString(recAxe1c(), 1)
      choicesValues = sqldf(query)
      isolate(
        selectInput(
          inputId = "firstAxeOutput", label = "", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE
        )
      )
    }else{
      selectInput(
        inputId = "firstAxeOutput", label = "", choices = NULL, multiple = TRUE, selectize = FALSE
      )
    }
  })
  ###############
  ### second Axis
  precAxeRes1 = reactive(paste(input$firstAxeOutput, collapse = "','"))
  output$secondAxe = renderUI({
    firstSel = input$firstAxe
    if (is.null(firstSel) || firstSel == '') {
      ContextElements = NULL
    }else{
      ContextElements = setdiff(ContextElements, firstSel)
    }
    selectInput(
      inputId = "secondAxe", label = h4("Second Axis"),
      choices = c(Choose = '',ContextElements), multiple = FALSE
    )
  })
  # output second axe
  output$secondAxeOutput = renderUI({
    if (axeHasVal(recAxe2c())) {
      query = queryString(recAxe2c(), 2)
      choicesValues = sqldf(query)
      selectInput(
        inputId = "secondAxeOutput", label = "", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE
      )
    }else{
      selectInput(
        inputId = "secondAxeOutput", label = "", choices = NULL, multiple = TRUE, selectize = FALSE
      )
    }
  })
  ###############
  ### third Axis
  precAxeRes2 = reactive(paste(input$secondAxeOutput, collapse = "','"))
  output$thirdAxe = renderUI({
    precSel = c(input$firstAxe, input$secondAxe)
    if (is.null(input$secondAxe) || input$secondAxe == '') {
      ContextElements = NULL
    }else{
      ContextElements = setdiff(ContextElements, precSel)
    }
    selectInput(
      inputId = "thirdAxe", label = h4("Third Axis"),
      choices = c(Choose = '',ContextElements), multiple = FALSE
    )
  })
  # output third axe
  output$thirdAxeOutput = renderUI({
    if (axeHasVal(recAxe3c())) {
      query = queryString(recAxe3c(), 3)
      choicesValues = sqldf(query)
      selectInput(
        inputId = "thirdAxeOutput", label = "", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE
      )
    }else{
      selectInput(
        inputId = "thirdAxeOutput", label = "", choices = NULL, multiple = TRUE, selectize = FALSE
      )
    }
  })
  ###############
  ### fourth Axis
  precAxeRes3 = reactive(paste(input$thirdAxeOutput, collapse = "','"))
  output$type3 = renderPrint(precAxeRes3())
  output$fourthAxe = renderUI({
    precSel = c(input$firstAxe, input$secondAxe, input$thirdAxe)
    if (is.null(input$thirdAxe) || input$thirdAxe == '') {
      ContextElements = NULL
    }else{
      ContextElements = setdiff(ContextElements, precSel)
    }
    selectInput(
      inputId = "fourthAxe", label = h4("Fourth Axis"),
      choices = c(Choose = '',ContextElements), multiple = FALSE
    )
  })
  # output fourth axe
  output$fourthAxeOutput = renderUI({
    if (axeHasVal(recAxe4c())) {
      query = queryString(recAxe4c(), 4)
      choicesValues = sqldf(query)
      selectInput(
        inputId = "fourthAxeOutput", label = "", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE
      )
    }else{
      selectInput(
        inputId = "fourthAxeOutput", label = "", choices = NULL, multiple = TRUE, selectize = FALSE
      )
    }
  })

})
