# dynamicQuery

rm(list = ls())

if (!require("shiny") && !require("sqldf") && !require("car")) {
  install.packages(c("shiny", "sqldf", "car"), dependencies = TRUE)
}

library(shiny)
library(sqldf)


library(shiny)

shinyUI(fluidPage(
  titlePanel(h3("Data Select", style = "background-color:gray")),
  hr(),
  div(
    class = "row-fluid", style = "height=350px; background-color:lightgray",
    column(width = 3,
           uiOutput("firstAxe"),
           uiOutput("firstAxeOutput")),
    column(width = 3,
           uiOutput("secondAxe"),
           uiOutput("secondAxeOutput")),
    column(width = 3,
           uiOutput("thirdAxe"),
           uiOutput("thirdAxeOutput")),
    column(width = 3,
           uiOutput("fourthAxe"),
           uiOutput("fourthAxeOutput"))
  ),
  hr(),
  hr(),
  div(class = "row-fluid",
      column(width = 12,
             verbatimTextOutput("mainQuery")))
  ,
  hr(),
  div(class = "row-fluid",
      column(width = 12,
             dataTableOutput("donnees")))
))
