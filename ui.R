library(shiny)

#Directory where the Shiny scripts are located

source("data.R")

shinyUI(fluidPage(
  headerPanel("Correlation between features"),
  sidebarPanel(
    selectInput('institution_names', 'Institution Names', choices=InstitutionNames),
    selectInput('city', 'Cities', choices=Cities),
    selectInput('control', 'Control', choices=Control)
)))

