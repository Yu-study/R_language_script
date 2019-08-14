#R shiny is developed by Rstudio
#R shiny is used to design an app online or offline
#We have two inportant scripts ui.R and server.R
# define web application's UI(user interface)
shinyUI(pageWithSidebar(
  # application's name/title
  headerPanel(“hello”),
  sidebarPanel(),
  mainPanel()
))



