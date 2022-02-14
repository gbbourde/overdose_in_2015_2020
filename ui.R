#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Mortality Due To Drug Use"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            radioButtons("variablechoice",
                         "Choice of variable:",
                         choices = c('waiting time','eruption time'),
                         selected = 'waiting time'
                         )
            #radioButtons("regionChoice",
            #             "Choice of region:",
            #             choices = c('waiting time','eruption time'),
            #             selected = 'waiting time'
            #)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
