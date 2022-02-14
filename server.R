#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# ********* #
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # THE RESULT OF RUNNING RENDER PLOT
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
      if(input$variablechoice == 'waiting time'){ x <- faithful[, 2] }
      if(input$variablechoice == 'eruption time'){ x <- faithful[, 1] }
      #x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #     xlab=input$variablechoice)

        ggplot(data=state_pct_od, aes(x=state, y=pct, color=region)) +
          geom_point() +
          scale_color_manual(values=c('#56B4E9', '#56B4E9', '#56B4E9', '#56B4E9', '#FF8333')) +
          facet_wrap(vars(year))
        
        #ggplot(data=state_pct_od, aes(x=state, y=pct, color=region)) +
        #  geom_point() +
        #  scale_color_manual(values=c('#56B4E9', '#56B4E9', '#56B4E9', '#FF8333', '#56B4E9')) +
        #  facet_wrap(vars(year)) +
        #  theme(axis.text.x = element_blank()) +
        #  labs(x = paste( regions$SouthAtlantic[ !is.na(regions$SouthAtlantic) ], collapse='   ' ))
    })

})
