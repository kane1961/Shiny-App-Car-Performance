Motor Trend Car Road Tests - An visualization data app

========================================================
author: Oscar Chamberlain
date: August 13, 2017
autosize: true


App Fundamentals
========================================================

- Data Source used in the app

- UI: User Interface code

- Server code


Data Source used in the app
========================================================

- The data was extracted from the 1974 Motor Trend US magazine, 
and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).
- Source Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391-411. 
- The table below shows an extract from the database
```{r}
head(mtcars)
```

UI code
========================================================

```{r,}
library(shiny)

#' Define UI for application that draws a plot of one car parameter and presents its performance (mpg and hp). A linear fit is done considering the parameters.

ui <- fluidPage(pageWithSidebar(
  headerPanel('Cars Performance'),
    sidebarPanel(
    h2("Visualize the car performance (mpg and hp)"),
    h5("Please select the parameters I to define the x in the plot and parameter II to display the points label."),
    h5("mpg:	 Miles/(US) gallon"), #There are other lines to explain abreviations 
    selectInput('ycol', 'Car performance', names(mtcars[c(1,4)])),
    selectInput('xcol', 'Car parameters I', names(mtcars[c(2,3,5:11)]),
               selected=names(mtcars)[[3]]),
        selectInput('par2','Car parameters II',names(mtcars[c(2,9:11)]),)),
    mainPanel(plotOutput('plot1') )))

```

Server code
========================================================

```{r,}
# Define server logic required to draw a plot 
server <- function(input, output, session) 
 {  # Combine the selected variables into a new data frame
 
  selectedData <- reactive( { mtcars[, c(input$xcol, input$ycol)]  })
  par2 <- reactive({mtcars[,c(input$par2)] })
    fit<-reactive({lm(mtcars[,c(input$ycol)]~mtcars[,c(input$xcol)] )})
    output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      par(mar = c(5.1, 4.1, 0, 1))
  plot(selectedData(), pch = 20, cex = 5, col=par2() )
  legend("topright", pch=20, col=unique(par2()), legend = order( unique( par2())))
  abline(fit(), lwd=2)  })
}

```
