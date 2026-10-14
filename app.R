library(shiny)

ui <- fluidPage(
  titlePanel("One-Way ANOVA Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Population Parameters"),
      sliderInput("mu1", "Group 1 Mean", min = 0, max = 100, value = 50),
      sliderInput("mu2", "Group 2 Mean", min = 0, max = 100, value = 50),
      sliderInput("mu3", "Group 3 Mean", min = 0, max = 100, value = 50),
      sliderInput("sd", "Common Standard Deviation", min = 1, max = 20, value = 4),
      sliderInput("n", "Sample Size per Group", min = 10, max = 100, value = 20),
      actionButton("regen", "Generate New Sample")
    ),
    
    mainPanel(
      plotOutput("boxplot"),
      #plotOutput("meanplot"),
      verbatimTextOutput("anova")
    )
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    input$regen
    isolate({
      y1 <- rnorm(input$n, input$mu1, input$sd)
      y2 <- rnorm(input$n, input$mu2, input$sd)
      y3 <- rnorm(input$n, input$mu3, input$sd)
      
      data.frame(
        y = c(y1,y2,y3),
        group = factor(rep(c("Group 1","Group 2","Group 3"), each = input$n))
      )
    })
  })
  
  output$boxplot <- renderPlot({
    boxplot(y ~ group, data = dat(), col = "skyblue", main = "Sample Data")
  })

  #  output$meanplot <- renderPlot({
  #    d <- dat()
  #    means <- tapply(d$y, d$group, mean)
  #    ses <- tapply(d$y, d$group,
  #                  function(x) sd(x)/sqrt(length(x)))
  #    plot( 1:3, means, ylim = range(means - 2*ses, means + 2*ses), xaxt = "n", 
  #          xlab = "Group", ylab = "Mean", pch = 19, cex = 1.5)
  #    axis(1, at = 1:3, labels = names(means))
  #    arrows( 1:3, means - 2*ses, 1:3, means + 2*ses, angle = 90, code = 3, length = 0.08)
  #    abline(h = mean(d$y), lty = 2, col = "red")
  #  })
  
  output$anova <- renderPrint({
    fit <- aov(y ~ group, data = dat())
    summary(fit)
  })
}

shinyApp(ui, server)