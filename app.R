library(shiny)
library(shinyjs)
library(shinydashboard)
library(nimble)

#Functions
source("Dist_Gompertz.R")
source("Dist_GompertzMakeham.R")
source("Dist_Siler.R")

#Mortality functions
gmort <- function(pdf,surv){
  pdf/surv
  }

gmmort <- function(a2,b2,c,x){
  c + exp(a2+(b2*x))
}

smort <- function(a1,a2,b1,b2,c,x){
  exp(a1-(b1*x)) + c + exp(a2+(b2*x))
}


header <- dashboardHeader(title = "Survival and Mortality Curves", titleWidth = 350)

sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    radioButtons("model", 
                 label = "Model", choices = c("Gompertz", "Gompertz Makeham", "Siler"),
                 selected = "Siler"),
    sliderInput("Age",
                label = "Age range",
                min = 0, max = 100, value = c(0,50)),
    uiOutput("a1"),
    sliderInput("a2",
                label = "a2 - Scale parameter",
                min = 0, max = 2, value = 0.05, step = 0.0001),
    uiOutput("b1"),
    sliderInput("b2",
                label = "b2 - Senescent rate",
                min = 0, max = 0.15, value = 0.05, step = 0.0001),
    uiOutput("c")
  )
)

body<- dashboardBody(
    box(title="Hazard rate h(t)", status = "primary", solidHeader = TRUE,
        plotOutput("hazardCurve")),
    
    box(title="Survival Curve S(t)", status = "warning", solidHeader = TRUE,
        plotOutput("SurvivalCurve"))
    ,
    box(title="Probability density function f(t)", status = "info", solidHeader = TRUE,
        plotOutput("pdfCurve"))
  )


ui <- dashboardPage(skin="blue",
                    header,
                    sidebar,
                    body
)


# Define server logic required to draw a histogram
server <- function(input, output){
  
  age<- reactive(seq(input$Age[1],input$Age[2]))
  
  model<-reactive(input$model)
  
  surv <- reactive(
    if (model()=="Gompertz"){
      surv<- pgompz(q=age(), a=input$a2, b=input$b2, lower.tail = FALSE)
    } else if (model()=="Gompertz Makeham"){
      surv<- pgompzMake(q=age(), a=input$a2, b=input$b2, c=input$c, lower.tail = FALSE)
    } else {
      surv<- psiler(q=age(), a1=input$a1, a2=input$a2, b1=input$b1, b2=input$b2, c=input$c, lower.tail = FALSE)
    }
  )
  
  pdf <- reactive(
    if (model()=="Gompertz"){
      pdf<- dgompz(x=age(), a=input$a2, b=input$b2)
    } else if (model()=="Gompertz Makeham"){
      pdf<- dgompzMake(x=age(), a=input$a2, b=input$b2, c=input$c)
    } else {
      pdf<- dsiler(x=age(), a1=input$a1, a2=input$a2, b1=input$b1, b2=input$b2, c=input$c)
    }
  )
  
  mort <- reactive(
    pdf()/surv()
  )
  
  output$hazardCurve<- renderPlot({
    plot(x=age(), y=mort(), type = "l", col="red", lwd=2)
  })
  
  output$SurvivalCurve<- renderPlot({
    plot(x=age(), y=surv(), type = "l", col="blue", lwd=2)
  })
  
  output$pdfCurve<- renderPlot({
    plot(x=age(), y=pdf(), type = "l", col="blue", lwd=2)
  })
 
  
  output$a1 <- renderUI({
  sliderInput("a1",
              label = "a1 - Mortality rate at birth",
              min = 0.0024, max = 0.1533, value = 0.0183, step = 0.0001)
  })
  
  exp(-4)
  output$b1 <- renderUI({
  sliderInput("b1",
              label = "b1 - Rate of mortality decline as a juvenile",
              min = 0.0001, max = 2, value = 0.5, step = 0.0001)
  })
  
  output$c <- renderUI({
  sliderInput("c",
                label = "c - Age independent mortality",
                min = 0, max = 0.12, value = 0.02, step = 0.0001)
  })
  
  observe(if (input$model=="Gompertz"){
    shinyjs::hide("a1")
    shinyjs::hide("b1")
    shinyjs::hide("c")
  }else if (input$model=="Gompertz Makeham"){
    shinyjs::hide("a1")
    shinyjs::hide("b1")
    shinyjs::show("c")
  }else{
    shinyjs::show("a1")
    shinyjs::show("b1")
    shinyjs::show("c")
  }
)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)