library(shiny)
library(shinyjs)
library(shinydashboard)

#Functions

#Mortality functions
gmort <- function(a2,b2,x){
    exp(a2+(b2*x))
}

gmmort <- function(a2,b2,c,x){
    c + exp(a2+(b2*x))
}

smort <- function(a1,a2,b1,b2,c,x){
    exp(a1-(b1*x)) + c + exp(a2+(b2*x))
}

#Survival functions
gsurv<- function(a2,b2,x){
    exp((a2 / b2) * (1 - exp(b2 * x)))
}

gmsurv<- function(a2,b2,c,x){
    exp(-c * x - (a2 / b2) * (exp(b2 * x) - 1))
}

ssurv<- function(a1,a2,b1,b2,c,x){
    exp((a1 / b1) * (exp(-b1 * x) - 1) - c * x + (a2 / b2) * (1 - exp(b2 * x)))
}

header <- dashboardHeader(title = "Survival and Mortality Curves", titleWidth = 350)

sidebar <- dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
        radioButtons("model", 
                     label = "Model", choices = c("Gompertz", "Gompertz Makeham", "Siler"),
                     selected = "Gompertz"),
        sliderInput("Age",
                    label = "Age range",
                    min = 0, max = 100, value = c(0,20)),
        uiOutput("a1"),
        sliderInput("a2",
                    label = "a2 - Scale parameter",
                    min = 0.001, max = 2.5, value = 0.05, step = 0.01),
        uiOutput("b1"),
        sliderInput("b2",
                    label = "b2 - Senescent rate",
                    min = 0.001, max = 2, value = 0.02, step = 0.001),
        uiOutput("c")
    )
)

body<- dashboardBody(
    box(title="Mortality Curve h(t)", status = "primary", solidHeader = TRUE,
        plotOutput("MortalityCurve")),
    
    box(title="Survival Curve S(t)", status = "warning", solidHeader = TRUE,
        plotOutput("SurvivalCurve"))
)


ui <- dashboardPage(skin="blue",
                    header,
                    sidebar,
                    body
)


# Define server logic required to draw a histogram
server <- function(input, output){
    
    output$MortalityCurve<- renderPlot({
        age<- seq(input$Age[1],input$Age[2])
        
        if (input$model=="Gompertz"){
            mort<- gmort(a2=input$a2, b2=input$b2, x=age)
        } else if (input$model=="Gompertz Makeham"){
            mort<- gmmort(a2=input$a2, b2=input$b2, c=input$c, x=age)
        } else {
            mort<- smort(a1=input$a1, a2=input$a2, b1=input$b1, b2=input$b2, c=input$c, x=age)
        }
        plot(x=age, y=mort, type = "l", col="red", lwd=2)
    })
    
    output$SurvivalCurve<- renderPlot({
        age<- seq(input$Age[1],input$Age[2])
        
        if (input$model=="Gompertz"){
            surv<- gsurv(a2=input$a2, b2=input$b2, x=age)
        } else if (input$model=="Gompertz Makeham"){
            surv<- gmsurv(a2=input$a2, b2=input$b2, c=input$c, x=age)
        } else {
            surv<- ssurv(a1=input$a1, a2=input$a2, b1=input$b1, b2=input$b2, c=input$c, x=age)
        }
        plot(x=age, y=surv, type = "l", col="blue", lwd=2)
    })
    
    
    output$a1 <- renderUI({
        sliderInput("a1",
                    label = "a1 - Mortality rate at birth",
                    min = 0.0001, max = 5, value = 3, step = 0.01)
    })
    
    output$b1 <- renderUI({
        sliderInput("b1",
                    label = "b1 - Rate of mortality decline as a juvenile",
                    min = 0.0001, max = 2, value = 0.02, step = 0.001)
    })
    
    output$c <- renderUI({
        sliderInput("c",
                    label = "c - Age independent mortality",
                    min = 0.0001, max = 5, value = 0.02, step = 0.01)
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