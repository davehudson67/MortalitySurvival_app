library(shiny)
library(shinyjs)

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
    exp(((exp(a2))/b2)*(1-exp(b2*x)))
}

gmsurv<- function(a2,b2,c,x){
    exp(- c*x + ((exp(a2))/b2)*(1-exp(b2*x)))
}

ssurv<- function(a1,a2,b1,b2,c,x){
    exp(((exp(a1))/b1)*(exp(-b1*x)-1) - c*x + ((exp(a2))/b2)*(1-exp(b2*x)))
}

ui <- fluidPage(
    useShinyjs(),
    titlePanel(h1("Exploring Mortality models")),
    sidebarLayout(
        sidebarPanel(
            radioButtons("model", "Model",
                          choices = list("Gompertz"=1,"Gompertz Makeham"=2,"Siler"=3), selected=1),
            helpText("Adjust the parameter values to see the effect on Mortality rate and Survival probability using the Siler function"),
            id = "form",
            sliderInput("a1",
                        label = "a1 - Mortality rate at birth",
                        min = -10, max = 10, value = -3.2, step = 0.1),
            sliderInput("a2",
                        label = "a2 - Mortality rate at beginning of senesence",
                        min = -10, max = 10, value = -3.5, step = 0.1),
            sliderInput("b1",
                        label = "b1 - Rate of mortality decline as a juvenile",
                        min = 0.001, max = 10, value = 0.02, step = 0.001),
            sliderInput("b2",
                        label = "b2 - Senescent increase in mortality",
                        min = 0.001, max = 10, value = 0.01, step = 0.001),
            sliderInput("c",
                        label = "c - Age independent mortality",
                        min = -5, max = 5, value = 0.02, step = 0.01),
            sliderInput("Age",
                        label = "Age range",
                        min = 0, max = 100, value = c(0,80)),
            actionButton("resetAll",
                         label = "Reset values"),
                         
            
        ),
        mainPanel(
            
            plotOutput("Mortality"),
            plotOutput("Survival")
            
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    if (input$model==1){
        hide(input$a1)
    }
    
    output$Mortality<-
    renderPlot({
        x<- seq(input$Age[1]:input$Age[2])
        if (input$model==1){
                mort<-gmort(input$a2,input$b2,x)
        }
        if (input$model==2){
                mort<-gmmort(input$a2, input$b2, input$c,x)
        }else {
                mort<-smort(input$a1, input$a2, input$b1, input$b2, input$c,x)
        }
        
        #Plot mortality trajectory
        plot(mort, type = 'l', xlab = "Age", ylab = "Mortality rate", xlim = c(input$Age[1],input$Age[2]),
            lwd = 2)
    })
    
    output$Survival<-
    renderPlot({
        x<- seq(input$Age[1]:input$Age[2])
        if (input$model==1){
            surv<-gsurv(input$a2,input$b2,x)
        }
        if (input$model==2){
            surv<-gmsurv(input$a2, input$b2, input$c,x)
        }else {
            surv<-ssurv(input$a1, input$a2, input$b1, input$b2, input$c,x)
        }
        
        plot(surv, type = "l", xlab = "Age", ylab = "Survival", ylim = c(0,1), xlim = c(input$Age[1],input$Age[2]),
                 lwd = 2)
        })
    observeEvent(input$resetAll,{
        reset("form")
    })
    
    
    }


# Run the application 
shinyApp(ui = ui, server = server)