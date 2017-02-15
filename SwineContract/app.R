#==============================================================================#
# Cash Flow
#==============================================================================#

library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = "bootstrap-theme.css",
  
  #----------------------------------------#
  # Header
  #----------------------------------------#
  
  # Add Title to program
  # tags$h1("Contract Cash Flow"),
  HTML('
    <h1>
    Contract Cash Flow
    </h1>
    '),
  
  # Add IA State Logo to program
  # tags$img(height=200, width=200, src="istate_logo.jpg"),
  HTML('
    <img src="istate_logo.jpg" alt="IA State Logo" style="width:200px;height:200px;"> 
    '), 
  
  # Write a short description
  # tags$p("This Shiny App is used to calculate the profit for a contract
  #   producer."),
  HTML('
  <ul>
  <li> This is a Shiny App to calculate returns, costs, and profit for
  a contract producer. </li>
  </ol>
  '),
  
  # Line break
  tags$hr(),
  
  #----------------------------------------#
  # Begin Tabs
  #----------------------------------------#
  
  # Start panel subset
  #tabsetPanel(
    #tabPanel("Inputs",
  
  tags$h2("Inputs"),
  
    tags$h3("General Info"),
  
       wellPanel(
        fluidRow(
         column(4, sliderInput(inputId="years",
                     label="Number of years (useful life):",
                     min = 5,
                     max = 35,
                     value = 24, step=1)),
         column(4, sliderInput(inputId="capacity",
                     label="Capacity of barn:",
                     min = 100,
                     max = 20000,
                     value = 2400, step=100)),
         # sliderInput(inputId="turns",
         #             label="Turns per year:",
         #             min = 1,
         #             max = 3,
         #             value = 1, step=1),
         column(4, sliderInput(inputId="payment",
                     label="Payment per head:",
                     min = 30,
                     max = 70,
                     value = 43, step=1))),
  
        fluidRow(
         column(4, sliderInput(inputId="contractLength",
                     label="Length of contract:",
                     min = 1,
                     max = 20,
                     value = 15, step=1)),
         column(4, sliderInput(inputId="taxRate",
                     label="Tax Rate (%):",
                     min = 0,
                     max = 50,
                     value = 15, step=1)))
       ),
  
      # Line Break
      tags$hr(),
  
    tags$h3("Building costs"),
  
       wellPanel(
        fluidRow(
         column(4, numericInput(inputId="landCost",
                     label="Land Cost:",
                     min = 0,
                     max = 100000,
                     value = 0, step=500)),
         column(4, numericInput(inputId="sitePrep",
                     label="Site Preparation Cost:",
                     min = 0,
                     max = 100000,
                     value = 60000, step=500))),
        fluidRow(
         column(4, numericInput(inputId="buildingCost",
                     label="Building Cost:",
                     min = 0,
                     max = 1000000,
                     value = 316000, step=1000)),
         column(4, numericInput(inputId="equipmentCost",
                     label="Equipment Cost:",
                     min = 0,
                     max = 1000000,
                     value = 258000, step=1000)))
       ),
  
      # Line Break
      tags$hr(),
  
    tags$h3("Loan Terms"),
  
       wellPanel(
        fluidRow(
         column(4, numericInput(inputId="equityContribution",
                     label="Equity Contribution:",
                     min = 0,
                     max = 100000,
                     value = 0, step=1000)),
         column(4, sliderInput(inputId="loanLength",
                     label="Loan Length (years):",
                     min = 1,
                     max = 20,
                     value = 12, step=1)),
         column(4, sliderInput(inputId="loanTaxRate",
                     label="Loan Tax Rate (%):",
                     min = 0,
                     max = 30,
                     value = 4, step=1)))
       ),
  
      # Line Break
      tags$hr(),
  
    tags$h3("Variable costs"),
  
       wellPanel(
        fluidRow(
         column(4, sliderInput(inputId="maintenance",
                     label="Maintenance costs:",
                     min = 0,
                     max = 10000,
                     value = 2400, step=200)),
         column(4, sliderInput(inputId="utilities",
                     label="Utility costs:",
                     min = 0,
                     max = 30000,
                     value = 16000, step=500)),
         column(4, sliderInput(inputId="insurance",
                     label="Insurance costs:",
                     min = 0,
                     max = 10000,
                     value = 3200, step=200))),
  
        fluidRow(
         column(4, sliderInput(inputId="taxes",
                     label="Tax costs:",
                     min = 0,
                     max = 10000,
                     value = 2100, step=100)),
         column(4, sliderInput(inputId="waste",
                     label="Waste Management costs:",
                     min = 0,
                     max = 20000,
                     value = 11000, step=500)),
         column(4, sliderInput(inputId="fertilizer",
                     label="Fertilizer value per year:",
                     min = 0,
                     max = 50000,
                     value = 25000, step=500))),
        fluidRow(
         column(4, sliderInput(inputId="wageRate",
                     label="Wage Rate per Hour:",
                     min = 0,
                     max = 30.00,
                     value = 0, step=0.50)),
         column(4, sliderInput(inputId="laborHours",
                     label="Labor per pig (hours):",
                     min = 0,
                     max = 1,
                     value = 0.40, step=0.1)))
         
       ),
    #),
  
  # tabPanel("Outputs",
  
    # Line Break
    tags$hr(),
  
  tags$h2("Output"),
  
    # header for Profit Plot
    tags$h3("Profit Plot"),
  
      # Show a plot of the profit
      fluidRow(column(8, offset=2, 
        plotOutput("profitPlot"))),
  
    # Line Break
    tags$hr(),
  
    # header for dataset output
    tags$h3("Dataset from Inputs"),
  
      # Print the Data Frame calculated from inputs
      dataTableOutput("dataoutput")
  
    #) # END tabPanel()
  #) # END tabsetPanel()
) # END fluidPage()











#==============================================================================#
# Server
#==============================================================================#

# Plot profit for cash flow example
server <- function(input, output) {
  
   data <- reactive({
      
      # get 
      vec.year <- seq(1, input$years, by=1)
      
      # create dataset
      data <- data.frame(Year=vec.year)
      
      # total costs
      data$Maintanance <- input$maintenance
      data$Utilities   <- input$utilities
      data$Insurance   <- input$insurance
      data$Tax         <- input$taxes
      data$Waste       <- input$waste
      data$Labor       <- input$laborHours * input$capacity * input$wageRate

      PMT <- function(rate, nper,pv, fv=0, type=0){
                    pmt = ifelse(rate!=0,
              (rate*(fv+pv*(1+ rate)^nper))/((1+rate*type)*(1-(1+ rate)^nper)),
               (-1*(fv+pv)/nper )
      )

      return(pmt)
      }
      IPMT <- function(rate, per, nper, pv, fv=0, type=0){
        ipmt = -( ((1+rate)^(per-1)) * (pv*rate + PMT(rate, nper,pv, fv=0, type=0)) - 
                  PMT(rate, nper,pv, fv=0, type=0))
         return(ipmt)
      }
      PPMT <- function(rate, per, nper, pv, fv=0, type=0){
                 ppmt = PMT(rate, nper,pv, fv=0, type=0) - 
                        IPMT(rate, per, nper, pv, fv=0, type=0)
      return(ppmt)
      }
      
      # get loan amount
      loanTotal <- input$buildingCost + input$equipmentCost + input$landCost +
                              input$sitePrep - 
                              input$equityContribution
      
      # calculate 
      data$PricipalOnLoan <- PPMT(input$loanTaxRate/100, data$Year, input$loanLength, -loanTotal)
      data$InterestOnLoan <- PMT(input$loanTaxRate/100, input$loanLength, -loanTotal)
      
      # set to 0 if after loan runs out
      data$PricipalOnLoan[data$Year > input$loanLength] <- 0
      data$InterestOnLoan[data$Year > input$loanLength] <- 0
      
      # fertilizer value
      data$Fertilizer <- input$fertilizer
      data$Fertilizer[data$Year==1] <- 0
      
      
      
      # total revenue
      data$Revenue <- input$capacity * input$payment + data$Fertilizer
      
      # set to 0 after contract is over
      data$Revenue[data$Year > input$contractLength] <- 0
      
      # get total costs
      data$TotalCosts <- with(data, Maintanance + Utilities + Insurance + 
                          Tax + Waste + Labor + 
                          PricipalOnLoan + InterestOnLoan)
      
      # get profit
      data$Profit <- data$Revenue - data$TotalCosts
      
      data <- data
      
      # get min and max of Profit
      # minProfit <- min(data$Profit)
      # maxProfit <- max(data$Profit)
      
      # sequence for Profit
      #seqProfit <- c(seq(minProfit, 0, by=20000), 0, seq(0, maxProfit, by=20000))
      
   })
   
   # Add the Profit Plot
   output$profitPlot <- renderPlot({

      # plot Profit by year
      ggplot(data(), aes(x=as.factor(Year), y=Profit)) +
        geom_bar(stat="identity", color="grey70", fill="steelblue") +
        #scale_y_continuous(breaks=seqProfit) +
        ggtitle("Profit by Year") +
        xlab("Year") +
        ylab("Profit ($)") +
        theme(text=element_text(size=18))
      
   })

   # Add Data table with all the values calculated above 
   output$dataoutput <- renderDataTable({
     data()
   })
}











#==============================================================================#
# Shiny App
#==============================================================================#

# Run the application 
shinyApp(ui = ui, server = server)











