library(shiny)
library(FinCal)
library(shinydashboard)
library(magrittr)

initial_int = 4
initial_unitprice = 5000
initial_area = 100
initial_tenure = 30
initial_rent = -pmt(initial_int/100/12, initial_tenure*12, initial_unitprice*initial_area,0,type = 1)/1.5*12
initial_rent = formatC(initial_rent, digits = 2, format = 'g') %>% as.numeric()

dashboardPage(
  dashboardHeader(title = 'Property Investment IRR'),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = 'Documentation', tabName = 'documentation'),
        menuItem(text = 'Dashboard', tabName = 'dashboard')
      ),
      sliderInput('deposit', 'Deposit (%)', min = 0, max = 100, value = 10,step = 5),
      numericInput('interest_yr', 'Annual Interest (%):', value = initial_int, min = 0.5, max = 10, step = .01),
      numericInput('area', 'Floor area (m2):', value = initial_area, min=30, max = 2000, step = 10),
      numericInput('tenure_yr', 'Mortgage tenure (yr)', value = initial_tenure, min = 10, max = 40, step = 1),
      numericInput('unitprice', 'Unit price ($/m2):',value = initial_unitprice, step = 100),
      selectInput(
      "inputrent", "Rent",
      c("With Rent",
        "Without Rent")),
      conditionalPanel(
        condition = "input.inputrent == 'With Rent'",
        numericInput('rental_yr', 'Annual rent', value = initial_rent, min = 500*12, max = 20000*12),
        sliderInput('rental_period', 'Rental period in yr (from - to)', min = 1, max = 50, value = c(1,20)),
        numericInput('rental_inc_2yr', 'Rental increment/2 yr', value = 200, step = 100)
      ),
      sliderInput('sell_period', 'When to sell (year)', value = 5, min = 2, max = 100),
      sliderInput('price_inc_yr', 'Property unit price increment/yr', value = 50, min = 10, max = 1000)
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'documentation',
                h2('Documentation'),
                  tags$p('This mortgage IRR (internal return rate) calculator allows you to calculate 
                        a baseline IRR for your property investment.'),
                  tags$p('There is some simple investment parameter in the sidebar menu that you can play around to evaluate
                         its impact on your IRR.'),
                  tags$p('The app will generate the necessary cash flow based on the selected parameter
                         and calculate the annual IRR using the irr function from FinCal package.'),
                  tags$p('Note that negative value in the mortgage spreadsheet means outflow cashflow and vice versa.'),
                  tags$p('The source code of this app resides in ', 
                         tags$a(href='https://github.com/khun84/mortgage_calculator',
                                'https://github.com/khun84/mortgage_calculator'))
                ),
        tabItem(tabName = 'dashboard',
          fluidRow(
            valueBoxOutput('IRR')
            ,valueBoxOutput('inflow_onsale')
            ,valueBoxOutput('selling_price')
            ,valueBoxOutput('total_interest_paid')
          )
          ,fluidRow(
            shiny::tableOutput('mortgage_table')
          )
        )
      )
   )
  
)