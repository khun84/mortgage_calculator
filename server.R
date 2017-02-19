library(shiny)
library(FinCal)
library(ggplot2)
library(magrittr)

shinyServer(function(input, output){
  
  loan = reactive((1-input$deposit/100)*totalprice())
  
  totalprice = reactive({input$unitprice * input$area})
  
  # holding period in month
  holdingperiod = reactive({(input$sell_period - 1) * 12 + 1})
  
  tenureperiod = reactive(({input$tenure_yr * 12}))
  
  totalperiod = reactive({ifelse(tenureperiod() >= holdingperiod(), tenureperiod(), holdingperiod())})

  rental_mth = reactive({input$rental_yr/12})
  
  rental_start = reactive({input$rental_period[1]})
  
  rental_end = reactive({input$rental_period[2]})
  

  payment = reactive({pmt(input$interest_yr/100/12, input$tenure_yr*12, loan(),0, 0)})

  # vector of loan tenure in month
  tenure_month = reactive({seq(0,tenureperiod())})
  
  # vector of holding period in month
  holding_month = reactive({seq(0,holdingperiod())})
  
  # vector of total period in month
  total_month = reactive({seq(0, totalperiod())})

  cf_buy = reactive({
    x = rep(0,length(total_month()))
    x[1] = totalprice()
    x
  })
  
  cf_payment = reactive({
    c(0,rep(payment(),totalperiod()))
    })
  
  cf_cum_payment = reactive({
    cumsum(cf_payment())
  })
  
  cf_interest = reactive({
    
    x = rep(0, totalperiod()+1)
    loan_1 = loan()
    payment_1 = payment()
    # let principal <= 0
    principal = 0
    int = input$interest_yr/12/100
    
    for (i in seq(2, totalperiod()+1)) {
      interest = -(loan_1 + principal) * int
      x[i] = interest
      principal = (payment_1 - interest) + principal
    }
    
    return(x)
  })
  
  cf_principal = reactive({
    cf_payment() - cf_interest()
  })
  
  cf_cum_principal = reactive({cumsum(cf_principal())})
  
  # principal balance vector
  cf_principal_bal = reactive({
    x = rep(0,length(total_month()))
    x = -(loan() - (cf_cum_principal() %>% abs))
    x
  })
  
  # helper function to generate rental increment cf vector
  f_rental_inc = function(idx,increment, start,end, gap = 24){
    if (idx != 0 && idx >= start && idx <= end) {
      return(floor((idx-start)/gap)*increment)
    }
    else {
      return(0)
    }
  }
  
  # helper function to generate rental cf vector
  f_rental_mth = function(idx, start, end, rental){
    if (idx >= start && idx <= end) {
      return(rental)
    }
    else {
      return(0)
    }
    
  }
  
  # generate rental cf vector
  cf_rental = reactive({
    
    # if there is any rental income
    if (input$inputrent == 'With Rent') {
      mth = total_month()
      start_1 = ifelse(rental_start()==1, 1, rental_start()*12+2)
      end_1 = ifelse(rental_start() == rental_end(), start_1 + 11, rental_end()*12)
      # base rental
      base = sapply(mth,
                    f_rental_mth,
                    start = start_1,
                    end = end_1,
                    rental = rental_mth())
      # rental increment
      delta = sapply(mth,
                     f_rental_inc,
                     increment = input$rental_inc_2yr,
                     start = start_1,
                     end = end_1,
                     gap = 24)

     return(base + delta)
  }
  else {
    # if not rental income, return 0 vector
    return({rep(0, length(total_month()))})
  }
  })
  
  # generate selling price vector
  cf_price = reactive({
    cf <- rep(0,length(total_month()))
    cf[total_month() == holdingperiod()] = (input$unitprice + input$sell_period * input$price_inc_yr) * input$area
    return(cf)
  })

  # generate net cash flow vector
  net_cf = reactive({
    cf_principal_bal_net = cf_principal_bal()
    cf_principal_bal_net[total_month() != holdingperiod()] = 0
    net = cf_price()+cf_rental()+cf_payment()+cf_principal_bal_net
    net[total_month() > holdingperiod()] = NA
    net[1] = -input$deposit/100*totalprice()
    
    
    net
  })
  
  output$IRR = renderValueBox({
    myirr = irr(net_cf()[total_month() <= holdingperiod()])*100*12
    valueBox(
      paste0(formatC(myirr, digits = 2, format = 'f'),'%')
      ,color = ifelse(myirr>=4, 'green','red')
      ,subtitle = 'Internal Return Rate'
    )
  })
  
  output$inflow_onsale = renderValueBox({
    inflow = net_cf()[total_month() == holdingperiod()]
    valueBox(
      paste0('$', formatC(inflow, digits = 0, format = 'f', big.mark = ','))
      ,color = ifelse(inflow>=0, 'green', 'red')
      ,subtitle = 'Net cash inflow upon sale'
    )
  })
  
  output$total_interest_paid = renderValueBox({
    interest = sum(cf_interest()[total_month() <= holdingperiod()]) %>% abs()
    valueBox(
      paste0('$', formatC(interest, digits = 0, format = 'f', big.mark = ','))
      ,color = ifelse(interest>=0, 'green', 'red')
      ,subtitle = 'Total interest paid'
    )
  })
  
  output$selling_price = renderValueBox({
    price = sum(cf_price())
    valueBox(
      paste0('$', formatC(price, digits = 0, format = 'f', big.mark = ','))
      ,color = ifelse(price > totalprice(), 'green', 'red')
      ,subtitle = 'Selling price'
    )
  })
  
  output$mortgage_table <- renderTable({

    data.frame(mth = total_month()
               ,payment = cf_payment()
               ,rental = cf_rental()
               ,sell = cf_price()
               ,cum_payment = cf_cum_payment()
               ,interest = cf_interest()
               ,principal = cf_principal()
               ,cum_principal = cf_cum_principal()
               ,principal_balance = cf_principal_bal()
               ,net = net_cf()
               )
    
  })
  
  
  
  
  
  # output$plot1 <- renderPlot({
  #   set.seed(123)
  #   number_of_points <- input$numeric
  #   minX <- input$sliderX[1]
  #   maxX <- input$sliderX[2]
  #   minY <- input$sliderY[1]
  #   maxY <- input$sliderY[2]
  #   dataX <- runif(number_of_points, minX, maxX)
  #   dataY <- runif(number_of_points, minY, maxY)
  #   xlab <- ifelse(input$show_xlab, 'X Axis', '')
  #   ylab <- ifelse(input$show_ylab, 'Y Axis', '')
  #   title <- ifelse(input$show_title, 'Title','')
  # ggplot() + geom_point(aes(x = dataX, y = dataY)) + labs(title = title, x = xlab, y = ylab) + theme_bw()
  # })
  
})