library(shiny)
library(ggplot2)


bayesian_test <- function(prior_success=1, prior_vol=1, v1_success, v1_vol, v2_success, thresh=0, v2_vol, n.trials=100000){
  
  #parameters for the posterior Beta distibution
  
  v1_beta.A <- v1_success + prior_success
  v1_beta.B <- (v1_vol - v1_success) + (prior_vol - prior_success)
  v2_beta.A <- v2_success + prior_success
  v2_beta.B <- (v2_vol - v2_success) + (prior_vol - prior_success)
  
  #sample from beta distributions
  v1_rbeta <- rbeta(n.trials, v1_beta.A , v1_beta.B)
  v2_rbeta <- rbeta(n.trials, v2_beta.A , v2_beta.B)
  
  #randomly sample from the beta distributions for monte carlo integration of distribution of difference
  v1wins <- sum(v1_rbeta-v2_rbeta > thresh)/n.trials
  v2wins <- sum(v2_rbeta-v1_rbeta > thresh)/n.trials
  practicalEquiv <- 1 - (v1wins + v2wins)
  
  
  #distributions of results
  results_p_dist <- c(practicalEquiv, v1wins, v2wins)
  
  #keep mean for now change to max aposteriori pred later
  map_diff <- abs(mean(v1wins)-mean(v2wins))
  
  
  if(v1wins>v2wins &&  map_diff>thresh){
    winner = 1
  } else if(v2wins>v1wins &&  map_diff>thresh){
    winner = 2
  } else {
    winner = 0
    return(val)
    
  }
  results <- list("winner"=winner, 
                  "v1_beta.A"=v1_beta.A, "v1_beta.B"=v1_beta.B,
                  "v2_beta.A"=v1_beta.A, "v2_beta.B"=v1_beta.B,
                  "dist"=results_p_dist, "diff"=map_diff)
  return(results)
  
}




ui<- fluidPage(
  theme = "bootstrap.css",
  
  #set some styles for the page
  tags$head(tags$style(HTML(".shiny-output-error-validation { color: white; font-size: 30px}"))),
  tags$head(tags$style(HTML("div#myDiv label { width: 20%; }
                           div#myDiv input { display: inline-block; width: 80%;}"))),
  
  #Title and logo
  fluidRow(column(11,align="center", offset=-2, 
                  titlePanel(title=div(img(height="110px",
                                           width="155px",
                                           src='MoneySuperMarket_logo.jpg'), 
                             "Bayesian A/B Test",
                             style = "font-family: 'Arial', cursive;
                                      font-weight: bold; line-height: 2.1; 
                                      font-size: 30px; color: #4F3685;")))),



  #Numeric input boxes
  fluidRow(
            column(3, align="center",
                   textInput(inputId =  "a_conv",
                             label = "Variant A Conversions",
                             width="80%"),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   textInput(inputId =  "b_conv",
                             label = "Variant B Conversions",
                             width="80%"),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   passwordInput(inputId =  "a_vol",
                                 label = "Variant A Volume",
                                 width="80%"),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   passwordInput(inputId =  "b_vol",
                                 label = "Variant B Volume",
                                 width="80%"),
                   style = "font-size: 12px; line-height: 2.1;")
      
  ),
  #Button to initate calculation
  column(12, align="center", actionButton("go", "Calculate Results")),
  
  #Blank row to move the picture down
  fluidRow(class = "myRow1"),
  tags$head(tags$style("
      .myRow1{height:80px;}")),
  
  #Picture display
  fluidRow(
    column(1),
    column(10,align="center", wellPanel(align="center", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",  plotOutput("prob_plot")),
    column(1)
    )
             
))

server  <- function(input, output){
  
  probs <- seq(0,1,length=1000)
  a_dens <- dbeta(probs, 200, 1000)
  b_dens <- dbeta(probs, 250, 1000)
  
  output$prob_plot <-renderPlot({
    ggplot() + geom_line(aes(probs,a_dens))+geom_line(aes(probs,b_dens))
    
  })
  
  #when button pressed check entries are ok and if they are assign list of values to imagen
  imagen <- eventReactive(input$go,{
    validate(
      need(as.single(input$age)>=18 & as.single(input$age)<=90, "Check the age you entered!")
    )
    validate(
      need(as.single(input$children)>=0 & as.single(input$children)<10, "Check your entry for number of children!")
    )
    validate(
      need(as.single(input$income)>0, "Check the income entered!")
    )
    validate(
      need(as.single(input$houseprice)>50000, "Check the house price entered!")
    )
    list(age = as.single(input$age), income=as.single(input$income), children=as.single(input$children), houseprice=as.single(input$houseprice))
  })
  
  #based on list of values choose whcih image to render
  
}
shinyAppDir(".")
shinyApp(ui = ui, server = server)