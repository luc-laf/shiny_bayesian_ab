library(shiny)
library(ggplot2)

# SECTION 1: define the bayesian test procedure #########################################################################################

bayesian_test <- function(prior1_success=1, prior1_vol=2,
                          prior2_success=1, prior2_vol=2,
                          v1_success, v1_vol, 
                          v2_success, v2_vol, 
                          thresh=0, n.trials=100000){
  
  #parameters for the posterior Beta distibution
  
  v1_beta.A <- v1_success + prior1_success
  v1_beta.B <- (v1_vol - v1_success) + (prior1_vol - prior1_success)
  v2_beta.A <- v2_success + prior2_success
  v2_beta.B <- (v2_vol - v2_success) + (prior2_vol - prior2_success)
  
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
  map_diff <- abs((v1_beta.A-1)/(v1_beta.A+v1_beta.B-2)-(v2_beta.A-1)/(v2_beta.A+v2_beta.B-2))
  
  
  if(v1wins>v2wins &&  map_diff>thresh){
    winner = 1
  } else if(v2wins>v1wins &&  map_diff>thresh){
    winner = 2
  } else {
    winner = 0
  }
  results <- list("winner"=winner, 
                  "v1_beta.A"=v1_beta.A, "v1_beta.B"=v1_beta.B,
                  "v2_beta.A"=v2_beta.A, "v2_beta.B"=v2_beta.B,
                  "dist"=results_p_dist, "diff"=map_diff)
  return(results)
  
}


# SECTION 2: shiny ui #########################################################################################

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
            column(3, align="center", wellPanel(align="left", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",
                   numericInput(inputId =  "a_conv",
                             label = "Variant A Conversions",
                             width="80%",
                             value=10)),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",wellPanel(align="left", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",
                   numericInput(inputId =  "a_vol",
                                label = "Variant A Volume", 
                                width="80%",
                                value=20)),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center", wellPanel(align="left", style = "overflow: hidden; background-color:  #4F3685; max-width:600px",
                   numericInput(inputId =  "b_conv",
                             label = "Variant B Conversions",
                             width="80%",
                             value=5)),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center", wellPanel(align="left", style = "overflow: hidden; background-color:  #4F3685; max-width:600px",
                   numericInput(inputId =  "b_vol",
                                 label = "Variant B Volume",
                                 width="80%",
                                value=20)),
                   style = "font-size: 12px; line-height: 2.1;")
      
  ),
  
  
  fluidRow(column(3, wellPanel(align="center", style = "overflow: hidden; border-color:  #4F3685; max-width:600px",  textOutput("result"))),
           column(3, wellPanel(align="center", style = "overflow: hidden; border-color:  #4F3685; max-width:600px",  textOutput("difference"))),
           column(6, align="center"),
           style = "font-family: 'Arial', cursive;
                    font-weight: bold; line-height: 2.1;" ),
  
  #Button to initate calculation
  column(12, align="center", actionButton("go", "Calculate Results")),

  
  #Blank row to move the picture down
  fluidRow(class = "myRow1"),
  tags$head(tags$style("
      .myRow1{height:80px;}")),
  
  
  
  #Picture display
  fluidRow(
    column(4,align="center", 
           wellPanel(align="center", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",  
                     titlePanel("Prior"), plotOutput("prior_plot"))),
    column(4,align="center", 
           wellPanel(align="center", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",  
                     titlePanel("Posterior"), plotOutput("prob_plot1"))),
    column(4,align="center", 
           wellPanel(align="center", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",  
                     titlePanel("Results"), plotOutput("prob_plot2")))
  ),
  #Extra parameter tuning
  fluidRow(column(11,align="center", offset=-2, 
                  titlePanel("Advanced Parameter Tuning"),
                  style = "font-family: 'Arial', cursive;
                             font-weight: bold; line-height: 2.1; 
                             font-size: 30px; color: #4F3685;")),
  
  fluidRow(
    column(3, align="center", numericInput(inputId = "prior_a_conv",
                                                     label = "Variant A Prior Conversions",
                                                     width="80%",
                                                     value=1),
           style = "font-size: 12px; line-height: 2.1;"),
    column(3, align="center", numericInput(inputId = "prior_a_vol",
                                                     label = "Variant A Prior Volume", 
                                                     width="80%",
                                                     value=2),
           style = "font-size: 12px; line-height: 2.1;"),
    column(3, align="center", numericInput(inputId = "prior_b_conv",
                                                     label = "Variant B Prior Conversions",
                                                     width="80%",
                                                     value=1),
           style = "font-size: 12px; line-height: 2.1;"),
    column(3, align="center",numericInput(inputId =  "prior_b_vol",
                                                     label = "Variant B Prior Volume",
                                                     width="80%",
                                                     value=2),
           style = "font-size: 12px; line-height: 2.1;")
  ),
  
  fluidRow(
    column(3, align="center", numericInput(inputId = "rope",
                                           label = "ROPE",
                                           width="80%",
                                           value=0)
           )
    )
)

# SECTION 2: shiny server #########################################################################################

server  <- function(input, output){
  
  #Calculate test results based on input
  test.output <-eventReactive(input$go,{
    bayesian_test(prior1_success = input$prior_a_conv, prior1_vol = input$prior_a_vol,
                  prior2_success = input$prior_b_conv, prior2_vol = input$prior_b_vol,
                  v1_success =  input$a_conv, v1_vol = input$a_vol,
                  v2_success = input$b_conv, v2_vol = input$b_vol,
                  thresh = input$rope)
    
  })
  #plot prior distributions
  output$prior_plot <-renderPlot({
 
    rate <- seq(0,1,length=1000)
    prior_a_dens <- dbeta(rate,  input$prior_a_conv,  input$prior_a_vol -  input$prior_a_conv)
    prior_b_dens <- dbeta(rate,  input$prior_b_conv,  input$prior_b_vol -  input$prior_b_conv)
    ggplot() + geom_line(aes(rate,prior_a_dens, color="A Prior"))+geom_line(aes(rate,prior_b_dens, color="B Prior")) +
      scale_color_manual(name='', values = c('A Prior'='#00AEEF', 'B Prior'='#4F3685')) + 
      labs(x="rate", y="probability density")
    
  })
  #plot test result probabilities
  output$prob_plot2 <-renderPlot({
    
    probability <- test.output()$dist
    if(probability[1]==0){
      groups <- c( "A Wins", "B Wins")
      probability <- probability[2:3]
    }else{
      groups <- c("Draw", "A Wins", "B Wins")
    }
    
    ggplot() + geom_bar(aes(groups, probability), stat="identity")
    
  })
  #plot posterior distributions 
  output$prob_plot1 <-renderPlot({
    rate <- seq(0,1,length=10000)
    a_alpha <- test.output()$v1_beta.A
    a_beta <- test.output()$v1_beta.B
    b_alpha <- test.output()$v2_beta.A
    b_beta <- test.output()$v2_beta.B
    
    #use distribution parameters to set limits of x axis, N.B. very approximate form of sdev due to integer overflow
    mean_a <- a_alpha/(a_alpha+a_beta)
    mean_b <- b_alpha/(b_alpha+b_beta)
    sdev_a <- 3*sqrt(1/(a_alpha+b_beta))
    sdev_b <- 3*sqrt(1/(a_alpha+b_beta))

    
    a_dens <- dbeta(rate, test.output()$v1_beta.A, test.output()$v1_beta.B)
    b_dens <- dbeta(rate, test.output()$v2_beta.A, test.output()$v2_beta.B)
    ggplot() + geom_line(aes(rate,a_dens, color="A Posterior"))+geom_line(aes(rate,b_dens, color="B Posterior")) + 
      scale_color_manual(name='', values = c('A Posterior'='#00AEEF', 'B Posterior'='#4F3685')) + 
      labs(x="rate", y="probability density") + xlim(max(min(mean_a,mean_b)-max(sdev_a,sdev_b),0), min(max(mean_a,mean_b)+max(sdev_a,sdev_b),1))

  })
  #print the test result
  output$result <- renderText(
    if(input$go>0){
      if(which.max(test.output()$dist)==1 || round(test.output()$dist[2], 3)==signif(test.output()$dist[3],3)){
      "Test Result: DRAW"}
    else if(which.max(test.output()$dist)==2){
      "Test Result: A WINS"}
    else if(which.max(test.output()$dist)==3){
     "Test Result: B WINS" }}
    else{
      "Test Result:"
      }
    )
  #print the MAP difference between the variants
  output$difference <- renderText({
    if(input$go>0){
      paste("Difference: ", signif(test.output()$diff, 3))
    }else{
      "Difference:"
    }
    })
  
}
shinyAppDir(".")
shinyApp(ui = ui, server = server)