library(shiny)
library(ggplot2)


bayesian_test <- function(prior_success=1, prior_vol=2, v1_success, v1_vol, v2_success, v2_vol, thresh=0, n.trials=100000){
  
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
  }
  results <- list("winner"=winner, 
                  "v1_beta.A"=v1_beta.A, "v1_beta.B"=v1_beta.B,
                  "v2_beta.A"=v2_beta.A, "v2_beta.B"=v2_beta.B,
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
                   numericInput(inputId =  "a_conv",
                             label = "Variant A Conversions",
                             width="80%",
                             value=1),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   numericInput(inputId =  "b_conv",
                             label = "Variant B Conversions",
                             width="80%",
                             value=1),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   numericInput(inputId =  "a_vol",
                                 label = "Variant A Volume",
                                 width="80%",
                                 value=2),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   numericInput(inputId =  "b_vol",
                                 label = "Variant B Volume",
                                 width="80%",
                                value=2),
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
    column(4,align="center", wellPanel(align="center", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",  plotOutput("prob_plot1"))),
    column(4,align="center", wellPanel(align="center", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",  plotOutput("prob_plot2"))),
    column(4,  titlePanel("Hello Shiny!"), textOutput("result"))
    )
             
)

server  <- function(input, output){
  
  
  
  test.output <-eventReactive(input$go,{
    bayesian_test(v1_success =  input$a_conv,
                  v1_vol = input$a_vol,
                  v2_success = input$b_conv,
                  v2_vol = input$b_vol)
    
  })

  output$prob_plot2 <-renderPlot({
    
    probs <- seq(0,1,length=1000)
    probability <- test.output()$dist
    if(probability[1]==0){
      groups <- c( "A Wins", "B Wins")
      probability <- probability[2:3]
    }else{
      groups <- c("Draw", "A Wins", "B Wins")
    }
    
    ggplot() + geom_bar(aes(groups, probability), stat="identity")
    
  })
  
  output$prob_plot1 <-renderPlot({
    cat(test.output()$dist)
    probs <- seq(0,1,length=1000)
    cat(test.output()$v1_beta.A, test.output()$v1_beta.B)
    a_dens <- dbeta(probs, test.output()$v1_beta.A, test.output()$v1_beta.B)
    b_dens <- dbeta(probs, test.output()$v2_beta.A, test.output()$v2_beta.B)
    ggplot() + geom_line(aes(probs,a_dens, color="red"))+geom_line(aes(probs,b_dens, color="blue"))

  })
  
  output$result <- renderText(if(which.max(test.output()$dist)==1 || round(test.output()$dist[2], 3)==round(test.output()$dist[3],3)){
                                "DRAW"}
                              else if(which.max(test.output()$dist)==2){
                                "A WINS"}
                              else if(which.max(test.output()$dist)==3){
                                "B WINS"
                              })

                              
  
}
shinyAppDir(".")
shinyApp(ui = ui, server = server)