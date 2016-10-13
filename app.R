library(shiny)

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
                   textInput(inputId =  "acon",
                             label = "Variant A Conversions",
                             width="80%"),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   textInput(inputId =  "bcon",
                             label = "Variant B Conversions",
                             width="80%"),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   passwordInput(inputId =  "avol",
                                 label = "Variant A Volume",
                                 width="80%"),
                   style = "font-size: 12px; line-height: 2.1;"),
            column(3, align="center",
                   passwordInput(inputId =  "bvol",
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
    column(10,align="center", wellPanel(align="center", style = "overflow: hidden; background-color:  #00AEEF; max-width:600px",  plotOutput("display.image")),
    column(1)
    )
             
))

server  <- function(input, output){
  rvs <- reactiveValues(default=0)
  observeEvent(input$go, {rvs$default <- input$go })
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
  output$display.image <- renderImage({
    if(rvs$default==0){
      segment="first"
    }else{
      if(imagen()$age>=60){
        segment <- "silverSurfers"
      }else if((imagen()$income>=44000 | imagen()$houseprice>=335000) & imagen()$children==0 & imagen()$age>=18 & imagen()$age<=40){
        segment <- "footlooseSingles"
      }else if((imagen()$income>=44000 | imagen()$houseprice>=335000) & imagen()$children>0) {
        segment <- "freeSpenders"
      }else if(imagen()$income<44000 & imagen()$houseprice<335000 & imagen()$children==0 & imagen()$age>=18 & imagen()$age<=35){
        segment <- "startingOut"
      }else if(imagen()$income<44000 & imagen()$houseprice<335000 & imagen()$children>0 & imagen()$age>=41 & imagen()$age<=59){
        segment <- "foundationFamilies"
      }else if(imagen()$income<44000 & imagen()$houseprice<335000 & imagen()$children==0){
        segment <- "midlifeJunction"
      }else if(imagen()$income<44000 & imagen()$houseprice<335000){
        segment <- "settlingDown"
      }
    }
    image_file <- paste("",segment, ".png", sep="")
    
    return(list(
      src = image_file,
      filetype = "image/jpeg",
      height = 400,
      width = 532
    ))
    
  }, deleteFile = FALSE)
}
shinyAppDir(".")
shinyApp(ui = ui, server = server)