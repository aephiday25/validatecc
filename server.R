library(shiny)
source("global.R")

shinyServer(function(input, output, session){
  
  ccnumber <- eventReactive(input$validate, {
    gsub("[a-zA-Z]+", "", paste0(input$num1, input$num2, input$num3, input$num4))
  })
  
  observeEvent(input$validate, {
    # message(ccnumber())
    ccnumber <- ccnumber()
    vldt <- validity(ccnumber)
    
    output$validity <- renderUI({
      if(ccnumber == "") return(NULL)
      if(vldt == "VALID!"){
        # message(vldt)
        h1(vldt, 
           style = "display: inline-block;
                    vertical-align: top;
                    width: 100%;
                    font-weight: bold;
                    font-size: 150%;
                    text-align: center;
                    color: darkgreen;")
      } else {
        h1(vldt, 
           style = "display: inline-block;
                    vertical-align: top;
                    width: 100%;
                    font-weight: bold;
                    font-size: 150%;
                    text-align: center;
                    color: red;")
      }
    })
    
    output$cardtype <- renderImage({
      list(src = paste0("www/type/", cardtype(ccnumber),".png"),
             contentType = 'image/png',
             width = 100
             # height = 50
             )
    }, deleteFile = FALSE)
    
    output$bank <- renderImage({
      list(src = paste0("www/bank/", bank(ccnumber)[1],".png"),
           contentType = 'image/png',
           height = 60)
    }, deleteFile = FALSE)
  })
  
  
  
  
  
  # output$cardlevel <- renderUI({
  #   ccnumber <- ccnumber()
  #   if(ccnumber == "") return(NULL)
  #   if(bank(ccnumber)[2] == "Gold"){
  #     h1(bank(ccnumber)[2], style = "display:inline-block;vertical-align:top;width:100%;font-weight:bold;font-size:200%;text-align:center;color:gold;")
  #   } else if(bank(ccnumber)[2] == "Platinum") {
  #     h1(bank(ccnumber)[2], style = "display:inline-block;vertical-align:top;width:100%;font-weight:bold;font-size:200%;text-align:center;color:silver;")
  #   } else if(bank(ccnumber())[2] == "Classic") {
  #     h1(bank(ccnumber)[2], style = "display:inline-block;vertical-align:top;width:100%;font-weight:bold;font-size:200%;text-align:center;color:green;")
  #   }
  # })
})