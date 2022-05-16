library(shiny)
library(lattice)
library(lme4)
library(emmeans)
library(sjPlot)
library(nlme)
library(ggplot2)
library(tidyr)
library(sjPlot)
library(party)
library(rpart)
library(caTools)
library(MLmetrics)
library(caret)
library(fastDummies)
library(ggmosaic)
library(lmerTest)
library(plotly)
library(DT)
aspekti <- readRDS("aspektilog.rds")
puu <- readRDS("puu.rds")
puuhl <- readRDS("puuhl.rds")
vetosuhde <- readRDS("oddsit.rds")
morfomalliL2 <- readRDS('lmel2.rds')
morfomalliL2 <- readRDS('lmel2.rds')
mlmetrics_tree <- readRDS("mlmetrics_tree.rds")
mlmetrics_treehl <- readRDS("mlmetrics_treehl.rds")
mllogl2 <- readRDS("mllogl2.rds")
mlmetricsloghl <- readRDS("mlmetricsloghl.rds")
morfomalliL1 <- readRDS("morfoL1.rds")
ortomalliL2 <- readRDS("ortoseml2.rds")
ortomalliL1 <- readRDS("ortosemL1.rds")
aspl2 <- readRDS("aspl2.rds")
asphl <- readRDS("asphl.rds")
mllogl2 <- readRDS("mllogl2.rds")
l2log <- readRDS("vidglm_l2.rds")
hllog <- readRDS("vidglm_hl.rds")
l2 <- readRDS("l2.rds")
hl <- readRDS("hl.rds")
prof_l2 <- readRDS("prof_l2.rds")
prof_hl <- readRDS("prof_hl.rds")
labralmel2 <- readRDS("labralmel2.rds")
aspektiaineisto <- readRDS("aspektiaineisto.rds")
aspektiaineisto_hl <- readRDS("aspektiaineisto_hl.rds")
reactionl2 <- readRDS("reaktioajatl2onl.rds")
reactionl2llab <- readRDS("reaktioajatl2onl.rds")
aspmark <-readRDS("aspmark.rds")
aspstem <- readRDS("aspstem.rds")
hlaspstem <- readRDS("hlaspstem.rds")
hlaspmark <- readRDS("hlaspmark.rds")
RTPrimemorfoL2lab <- readRDS("reaktiomorfol2lab.rds")
RTPrimemorfoL2 <- readRDS("reaktiomorfol2.rds")
RTPrimeortoL2 <- readRDS("reaktioortoseml2.rds")
RTPrimemorfoNAT <- readRDS("reaktiomorfo_nat.rds")
RTPrimeortoNAT <- readRDS("reaktioortosem_nat.rds")

ui <- navbarPage(
  "My PhD",   
  tabPanel("LME Priming", radioButtons(inputId = "dataset", "Linear mixed effects model", choices = c("RT Morf L2", "RT Morf L2 lab",
                                                                                   "RT Morf L1", "RT Orto/Sem L2",
                                                                                   "RT Orto/Sem L1"), selected = "RT Morf L2"), 
           verbatimTextOutput("summary")),
  tabPanel("Priming stats", radioButtons(inputId = "rt","Mean RT by priming condition, milliseconds", choices = c("RT morphological condition L2 lab",
                                                                                                    "RT morphological condition L2",
                                                                                                    "RT orthographic/semantic condition L2",
                                                                                                    "RT morphological condition L1",
                                                                                                    "RT orthographic/semantic condition L1")),verbatimTextOutput("keskiarvot")),
  tabPanel("Visualize", radioButtons("mosaic","Mosaic plot", choices = c("Marker L2","Stem L2","Marker HL","Stem HL")),plotlyOutput("ainvisu"),selectInput(inputId = "asptaul","Choose dataset", choices = c("L2","HL")),dataTableOutput("taulvisu")),
  tabPanel("Asp, Odds ratio", radioButtons(inputId = "sjplot","Odds ratio", choices = c("L2","HL","Random effect language L2","Random effect language HL" ,"Random effect proficiency L2","Random effect proficiency HL")), plotlyOutput("vetosuhde")),
  tabPanel("Asp, logreg", 
           radioButtons(inputId = "loginp", "Statistics", choices = c("ASP L2", "ASP HL")),
           radioButtons(inputId = "logmlinp", "Machine Learning", choices = c("ML L2", "ML HL")), 
           verbatimTextOutput("aspekti"),
           verbatimTextOutput("mllog")),
  tabPanel("Asp, tree models", 
           radioButtons(inputId = "treeinp", "Statistics", choices = c("Decision tree L2", "Decision tree HL")),
           plotOutput("tree"),
           fileInput("upload","Upload dataset"),
           selectInput("var1_ml", "Select ML Variable", choices = NULL),
           selectInput("var2_ml", "Select ML Variable", choices = NULL),
           selectInput("var3_ml", "Select ML Variable", choices = NULL),
           actionButton("go_ml","Run ML model"),
           verbatimTextOutput("glmSummary"),
           verbatimTextOutput("mlSummary"))
  )


server <- function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
    
  })
  
  
  
  observeEvent(input$upload, {
    req(data())
    updateSelectInput(session, "var1", choices = colnames(data()))
    updateSelectInput(session, "var2", choices = colnames(data()))
    updateSelectInput(session, "var3", choices = colnames(data()))
    updateSelectInput(session, "var4", choices = colnames(data()))
    updateSelectInput(session, "var5", choices = colnames(data()))
    updateSelectInput(session, "var1_ml", choices = colnames(data()))
    updateSelectInput(session, "var2_ml", choices = colnames(data()))
    updateSelectInput(session, "var3_ml", choices = colnames(data()))
  })
  
  mosaiikki <- reactive({
    switch(input$mosaic,
           "Marker L2" = aspmark,
           "Stem L2" = aspstem,
           "Marker HL" = hlaspmark,
           "Stem HL" = hlaspstem)
    })

    reaktioajat <- reactive({
    switch(input$rt,
          "RT morphological condition L2 lab"  = RTPrimemorfoL2lab, 
          "RT morphological condition L2" = RTPrimemorfoL2,
          "RT orthographic/semantic condition L2" = RTPrimeortoL2,
          "RT morphological condition L1" = RTPrimemorfoNAT, 
          "RT orthographic/semantic condition L1" = RTPrimeortoNAT 
    )
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "RT Morf L2" = morfomalliL2,
           "RT Morf L2 lab" = labralmel2,
           "RT Morf L1" = morfomalliL1,
           "RT Orto/Sem L2" = ortomalliL2,
           "RT Orto/Sem L1" = ortomalliL1)
            
  })
  
  aspektitaulukko <- reactive({
    switch(input$asptaul,
           "L2" = aspektiaineisto,
           "HL" = aspektiaineisto_hl)
  })
  
  treeInput <- reactive({
    switch(input$treeinp,
           "Decision tree L2" = puu,
           "Decision tree HL" = puuhl
           )
  })
  
  oddsit <- reactive({
    switch (input$sjplot,
            "L2" = l2log,
            "HL" = hllog,
            "Random effect language L2" = l2,
            "Random effect language HL" = hl,
            "Random effect proficiency L2" = prof_l2,
            "Random effect proficiency HL" = prof_hl)
  })
  
  mltreeInput <- reactive({
    switch(input$mltreeinp,
           "ML CART L2" = mlmetrics_tree,
           "ML CART HL" = mlmetrics_treehl)
  })
  
  logreg <- reactive({
    switch(input$loginp,
           "ASP L2" = aspl2,
           "ASP HL" = asphl)
  })
  
  logml <- reactive({
    switch(input$logmlinp,
          "ML L2" = mllogl2,
          "ML HL" = mlmetricsloghl)
  })
  
  output$keskiarvot <- renderPrint({
    reaktioajat()
  })
  
  output$mllog <- renderPrint({
    logml()
  })   

  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$aspekti <- renderPrint({
    loginp <- logreg()
    summary(loginp)
  })
  
  output$tree <- renderPlot({
  treeinp <- treeInput()
  plot(treeinp)}
  )
  
  output$mltree <- renderPrint({
    mlcart <- mltreeInput()
    mlcart
  })
  
  output$ainvisu <- renderPlotly({
    ggplotly(mosaiikki())
  })
  
  output$taulvisu <- renderDataTable({
    aspektitaulukko() 
  })
  
  output$vetosuhde <- renderPlotly({
   oddsit()
    })
  
  mlModel <- eventReactive(input$go_ml, {
    req(data(),input$var1_ml,input$var2_ml,input$var3_ml)
    x3 <- as.factor(data()[[as.name(input$var1_ml)]])
    y3 <- as.factor(data()[[as.name(input$var2_ml)]])
    z3 <- as.factor(data()[[as.name(input$var3_ml)]])
    df <- data.frame(x3,y3,z3)
    set.seed(123)
    split = sample.split(df$x3, SplitRatio = 0.75)
    training_set = subset(df, split == TRUE)
    test_set = subset(df, split == FALSE)
    classifier = rpart(formula = x3 ~ y3 + z3, data = training_set)
    y_pred = predict(classifier, newdata = test_set[-1], type = 'class')
    accuracy <- Accuracy(y_pred,test_set[,1])*100
    recall <- Recall(y_pred,test_set[,1])*100
    precision <- Precision(y_pred,test_set[,1])*100
    f1 <- F1_Score(y_pred,test_set[,1])*100
    metrics <- cbind(accuracy,recall,precision,f1)
    metrics   
  })
  
  output$mlSummary <- renderPrint({
    req(mlModel())
    mlModel()
  })
  
}
  



shinyApp(ui, server)
