### ASSUMPTIONS ###

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
## PERFORMANCE ##

#All Data From Yahoo Finance
#Adjusted Closing Prices Considered to Include Dividends Into Returns Calculations
#Discrete Compounding for Rate of Change
#Geometric Average for Returns
#Proxy for S&P500 Returns -> SPY (To Take Dividends Into Account)
#Proxy for S&P/TSX Returns -> XIC.TO (To Take Dividends Into Account)
#Proxy for MSCI EAFE IMI Returns -> XEF.TO (To Take Dividends Into Account). Note The Impact of Currency Fluctuation**
#Proxy for MSCI Emerging Markets Returns -> XEC.TO (To Take Dividends Into Account). Note The Impact of Currency Fluctuation**
#CAPM Assumptions: Benchmark = S&P500, Risk Free Rate = Treasury Yield 10 Years
#VaR Calculated Using "Historical" Method

# WARNINGS #

#To Maintain Accurate Results 
    #Do Not Include Short Positions
    #Only Pick Stocks, ETFs and/or Mutual Funds (No Futures, No Gold, No Cryptocurrencies, etc)
    #All Assets in the Portfolio Have to be Trading in the Same Country 
        #(Different Exchanges is Fine)
    #Add Entries to Rebalancing in Chronological Order

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
## CONSTRUCTION ##

#Efficient Frontier Built Using Weekly Returns
    #Expected Volatility Based on Historical Volatility (Furthest Between 1995 and IPO Date)
    #Expected Returns Based on 1-Y Target Price (Analyst Consensus) From Yahoo Finance (If Available). Otherwise, Based on Historical Returns (Furthest Between 1995 and IPO Date)
#Monte Carlo Simulation of 1000 Portfolios

# WARNINGS #

#To Maintain Accurate Results 
    #Only Pick Stocks, ETFs and/or Mutual Funds (No Futures, No Gold, No Cryptocurrencies, etc)

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


library(shiny)
library(quantmod)                            
library(PerformanceAnalytics)
library(zoo)
library(xts)
library(plyr)
library(dygraphs)
library(ggplot2)
library(RiskPortfolios)
library(quadprog)
library(rvest)
library(purrr)
library(dplyr)

#User Interface
ui <- shinyUI(navbarPage("Analysis",
                         
      tabPanel(
        "Performance",
         
        includeCSS("dy-graph.css"), #Include CSS File
        
        titlePanel("Performance"),
        br(),
        
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId =  "DateFrom", 
                    label = "Starting Year (YYYY-01-01)", 
                    choices = (as.numeric(format(Sys.Date(), "%Y"))-1):2014  #Starts in 2014 Since MSCI EAFE IMI (XEF.TO) and MSCI Emerging Markets (XEC.TO) ETFs Opened in 2013
                ),
                selectInput(
                    inputId =  "DateTo", 
                    label = "Ending Year (YYYY-12-31)", 
                    choices = (as.numeric(format(Sys.Date(), "%Y"))-1):2014    
                ),
                h3("Initial Portfolio"),
                p("Select a portfolio of Stocks, ETFs and/or Mutual funds trading in the same country. No short positions", style = "font-size: 12px"),
                textInput("Stock","Ticker (Yahoo)"),
                numericInput("Shares","Number of Shares",0, min = 0, step = 1),
                column(12,
                       splitLayout(cellWidths = c("70%", "30%"),
                                   actionButton("action", "Add",icon("dollar-sign"),  
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   actionButton("reset", "Reset",icon("trash"),  
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                br(),
                br(),
                h3("Rebalancing"),
                p("Add in chronological order", style = "font-size: 12px"),
                dateInput("DateReb", "Date of Purchase/Sale", min = "2014-01-01", max = paste0(as.numeric(format(Sys.Date(), "%Y"))-1, "-12-31")),
                textInput("Stock1", "Ticker (Yahoo)"),
                numericInput("Shares1","Number of Shares (+/-)", 0, step = 1),
                column(12,
                       splitLayout(cellWidths = c("70%", "30%"),
                                   actionButton("action1", "Add",icon("dollar-sign"),  
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   actionButton("reset1", "Reset",icon("trash"),  
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                br(),
                br(),
                fluidRow(
                    align = "center",
                    p("____________________________________"),
                    p("Ready to launch?", style = "font-size: 14px; font-weight: bold"),
                    actionButton("Go", "Go!", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin: auto")),
            ),
            
            mainPanel(
                column(10,
                       splitLayout(cellWidths = c("50%", "50%"),
                                   htmlOutput("InitialHoldings", style = "font-weight: bold; text-decoration: underline"),
                                   htmlOutput("Rebalancing", style = "font-weight: bold; text-decoration: underline"))),
                br(),
                br(),
                fluidRow(
                    column(12,
                           align = "center",
                           splitLayout(cellWidths = c("40%", "60%"),
                                       tableOutput("table"), 
                                       tableOutput("tableREB"), 
                                       style = "height:185px; overflow-y: scroll; border: 1px solid #e3e3e3; border-radius: 8px; background-color: #f7f7f7;text-align: left; overflow-x: hidden"))),
                fluidRow(
                    column(12,
                           align = "center",
                           br(),
                           dygraphOutput("Graph"),
                           br(),
                           div(tableOutput("RetTable"), style = "font-size: 13px"),
                           div(tableOutput("AB"), style = "font-size: 13px")))
            )
      )),
      
      tabPanel(
          "Construction",
          
          titlePanel("Construction"),
          br(),
          
          sidebarLayout(
              sidebarPanel(
                  textInput("Stockw","Ticker (Yahoo)"),
                  numericInput("Sharesw","Number of Shares",0, min = 0, step = 1),
                  selectInput("Countryw","Country",choices = c("Canada","United States")),
                  
                  column(12,
                         splitLayout(cellWidths = c("70%", "30%"),
                                     actionButton("actionw", "Add",icon("dollar-sign"),  
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                     actionButton("resetw", "Reset",icon("trash"),  
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  br(),
                  br(),
                  checkboxInput("EF", "Efficient Frontier"),
                  checkboxInput("MonteCarlo", "Monte Carlo Simulation"),
                  
                  fluidRow(
                      align = "center",
                      p("____________________________________"),
                      p("Ready to launch?", style = "font-size: 14px; font-weight: bold"),
                      actionButton("Gow", "Go!", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin: auto")),
                  
              ),
              
              mainPanel(
                  column(12,
                         tableOutput("tablew"), 
                         style = "height:185px; overflow-y: scroll; border: 1px solid #e3e3e3; border-radius: 8px; background-color: #f7f7f7;text-align: left; overflow-x: hidden"),
                  column(12,
                         br(),
                         align = "left",
                         splitLayout(cellWidths = c("70%", "30%"),
                                     plotOutput("Graphw"),
                                     tableOutput("myTable"))),
                  column(12,
                    align = "center",
                    plotOutput("myGraph"))
              )
          )
      )
))


#Server
server <- shinyServer(function(input, output) {
    
    #PERFORMANCE
    
    #Store Initial Tickers/Number of Shares From User Inputs (In Vectors and Data Frame)
    valuesDF <- reactiveValues() #Initialize Data Frame
    valuesDF$df <- data.frame("Ticker" = numeric(0), "Shares" = numeric(0)) 
    valuesVEC <- reactiveValues(tickers = NULL, SharesVec = NULL) #Initialize Vectors
    
    observeEvent(input$action, {
        isolate(valuesDF$df[nrow(valuesDF$df) + 1,] <- c(input$Stock, input$Shares)) #Store Data frame
        valuesVEC$tickers <- c(valuesVEC$tickers,input$Stock)                        #Store Vectors
        valuesVEC$SharesVec <- c(valuesVEC$SharesVec,input$Shares)
    })
    
    #Reset Initial Tickers/Number of Shares From User Inputs (In Vectors and Data Frame)
    observeEvent(input$reset, {
        valuesVEC$tickers <- valuesVEC$tickers[-1:-(length(valuesVEC$tickers))] #Reset Vectors
        valuesVEC$SharesVec <- valuesVEC$SharesVec[-1:-(length(valuesVEC$SharesVec))]
        valuesDF$df <- valuesDF$df[0,] #Reset Data Frame
    })
    
    #Store Rebalancing Tickers/Number of Shares From User Inputs (In Vectors and Data Frame)
    valuesREB <- reactiveValues()
    valuesREB$REB <- data.frame("Ticker" = numeric(0), "Shares" = numeric(0), "Date" = numeric(0)) 
    valuesRV <- reactiveValues(tickersREB = NULL, SharesVecREB = NULL, DateREB = NULL)
    
    observeEvent(input$action1, {
        isolate(valuesREB$REB[nrow(valuesREB$REB) + 1,] <- c(input$Stock1, input$Shares1, as.character(input$DateReb))) 
        valuesRV$tickersREB <- c(valuesRV$tickersREB,input$Stock1)                        
        valuesRV$SharesVecREB <- c(valuesRV$SharesVecREB,input$Shares1)
        valuesRV$DateREB <- c(valuesRV$DateREB,as.character(input$DateReb))
    })
    
    #Reset Rebalancing Tickers/Number of Shares From User Inputs (In Vectors and Data Frame)
    observeEvent(input$reset1, {
        valuesREB$REB <- valuesREB$REB[0,]
        valuesRV$tickersREB <- valuesRV$tickersREB[-1:-(length(valuesRV$tickersREB))]
        valuesRV$SharesVecREB <- valuesRV$SharesVecREB[-1:-(length(valuesRV$SharesVecREB))]
        valuesRV$DateREB <- valuesRV$DateREB[-1:-(length(valuesRV$DateREB))]
    })
    
    #Call Function (Defined Bellow)
    OP <- reactiveValues()
    observeEvent(input$Go, {
        showModal(modalDialog("Loading... Please Wait", footer=NULL)) #Creates Loading Pop-up Message
        OP$LIST <- RunPlot(valuesVEC$tickers,valuesVEC$SharesVec,
                           as.Date(paste0(as.numeric(input$DateFrom)-1,"-12-31")),
                           as.Date(paste0(as.numeric(input$DateTo),"-12-31")),
                           valuesRV$tickersREB,valuesRV$SharesVecREB,valuesRV$DateREB)
        removeModal() #Removes Loading Pop-up Message
    })
    
    #Output Variables
    output$table <- renderTable({valuesDF$df}) #Initial Holdings Data Frame
    output$tableREB <- renderTable({valuesREB$REB}) #Rebalancing Data Frame
    output$InitialHoldings <- renderText({paste("Initial Holdings")})
    output$Rebalancing <- renderText({paste("Rebalancing")})
    
    output$Graph <- renderDygraph({ #Interactive Graph
        OP$LIST[[1]]
    })
    output$RetTable <- renderTable({ #Returns, Std Dev, Sharpe (Data Frame)
        OP$LIST[[2]]}, rownames = TRUE
    )
    output$AB <- renderTable({ #Alpha/Beta (Date Frame)
        OP$LIST[[3]]
    })
    
    #Define Analysis Function
    RunPlot <- function(tickers, sharesVec, FromDate, ToDate, tickersREB, SharesVecREB, DateREB){
        
        #Find Closing Prices of Every Tickers From Initial Portfolio
        PortfolioPrices <- NULL 
        for (i in tickers){
            PortfolioPrices <- cbind(PortfolioPrices, 
                                     getSymbols.yahoo(i, from = FromDate, to = ToDate,             
                                                      auto.assign = F)[,6])  
        }  
        
        #Compute Initial Weights
        TempValue <- PortfolioPrices[1,]
        MarketValue <- sharesVec*TempValue
        Weights <- MarketValue/sum(MarketValue) 
        
        #Rebalancing (If Needed)
        if (length(tickersREB)>0){
            
            TEMPDF <- data.frame(tickersREB, SharesVecREB, DateREB) #Store Rebalancing Info in Data Frame
            WeightREB <- fortify.zoo(Weights) #Convert Initial Weights From XTS to Data Frame
            
            #Compute Weights at Rebalancing Dates
            for (i in tickersREB){
                PortfolioPrices <- cbind(PortfolioPrices, 
                                         getSymbols.yahoo(i, from = FromDate, to = ToDate,             
                                                          auto.assign = F)[,6])  
                
                for (j in 1:nrow(TEMPDF)){
                    if (TEMPDF[j,1]==i){
                        DateUsed <- TEMPDF[j,3]
                        SharesUsed <- TEMPDF[j,2]
                    }
                }
                
                TempValue <- PortfolioPrices[DateUsed]
                sharesVec <- c(sharesVec,SharesUsed)
                MarketValue <- sharesVec*TempValue
                TempWeights <- MarketValue/sum(MarketValue) 
                
                TempWeights <- fortify.zoo(TempWeights) 
                WeightREB <- rbind.fill(WeightREB, TempWeights) 
                
            }
            
            WeightREB[is.na(WeightREB)] <- 0
            TimeXTS <- WeightREB[,1]
            WeightsXTS <- WeightREB[,2:ncol(WeightREB)]
            WeightREB <- xts(WeightsXTS,TimeXTS) #Convert Weights Back to XTS
            
            Weights <- WeightREB #Updated Weights Object (Now Including Rebalancing Weights)
        
            #Hotfix for "IPO Crash"
            for (j in 1:ncol(PortfolioPrices)){
                if (is.na(PortfolioPrices[1,j]) == TRUE){
                    for (i in nrow(PortfolioPrices):1){
                        if (is.na(PortfolioPrices[i,j]) == TRUE){
                            PortfolioPrices[i,j] <- PortfolioPrices[i+1,j]
                        }
                    }
                }
            }
        }

        #Compute Risk-Free Rate
        Rf <- getQuote("^TNX", src = "yahoo")[1,2]/100 
        
        #Compute Annualized Portfolio Returns and VaR
        AssetsReturns <- na.omit(ROC(PortfolioPrices, type = "discrete"))                                  
        PortfolioReturns <- Return.portfolio(AssetsReturns, weights = Weights)                             
        PortfolioAnnualizedReturns <- table.AnnualizedReturns(PortfolioReturns, scale = nrow(PortfolioReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(PortfolioReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))                       
        colnames(PortfolioAnnualizedReturns) <- "Portfolio"
        PortfolioVAR95 <- VaR(PortfolioReturns, p=0.95, method = "historical")
        PortfolioVAR99 <- VaR(PortfolioReturns, p=0.99, method = "historical")
        
        #Compute Annualized S&P500 Returns and VaR
        SP500 <- getSymbols.yahoo("SPY", from = FromDate, to = ToDate, auto.assign = F)[,6]       
        SP500Returns <- na.omit(ROC(SP500, type = "discrete"))                                             
        SP500AnnualizedReturns <- table.AnnualizedReturns(SP500Returns, scale = nrow(SP500Returns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(SP500Returns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))))) 
        colnames(SP500AnnualizedReturns) <- "S&P500"
        SP500VAR95 <- VaR(SP500Returns, p=0.95, method = "historical")
        SP500VAR99 <- VaR(SP500Returns, p=0.99, method = "historical")
        
        #Compute Annualized S&P/TSX Returns and VaR
        SPTSX <- getSymbols.yahoo("XIC.TO", from = FromDate, to = ToDate, auto.assign = F)[,6]   
        SPTSXReturns <- na.omit(ROC(SPTSX, type = "discrete"))                                             
        SPTSXAnnualizedReturns <- table.AnnualizedReturns(SPTSXReturns, scale = nrow(SPTSXReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(SPTSXReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))  
        colnames(SPTSXAnnualizedReturns) <- "S&P/TSX"
        SPTSXVAR95 <- VaR(SPTSXReturns, p=0.95, method = "historical")
        SPTSXVAR99 <- VaR(SPTSXReturns, p=0.99, method = "historical")
        
        #Compute Annualized MSCI EAFE IMI Returns and VaR
        MSCIIMI <- getSymbols.yahoo("XEF.TO", from = FromDate, to = ToDate, auto.assign = F)[,6]   
        MSCIIMIReturns <- na.omit(ROC(MSCIIMI, type = "discrete"))                                             
        MSCIIMIAnnualizedReturns <- table.AnnualizedReturns(MSCIIMIReturns, scale = nrow(MSCIIMIReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(MSCIIMIReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))))) 
        colnames(MSCIIMIAnnualizedReturns) <- "MSCI EAFE IMI"
        MSCIIMIVAR95 <- VaR(MSCIIMIReturns, p=0.95, method = "historical")
        MSCIIMIVAR99 <- VaR(MSCIIMIReturns, p=0.99, method = "historical")
        
        #Compute Annualized MSCI Emerging Markets Returns and VaR
        MSCIEME <- getSymbols.yahoo("XEC.TO", from = FromDate, to = ToDate, auto.assign = F)[,6]   
        MSCIEMEReturns <- na.omit(ROC(MSCIEME, type = "discrete"))                                             
        MSCIEMEAnnualizedReturns <- table.AnnualizedReturns(MSCIEMEReturns, scale = nrow(MSCIEMEReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(MSCIEMEReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))  
        colnames(MSCIEMEAnnualizedReturns) <- "MSCI Emerging Markets"
        MSCIEMEVAR95 <- VaR(MSCIEMEReturns, p=0.95, method = "historical")
        MSCIEMEVAR99 <- VaR(MSCIEMEReturns, p=0.99, method = "historical")
        
        #Create VaR Table
        VAR <- rbind(c(PortfolioVAR95,SP500VAR95,SPTSXVAR95,MSCIIMIVAR95,MSCIEMEVAR95),
                     c(PortfolioVAR99,SP500VAR99,SPTSXVAR99,MSCIIMIVAR99,MSCIEMEVAR99))
        rownames(VAR) <- c("1-Day VaR (95%)", "1-Day VaR (99%)")
        colnames(VAR) <- c("Portfolio","S&P500","S&P/TSX","MSCI EAFE IMI","MSCI Emerging Markets")
        
        #Find Cumulative Returns (Used in Plot)
        PortfolioCumReturns <- fortify.zoo(PortfolioReturns+1)                                             
        SP500CumReturns <- fortify.zoo(SP500Returns+1)
        SPTSXCumReturns <- fortify.zoo(SPTSXReturns+1)
        MSCIIMICumReturns <- fortify.zoo(MSCIIMIReturns+1)
        MSCIEMECumReturns <- fortify.zoo(MSCIEMEReturns+1)
        
        for (j in 2:nrow(PortfolioCumReturns)){                                                            
            PortfolioCumReturns[j,2] <- PortfolioCumReturns[j-1,2]*PortfolioCumReturns[j,2]
        }
        for (j in 2:nrow(SP500CumReturns)){
            SP500CumReturns[j,2] <- SP500CumReturns[j-1,2]*SP500CumReturns[j,2]
        }
        for (j in 2:nrow(SPTSXCumReturns)){
            SPTSXCumReturns[j,2] <- SPTSXCumReturns[j-1,2]*SPTSXCumReturns[j,2]
        }
        for (j in 2:nrow(MSCIIMICumReturns)){
            MSCIIMICumReturns[j,2] <- MSCIIMICumReturns[j-1,2]*MSCIIMICumReturns[j,2]
        }
        for (j in 2:nrow(MSCIEMECumReturns)){
            MSCIEMECumReturns[j,2] <- MSCIEMECumReturns[j-1,2]*MSCIEMECumReturns[j,2]
        }
        
        PortfolioCumReturns <- PortfolioCumReturns - 1                                                      
        SP500CumReturns <- SP500CumReturns - 1
        SPTSXCumReturns <- SPTSXCumReturns - 1
        MSCIIMICumReturns <- MSCIIMICumReturns - 1
        MSCIEMECumReturns <- MSCIEMECumReturns - 1
        
        #Annualized Returns, Std Dev,Sharpe Ratio and VaR (UI Output)
        ReturnsTABLE <- cbind(PortfolioAnnualizedReturns,SP500AnnualizedReturns,SPTSXAnnualizedReturns,MSCIIMIAnnualizedReturns,MSCIEMEAnnualizedReturns)
        ReturnsTABLE <- rbind(ReturnsTABLE, VAR)
        
        #Compute Alpha/Beta (UI Output)
        Benchmark <- getSymbols.yahoo("^GSPC", from = FromDate, to = ToDate, auto.assign = F)[,6]   
        BenchmarkReturns <- na.omit(ROC(Benchmark, type = "discrete"))  
        BETA <- CAPM.beta(PortfolioReturns, BenchmarkReturns, Rf/(nrow(BenchmarkReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))
        ALPHA <- CAPM.jensenAlpha(PortfolioReturns, BenchmarkReturns, Rf/(nrow(BenchmarkReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))
        df <- data.frame(ALPHA,BETA)
        
        #Interactive Plot
        #Convert Every Returns Data Frame to XTS (Needed for dygraph)
        #Portfolio
        PCR1 <- PortfolioCumReturns[,1]
        PCR2 <- PortfolioCumReturns[,2]
        PortfolioCumReturns <- xts(PCR2,PCR1)
        colnames(PortfolioCumReturns) <- "Portfolio"
        
        #S&P500
        SP1 <- SP500CumReturns[,1]
        SP2 <- SP500CumReturns[,2]
        SP500CumReturns <- xts(SP2,SP1)
        colnames(SP500CumReturns) <- "S&P500"
        
        #S&P/TSX
        TSX1 <- SPTSXCumReturns[,1]
        TSX2 <- SPTSXCumReturns[,2]
        SPTSXCumReturns <- xts(TSX2,TSX1)
        colnames(SPTSXCumReturns) <- "S&P/TSX"
        
        #MSCI EAFE IMI
        IMI1 <- MSCIIMICumReturns[,1]
        IMI2 <- MSCIIMICumReturns[,2]
        MSCIIMICumReturns <- xts(IMI2,IMI1)
        colnames(MSCIIMICumReturns) <- "MSCI EAFE IMI"
        
        #MSCI Emerging Markets
        EME1 <- MSCIEMECumReturns[,1]
        EME2 <- MSCIEMECumReturns[,2]
        MSCIEMECumReturns <- xts(EME2,EME1)
        colnames(MSCIEMECumReturns) <- "MSCI Emerging Markets"
        
        NEW <- cbind(PortfolioCumReturns,SP500CumReturns,SPTSXCumReturns,MSCIIMICumReturns,MSCIEMECumReturns)
        
        #Remove NAs for Nicer Looking Graph
        for (j in 1:ncol(NEW)){   
            
            for (i in 1:nrow(NEW)){
                if (is.na(NEW[i,j]) == TRUE){
                    NEW[i,j] <- NEW[i-1,j]
                }
            }
        }
        
        #Graph (UI Output)
        Graph <- dygraph(NEW, ylab = "Returns",main = "Performance Overview") %>%
            dyRangeSelector() %>%
            dyOptions(colors = c("black","Navy","steelblue","lightblue","lightsteelblue")) %>%
            dyLegend(width = 560) %>%
            dyCSS("dy-graph.css")
        
        #Final Output (List)
        return(list(Graph, ReturnsTABLE, df))
    }
    
    
    #CONSTRUCTION
    
    #Store Initial Tickers/Number of Shares/Countries From User Inputs (In Vectors and Data Frame)
    valuesDFw <- reactiveValues() #Initialize Data Frame
    valuesDFw$dfw <- data.frame("Ticker" = numeric(0), "Shares" = numeric(0), "Country" = numeric(0)) 
    valuesVECw <- reactiveValues(tickersw = NULL, SharesVecw = NULL, CountryVecw = NULL) #Initialize Vectors
    
    observeEvent(input$actionw, {
        isolate(valuesDFw$dfw[nrow(valuesDFw$dfw) + 1,] <- c(input$Stockw, input$Sharesw, input$Countryw)) #Store Data frame
        valuesVECw$tickersw <- c(valuesVECw$tickersw,input$Stockw)  #Store Vectors
        valuesVECw$SharesVecw <- c(valuesVECw$SharesVecw,input$Sharesw)
        valuesVECw$CountryVecw <- c(valuesVECw$CountryVecw, input$Countryw)
    })
    
    #Reset Initial Tickers/Number of Shares/Countries From User Inputs (In Vectors and Data Frame)
    observeEvent(input$resetw, {
        valuesVECw$tickersw <- valuesVECw$tickersw[-1:-(length(valuesVECw$tickersw))] #Reset Vectors
        valuesVECw$SharesVecw <- valuesVECw$SharesVecw[-1:-(length(valuesVECw$SharesVecw))]
        valuesVECw$CountryVecw <- valuesVECw$CountryVecw[-1:-(length(valuesVECw$CountryVecw))]
        valuesDFw$dfw <- valuesDFw$dfw[0,] #Reset Data Frame
    })
    
    #Call Functions (Defined Bellow)
    OPwPC <- eventReactive(input$Gow, {
      Run1(valuesVECw$tickersw,valuesVECw$SharesVecw,valuesVECw$CountryVecw)
    })

    OPw <- eventReactive(input$Gow, {
      selectedRun <- NULL
      showModal(modalDialog("Loading... Please Wait", footer = NULL))
      if (input$EF == TRUE && input$MonteCarlo == FALSE) {
        selectedRun <- Run2(valuesVECw$tickersw,valuesVECw$SharesVecw,valuesVECw$CountryVecw)
      } else if (input$MonteCarlo == TRUE && input$EF == FALSE) {
        selectedRun <- Run3(valuesVECw$tickersw,valuesVECw$SharesVecw,valuesVECw$CountryVecw)
      } else if (input$MonteCarlo == TRUE && input$EF == TRUE) {
        selectedRun <- Run3(valuesVECw$tickersw,valuesVECw$SharesVecw,valuesVECw$CountryVecw)
      }
      removeModal()
      return(selectedRun)
    })
    
    #Output Variables
    output$tablew <- renderTable({valuesDFw$dfw}) #Initial Holdings Data Frame
    output$Graphw <- renderPlot({ #Pie Chart
        OPwPC()}, height = 400, width = 400)
    
    output$myGraph <- renderPlot({ #Graph EF
      OPw()[[1]]
    },height = 550, width = 700)
    
    output$myTable <- renderTable({
      OPw()[[2]]
    }, colnames = TRUE)
})


#Functions
#Weights Function
Run1 <- function(tickersw, SharesVecw, CountryVecw){
  
  USDtoCAD <- getQuote("CAD=X", src = "yahoo")[2] #Convert USD to CAD
  USDtoCAD <- USDtoCAD[[1]] 
  
  #Select Last Prices (From Tickers)
  PortfolioPricesw <- NULL 
  tickersw <- toupper(tickersw) #CAPS
  for (i in tickersw){
      PortfolioPricesw <- cbind(PortfolioPricesw, getQuote(i, src = "yahoo")[,2])          
  }  
  
  #Convert USD Denominated Assets to CAD
  for (i in 1:length(PortfolioPricesw)){
      if(CountryVecw[i] == "United States"){
          PortfolioPricesw[i] <- USDtoCAD*PortfolioPricesw[i]
      }
  }
  
  #Find Weights
  MarketValuew <- SharesVecw*PortfolioPricesw
  Weightsw <- MarketValuew/sum(MarketValuew)*100
  colnames(Weightsw) <- tickersw
  
  #Create Pie Chart 
  tickersw <- tickersw[order(Weightsw)]; Weightsw <- sort(Weightsw)
  Percent <- factor(paste(tickersw, scales::percent(Weightsw/100, accuracy = 0.1)), paste(tickersw, scales::percent(Weightsw/100, accuracy = 0.1)))
  
  Plot <- ggplot() + theme_bw() +
      geom_bar(aes(x = "", y = Weightsw, fill = Percent),
               stat = "identity", color = "white") + 
      coord_polar("y", start = 0) +
      ggtitle("My Portfolio") +
            theme(axis.title = element_blank(),
              plot.title = element_text(size=14, face="bold.italic", hjust = 0.5),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.border = element_blank()) +
            guides(fill = guide_legend(reverse = TRUE)) + 
            theme(legend.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.key.size = unit(0.8,"cm")) 

  return(Plot)
  
}


#Efficient Frontier Function
Run2 <- function(tickersw, SharesVecw, CountryVecw){

AdjustedPrices <- NULL
TargetPrice <- NULL
CurrentPrice <- NULL
yret <- NULL
wret <- NULL
ReturnsVec <- NULL

#Scrape Data From Yahoo Finance Homepage
get_summary_table <- function(symbol){
  
  url <- paste0("https://finance.yahoo.com/quote/",symbol)
  df <- url %>%
    read_html() %>%
    html_table(header = FALSE) %>%
    map_df(bind_cols) %>%
    as_tibble()
  
  names(df) <- c("name", "value")
  df["stock"] <- symbol
  
  df
}

#Find Furthest Possible Starting Date of Portfolio
for (i in tickersw){
  AdjustedPrices <- cbind(AdjustedPrices, 
                          getSymbols.yahoo(i, from = "1995-01-01", to = Sys.Date(),             
                                           periodicity = "weekly", auto.assign = F)[,6])  
}  

count1 <- matrix(NA, nrow = length(AdjustedPrices[,1]), ncol = length(AdjustedPrices[1,]))
for (i in 1:length(AdjustedPrices[1,])){
  if (is.na(AdjustedPrices[1,i] == TRUE)){
    for (j in 1:length(AdjustedPrices[,1])){
      if (is.na(AdjustedPrices[j,i] == TRUE)){
        count1[j,i] <- TRUE
      }else{
        break
      }
    }
  }
}

count2 <- vector("numeric", length(AdjustedPrices[1,]))
for (k in 1:length(AdjustedPrices[1,])){
  count2[k] <- sum(count1[,k], na.rm = TRUE)
}

FinalCount <- max(count2)
APrices <- fortify.zoo(AdjustedPrices)
DateBreak <- APrices[FinalCount+1,1]


#Find Historical Adjusted Prices and 1-Y Target Prices (If Available)
Trigger <- 0
AdjustedPrices <- NULL
for (i in tickersw){
  AdjustedPrices <- cbind(AdjustedPrices, 
                          getSymbols.yahoo(i, from = DateBreak, to = Sys.Date(),             
                                           periodicity = "weekly", auto.assign = F)[,6])  
  
  TargetPrice <- unlist(get_summary_table(i)[16,2])
  if (TargetPrice != "N/A"){
    TargetPrice <- as.numeric(gsub(",","",TargetPrice))
    CurrentPrice <- as.numeric(gsub(",","",unlist(get_summary_table(i)[1,2])))
    yret <- (TargetPrice-CurrentPrice)/CurrentPrice 
    wret <- (1+yret)^(1/52) - 1
    ReturnsVec <- c(ReturnsVec, wret)
  }else{
    Trigger = 1
  }
}   

#Find Weekly Returns
Returnsw <- Return.calculate(AdjustedPrices, method = "discrete")
Returnsw <- Returnsw[-1,]

#Minimum Variance Portfolio
sigma <- cov(Returnsw)
weights_mv <- optimalPortfolio(Sigma = sigma, 
                               control = list(type = "minvol", constraint = "lo"))

#Efficient Frontier
if (Trigger == 1){
  ReturnsVec <- apply(Returnsw, 2, mean) 
}
ret_min <- sum(ReturnsVec*weights_mv)
ret_max <- max(ReturnsVec)
ret_range <- seq(from = ret_min, to = ret_max, length.out = 30)

vol <- rep(NA, 30)
mu <- rep(NA,30)
eweights <- matrix(NA, nrow = length(tickersw), ncol = 30)

#Min Weights
eweights[,1] <- weights_mv
vol[1] <- sqrt(tcrossprod(crossprod(weights_mv, sigma), weights_mv))
mu[1] <- ret_min

#Max Weights
max_ret_idx <- which(ReturnsVec == ret_max)
w_maxret <- rep(0,length(tickersw))
w_maxret[max_ret_idx] <- 1

eweights[,30] <- w_maxret
vol[30] <- apply(Returnsw,2,sd)[max_ret_idx]
mu[30] <- ReturnsVec[max_ret_idx]

#Rest of Weights
for (i in 2:29){
  res <- solve.QP(Dmat = sigma, dvec = rep(0,length(tickersw)), Amat = cbind(matrix(rep(1,length(tickersw)), ncol=1), diag(length(tickersw)), matrix(ReturnsVec, ncol=1)), bvec = c(1,rep(0,length(tickersw)), ret_range[i]), meq = 1)
  w <- res$solution
  
  eweights[,i] <- w
  vol[i] <- sqrt(tcrossprod(crossprod(w,sigma),w))
  mu[i] <- sum(ReturnsVec*w)
}

#My Weights
USDtoCAD <- getQuote("CAD=X", src = "yahoo")[2] #Convert USD to CAD
USDtoCAD <- USDtoCAD[[1]] 

#Select Last Prices (From Tickers)
PortfolioPricesw <- NULL 
tickersw <- toupper(tickersw) #CAPS
for (i in tickersw){
  PortfolioPricesw <- cbind(PortfolioPricesw, getQuote(i, src = "yahoo")[,2])          
}  

#Convert USD Denominated Assets to CAD
for (i in 1:length(PortfolioPricesw)){
  if(CountryVecw[i] == "United States"){
    PortfolioPricesw[i] <- USDtoCAD*PortfolioPricesw[i]
  }
}

#Find Weights
MarketValuew <- SharesVecw*PortfolioPricesw
Weightsw <- MarketValuew/sum(MarketValuew)
Weightsw <- as.vector(Weightsw)

MyMu <- sum(ReturnsVec*Weightsw)
MyVol <- as.numeric(sqrt(tcrossprod(crossprod(Weightsw,sigma),Weightsw)))

eweights <- round(eweights,2)
eweights <- t(eweights)
colnames(eweights) <- gsub(".Adjusted", "", colnames(sigma))
eweights <- abs(eweights[c(1,5,8,12,16,19,23,26,30),])

#Graph
MYPLOT <- ggplot(as.data.frame(cbind(vol,mu)), aes(vol, mu)) +
            geom_line() +
            geom_point(aes(MyVol,MyMu, colour = "My Portfolio"), 
                       shape = 18, 
                       size = 3) +
            ggtitle("Efficient Frontier") +
            xlab("Volatility (Weekly)") +
            ylab("Expected Returns (Weekly)") +
            theme(plot.title = element_text(size=14, face="bold.italic", hjust = 0.5, margin=margin(0,0,15,0)),
                  axis.title.x = element_text(size = 10, margin=margin(15,0,0,0)),
                  axis.title.y = element_text(size = 10, margin=margin(0,15,0,0)),
                  panel.border = element_rect(colour = "black", fill=NA, size=1),
                  legend.position = c(0.92,0.06),
                  legend.title = element_blank(),
                  legend.text = element_text(size=8),
                  legend.background = element_rect(color = "black"),
                  legend.key=element_blank())

return(list(MYPLOT, eweights))
}


#Monte Carlo Function
Run3 <- function(tickersw, SharesVecw, CountryVecw){

AdjustedPrices <- NULL
TargetPrice <- NULL
CurrentPrice <- NULL
yret <- NULL
wret <- NULL
ReturnsVec <- NULL

#Scrape Data From Yahoo Finance Homepage
get_summary_table <- function(symbol){
  
  url <- paste0("https://finance.yahoo.com/quote/",symbol)
  df <- url %>%
    read_html() %>%
    html_table(header = FALSE) %>%
    map_df(bind_cols) %>%
    as_tibble()
  
  names(df) <- c("name", "value")
  df["stock"] <- symbol
  
  df
}

#Find Furthest Possible Starting Date of Portfolio
for (i in tickersw){
  AdjustedPrices <- cbind(AdjustedPrices, 
                          getSymbols.yahoo(i, from = "1995-01-01", to = Sys.Date(),             
                                           periodicity = "weekly", auto.assign = F)[,6])  
}  

count1 <- matrix(NA, nrow = length(AdjustedPrices[,1]), ncol = length(AdjustedPrices[1,]))
for (i in 1:length(AdjustedPrices[1,])){
  if (is.na(AdjustedPrices[1,i] == TRUE)){
    for (j in 1:length(AdjustedPrices[,1])){
      if (is.na(AdjustedPrices[j,i] == TRUE)){
        count1[j,i] <- TRUE
      }else{
        break
      }
    }
  }
}

count2 <- vector("numeric", length(AdjustedPrices[1,]))
for (k in 1:length(AdjustedPrices[1,])){
  count2[k] <- sum(count1[,k], na.rm = TRUE)
}

FinalCount <- max(count2)
APrices <- fortify.zoo(AdjustedPrices)
DateBreak <- APrices[FinalCount+1,1]

#Find Historical Adjusted Prices and 1-Y Target Prices (If Available)
Trigger <- 0
AdjustedPrices <- NULL
for (i in tickersw){
  AdjustedPrices <- cbind(AdjustedPrices, 
                          getSymbols.yahoo(i, from = DateBreak, to = Sys.Date(),             
                                           periodicity = "weekly", auto.assign = F)[,6])  
  
  TargetPrice <- unlist(get_summary_table(i)[16,2])
  if (TargetPrice != "N/A"){
    TargetPrice <- as.numeric(gsub(",","",TargetPrice))
    CurrentPrice <- as.numeric(gsub(",","",unlist(get_summary_table(i)[1,2])))
    yret <- (TargetPrice-CurrentPrice)/CurrentPrice 
    wret <- (1+yret)^(1/52) - 1
    ReturnsVec <- c(ReturnsVec, wret)
  }else{
    Trigger = 1
  }
}   

#Find Weekly Returns
Returnsw <- Return.calculate(AdjustedPrices, method = "discrete")
Returnsw <- Returnsw[-1,] #Removes NA

#Minimum Variance Portfolio
sigma <- cov(Returnsw)
weights_mv <- optimalPortfolio(Sigma = sigma, 
                               control = list(type = "minvol", constraint = "lo"))

#Efficient Frontier
if (Trigger == 1){
  ReturnsVec <- apply(Returnsw, 2, mean) #Use historical returns if target price doesnt exist in yahoo finance
}
ret_min <- sum(ReturnsVec*weights_mv)
ret_max <- max(ReturnsVec)
ret_range <- seq(from = ret_min, to = ret_max, length.out = 30)

vol <- rep(NA, 30)
mu <- rep(NA,30)
eweights <- matrix(NA, nrow = length(tickersw), ncol = 30)

#Min Weights
eweights[,1] <- weights_mv
vol[1] <- sqrt(tcrossprod(crossprod(weights_mv, sigma), weights_mv))
mu[1] <- ret_min

#Max Weights
max_ret_idx <- which(ReturnsVec == ret_max)
w_maxret <- rep(0,length(tickersw))
w_maxret[max_ret_idx] <- 1

eweights[,30] <- w_maxret
vol[30] <- apply(Returnsw,2,sd)[max_ret_idx]
mu[30] <- ReturnsVec[max_ret_idx]

#Rest of Weights
for (i in 2:29){
  res <- solve.QP(Dmat = sigma, dvec = rep(0,length(tickersw)), Amat = cbind(matrix(rep(1,length(tickersw)), ncol=1), diag(length(tickersw)), matrix(ReturnsVec, ncol=1)), bvec = c(1,rep(0,length(tickersw)), ret_range[i]), meq = 1)
  w <- res$solution
  
  eweights[,i] <- w
  vol[i] <- sqrt(tcrossprod(crossprod(w,sigma),w))
  mu[i] <- sum(ReturnsVec*w)
}

#Monte Carlo Simulation
W_Vec <- matrix(NA, nrow = length(tickersw), ncol = 1000)
VOL <- rep(NA, 1000)
MU <- rep(NA,1000)

for (i in 1:1000){
  W_Vec[,i] <- runif(length(tickersw)) 
  W_Vec[,i] <- W_Vec[,i]/sum(W_Vec[,i]) 
  MU[i] <- sum(ReturnsVec*W_Vec[,i])
  VOL[i] <- sqrt(tcrossprod(crossprod(W_Vec[,i],sigma),W_Vec[,i]))
}

#My Weights
USDtoCAD <- getQuote("CAD=X", src = "yahoo")[2] #Convert USD to CAD
USDtoCAD <- USDtoCAD[[1]] 

#Select Last Prices (From Tickers)
PortfolioPricesw <- NULL 
tickersw <- toupper(tickersw) #CAPS
for (i in tickersw){
  PortfolioPricesw <- cbind(PortfolioPricesw, getQuote(i, src = "yahoo")[,2])          
}  

#Convert USD Denominated Assets to CAD
for (i in 1:length(PortfolioPricesw)){
  if(CountryVecw[i] == "United States"){
    PortfolioPricesw[i] <- USDtoCAD*PortfolioPricesw[i]
  }
}

#Find Weights
MarketValuew <- SharesVecw*PortfolioPricesw
Weightsw <- MarketValuew/sum(MarketValuew)
Weightsw <- as.vector(Weightsw)

MyMu <- sum(ReturnsVec*Weightsw)
MyVol <- as.numeric(sqrt(tcrossprod(crossprod(Weightsw,sigma),Weightsw)))

eweights <- round(eweights,2)
eweights <- t(eweights)
colnames(eweights) <- gsub(".Adjusted", "", colnames(sigma))
eweights <- abs(eweights[c(1,5,8,12,16,19,23,26,30),])

#Graph
MYPLOT <- ggplot(as.data.frame(cbind(VOL,MU)), aes(VOL, MU)) +
            geom_point(shape = 1) +
            geom_point(aes(MyVol,MyMu, colour = "My Portfolio"), 
                       shape = 18, 
                       size = 3) +
            geom_line(data=data.frame(vol,mu), mapping=aes(vol, mu)) +
            ggtitle("Efficient Frontier") +
            xlab("Volatility (Weekly)") +
            ylab("Expected Returns (Weekly)") +
            theme(plot.title = element_text(size=14, face="bold.italic", hjust = 0.5, margin=margin(0,0,15,0)),
                  axis.title.x = element_text(size = 10, margin=margin(15,0,0,0)),
                  axis.title.y = element_text(size = 10, margin=margin(0,15,0,0)),
                  panel.border = element_rect(colour = "black", fill=NA, size=1),
                  legend.position = c(0.92,0.06),
                  legend.title = element_blank(),
                  legend.text = element_text(size=8),
                  legend.background = element_rect(color = "black"),
                  legend.key=element_blank())

return(list(MYPLOT, eweights))
}
    
shinyApp (ui = ui, server = server)
