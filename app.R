## ASSUMPTIONS ##

#All Data From Yahoo Finance
#Adjusted Closing Prices Considered to Include Dividends Into Returns Calculations
#Discrete Compounding for Rate of Change
#Geometric Average for Returns
#Proxy for S&P500 Returns -> SPY (To Take Dividends Into Account)
#Proxy for S&P/TSX Returns -> XIC.TO (To Take Dividends Into Account)
#Proxy for MSCI EAFE IMI Returns -> XEF.TO (To Take Dividends Into Account). Note The Impact of Currency Fluctuation**
#Proxy for MSCI Emerging Markets Returns -> XEC.TO (To Take Dividends Into Account). Note The Impact of Currency Fluctuation**
#CAPM Assumptions: Benchmark = S&P500, Risk Free Rate = Treasury Yield 10 Years


## WARNINGS ##

#To Maintain Accurate Results
    #Do Not Include Short Positions
    #Only Pick Stocks, ETFs and/or Mutual Funds (No Futures, No Gold, No Cryptocurrencies, etc)
    #All Assets in the Portfolio Have to be Trading in the Same Country 
        #(Different Exchanges is Fine)
    #Add Entries to Rebalancing in Chronological Order


library(shiny)
library(quantmod)                            
library(PerformanceAnalytics)
library(zoo)
library(xts)
library(plyr)
library(dygraphs)


#User Interface
ui <- shinyUI(fluidPage(
    
    includeCSS("dy-graph.css"), #Include CSS File
    
    titlePanel("Portfolio Analysis Tool"),
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
            textInput("Stock","Ticker"),
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
            textInput("Stock1", "Ticker"),
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
    )
))


#Server
server <- shinyServer(function(input, output) {
    
    #Store Initial Tickers/Number of Shares From User Inputs (In Vectors and Data Frame)
    valuesDF <- reactiveValues() #Initialize Data Frame
    valuesDF$df <- data.frame("Stock" = numeric(0), "Shares" = numeric(0)) 
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
    valuesREB$REB <- data.frame("Stock" = numeric(0), "Shares" = numeric(0), "Date" = numeric(0)) 
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
        
        #Compute Annualized Portfolio Returns
        AssetsReturns <- na.omit(ROC(PortfolioPrices, type = "discrete"))                                  
        PortfolioReturns <- Return.portfolio(AssetsReturns, weights = Weights)                             
        PortfolioAnnualizedReturns <- table.AnnualizedReturns(PortfolioReturns, scale = nrow(PortfolioReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(PortfolioReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))                       
        colnames(PortfolioAnnualizedReturns) <- "Portfolio"
        
        #Compute Annualized S&P500 Returns
        SP500 <- getSymbols.yahoo("SPY", from = FromDate, to = ToDate, auto.assign = F)[,6]       
        SP500Returns <- na.omit(ROC(SP500, type = "discrete"))                                             
        SP500AnnualizedReturns <- table.AnnualizedReturns(SP500Returns, scale = nrow(SP500Returns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(SP500Returns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))))) 
        colnames(SP500AnnualizedReturns) <- "S&P500"
        
        #Compute Annualized S&P/TSX Returns
        SPTSX <- getSymbols.yahoo("XIC.TO", from = FromDate, to = ToDate, auto.assign = F)[,6]   
        SPTSXReturns <- na.omit(ROC(SPTSX, type = "discrete"))                                             
        SPTSXAnnualizedReturns <- table.AnnualizedReturns(SPTSXReturns, scale = nrow(SPTSXReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(SPTSXReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))  
        colnames(SPTSXAnnualizedReturns) <- "S&P/TSX"
        
        #Compute Annualized MSCI EAFE IMI Returns
        MSCIIMI <- getSymbols.yahoo("XEF.TO", from = FromDate, to = ToDate, auto.assign = F)[,6]   
        MSCIIMIReturns <- na.omit(ROC(MSCIIMI, type = "discrete"))                                             
        MSCIIMIAnnualizedReturns <- table.AnnualizedReturns(MSCIIMIReturns, scale = nrow(MSCIIMIReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(MSCIIMIReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))))) 
        colnames(MSCIIMIAnnualizedReturns) <- "MSCI EAFE IMI"
        
        #Compute Annualized MSCI Emerging Markets Returns
        MSCIEME <- getSymbols.yahoo("XEC.TO", from = FromDate, to = ToDate, auto.assign = F)[,6]   
        MSCIEMEReturns <- na.omit(ROC(MSCIEME, type = "discrete"))                                             
        MSCIEMEAnnualizedReturns <- table.AnnualizedReturns(MSCIEMEReturns, scale = nrow(MSCIEMEReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y'))), Rf = Rf/(nrow(MSCIEMEReturns)/(as.numeric(format(ToDate,'%Y')) - as.numeric(format(FromDate,'%Y')))))  
        colnames(MSCIEMEAnnualizedReturns) <- "MSCI Emerging Markets"
        
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
        
        #Annualized Returns, Std Dev and Sharpe Ratio (UI Output)
        ReturnsTABLE <- cbind(PortfolioAnnualizedReturns,SP500AnnualizedReturns,SPTSXAnnualizedReturns,MSCIIMIAnnualizedReturns,MSCIEMEAnnualizedReturns)
        
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
    
})
    
shinyApp (ui = ui, server = server)