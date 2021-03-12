library(shiny)

library(qmethod)

data(lipset)



shinyServer(function(input, output) {

  output$contents <- renderTable({

    

    # input$file1 will be NULL initially. After the user selects

    # and uploads a file, it will be a data frame with 'name',

    # 'size', 'type', and 'datapath' columns. The 'datapath'

    # column will contain the local filenames where the data can

    # be found.

    

    inFile <- input$file1

    

    if (is.null(inFile))

      return(NULL)

    

    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)

  })

  

  output$rawinfo <- renderText({

    inFile <- input$file1

    if (is.null(inFile))

      return(NULL)

    raw <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)

    paste0("The loaded data have ", nrow(raw)," statements and ", ncol(raw)," Q-sorts")})

  

  output$codeUpload <- renderText({

    inFile <- input$file1

    if (is.null(inFile))

      return("Customised code will display here when some data is uploaded in step 1 above.")

    paste0("library(qmethod)

mydata <- read.csv('", input$file1[[1]], "', 
                   header = ", input$header, ", sep = '", input$sep, "', quote = '", input$quote, "')")

  })

  

  

  output$codeQmethod <- renderText({

    inFile <- input$file1

    if (is.null(inFile))

      return("Customised code will display here when some data is uploaded in step 1 above.")

    paste0("results <- qmethod(mydata, nfactors = ", input$nfactors, ", extraction = ", 

           input$extraction, ", rotation = ", input$rotation, ")")

  })

  

  output$codeSave <- renderText({

    inFile <- input$file1

    if (is.null(inFile))

      return("Customised code will display here when some data is uploaded in step 1 above.")

    paste0("save(results, 'qm_results.RData')")

  })

  

  output$codeReport <- renderText({

    inFile <- input$file1

    if (is.null(inFile))

      return("Customised code will display here when some data is uploaded in step 1 above.")

    paste0("export.qm(results, 'qm_report.txt', style='PQMethod')")

  })

  

  

  output$codePlot <- renderText({

    inFile <- input$file1

    if (is.null(inFile))

      return("Customised code will display here when some data is uploaded in step 1 above.")

    paste0("plot(results, sub='Plot of statement z-scores (filled points: distinguishing items)')

abline(v=0, col='grey')")

  })

  

  output$qmPlot <- renderPlot({

    inFile <- input$file1

    if (is.null(input$file1))

      return(NULL)

    results <- qmethod(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote), nfactors=input$nfactors, rotation=input$rotation, extraction=input$extraction)

    par(mai=c(1,0.8,0,0), omi=c(0,0,0,0))

    plot(results, sub="Plot of statement z-scores")

    abline(v=0, col="grey")

  })

  

  

  output$summary <- renderPrint({

    inFile <- input$file1

    if (is.null(input$file1))

      return("Summary of results will display here when some data is uploaded in step 1 above.")

    results <- qmethod(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote), nfactors=input$nfactors, rotation=input$rotation, extraction=input$extraction)

    cat("\nFactor scores\n")

    print(results[[6]])

  })

  

  

  output$fullResults <- renderPrint({

    inFile <- input$file1

    if (is.null(input$file1))

      return("Summary of results will display here when some data is uploaded in step 1 above.")

    results <- qmethod(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote), nfactors=input$nfactors, rotation=input$rotation, extraction=input$extraction, silent=T)

    print(results, length=max(dim(results[[2]])))

  })

  

  

  output$flaggedqsorts <- renderPrint({

    inFile <- input$file1

    if (is.null(input$file1))

      return("Flagged Q-sorts will display here when some data is uploaded in step 1 above.")

    results2 <- qmethod(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote), nfactors=input$nfactors2, rotation=input$rotation2, extraction=input$extraction2)

    flagqs <- loa.and.flags(results2)

    cat("\nNumber of Q-sorts flagged for each factor:\n")

    print(results2[[7]][[1]]["nload"])

    cat("\n")

    print(flagqs)

  })

  

  

  output$factorsel <- renderPrint({

    inFile <- input$file1

    if (is.null(input$file1))

      return("Information to select the number of factors will display here when some data is uploaded in step 1 above.")

    results3 <- qmethod(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote), nfactors=input$nfactors3, rotation=input$rotation3, extraction=input$extraction3)[[7]]

    cat("\nGeneral factor characteristics:\n")

    print(round(results3[[1]], digits=2))

    cat("\nTotal variance explained: ")

    tve <- round(sum(results3[[1]]$expl_var), digits=2)

    cat(tve, "%")

    cat("\n\nCorrelation between factor z-scores:\n")

    print(round(results3[[2]], digits=2))

  })

  

  

  output$screePlot <- renderPlot({

    inFile <- input$file1

    if (is.null(input$file1))

      return(NULL)

    screeplot(prcomp(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)), type="lines", main="Screeplot of unrotated PCA components")

    abline(h=1, col="grey")

  })

  

  

  })