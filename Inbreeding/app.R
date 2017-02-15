#==============================================================================#
# Falconer Chapter 5: Inbreeding Coefficients
#==============================================================================#

library(shiny)
library(pedigreemm)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

#==============================================================================#
# Setup Page
#==============================================================================#

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Calculate Inbreeding Coefficients"),
  
  HTML("
    <p>
    This Shiny App was developed by Austin Putz at Iowa State University. <br>
    Please contact me with problems <a href=\"mailto:aputz@iastate.edu\">aputz@iastate.edu</a>
    </p>

    <p>
    <img src=\"istate_logo.jpg\" height=150 width=150 />
    </p>

    <p>
    The use is to calculate pedigree information, such as inbreeding,
    number of offspring, and more will be added as I go. 
    Not tested with large pedigrees, not sure how many animals it can handle. 
    So far it can:
    <ol>
      <li>Edits your pedigree</li>
      <li>Calculate Inbreeding Coefficients (F)</li>
      <li>Calculate the number of full and half-sibs</li>
      <li>Calculate the number of offspring for sires and dams</li>
    </ol>
    </p>
    "),
  
  tags$h2("Input"),
  
  # take file input
    fluidRow(column(6,
  wellPanel(
  fileInput('file1', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'))))
  ), # END wellPanel()
  
  tags$h2("Output"),
  
  # Show the raw pedigree
  tags$h3("Raw Pedigree (0 is missing but replaced in R)"),
  fluidRow(column(6,
  dataTableOutput("output_data"))),
  
  #----------------------------------------#
  # Inbreeding Coefs
  #----------------------------------------#
  
  actionButton("button", "Run Inbreeding Coefficients", icon("refresh"), 
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  
  tags$h3("Dataset with inbreeding coefficients"),
  
  fluidRow(column(4,
  dataTableOutput("output_data2"))),

  downloadButton('downloadInbreeding', 'Download Inbreeding'),

  downloadButton('downloadRelationships', 'Download Relationships'),
  
  wellPanel(
  tags$h3("Histogram of Inbreeding Coefficients"),
  
  fluidRow(column(8,
  plotOutput("coefs_plot"))),
  
  tags$h3("Histogram of Full Sibs"),
  
  fluidRow(column(8,
  plotOutput("n_full_sib_plot"))),
  
  tags$h3("Histogram of Paternal Half-Sibs (PHS)"),
  
  fluidRow(column(8,
  plotOutput("n_PHS_plot"))),
  
  tags$h3("Histogram of Maternal Half-Sibs (MHS)"),
  
  fluidRow(column(8,
  plotOutput("n_MHS_plot")))
  ),
  
  #----------------------------------------#
  # Offspring Counts
  #----------------------------------------#
  
  wellPanel(
  tags$h3("Number of offspring"),
    
  tags$h4("Histogram of Number of Sire Offspring"),
  fluidRow(column(8,
  plotOutput("sire_off_plot"))),
  
  tags$h4("Histogram of Number of Dam Offspring"),
  fluidRow(column(8,
  plotOutput("dam_off_plot")))
  
  )

)

#==============================================================================#
# Server
#==============================================================================#

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #----------------------------------------#
  # Read in Data
  #----------------------------------------#
  
  myData <- reactive({
    
    # set the input file
    inFile <- input$file1
    
    # read the file in
    if (is.null(inFile)) return(NULL)
    
    # read in csv file
    data <- read.csv(inFile$datapath, header=TRUE, na.strings=c("NA", "0", ""))
    
    # set the 0's to NA for the pedigreemm package
    # data[data==0] <- NA
    data
    
  })
  
  #----------------------------------------#
  # Output Data
  #----------------------------------------#
  
  # output the raw pedigree in Data Table
  output$output_data <- renderDataTable({
    myData()
  })
  
  #----------------------------------------#
  # Run Inbreeding
  #----------------------------------------#
  
  # use button to trigger the rest to happen
  observeEvent(input$button, {
    
    # start ped section (dataset)
    inbred <- reactive({
  
      # set data from above that was read in with csv()
      ped <- myData()
      
      # subset first three columns
      ped <- ped[, 1:3]
      
      # rename columns
      names(ped) <- c("Animal", "Sire", "Dam")
      
      # use edit ped to manage dataset
      ped.edit  <- editPed(sire=ped[,2], 
                           dam=ped[,3], 
                           label=ped[,1])
      
      # set pedigree
      ped.comp <- pedigree(ped.edit$sire, 
                           ped.edit$dam, 
                           ped.edit$label)
      
      # recreate new pedigree
      ped.2           <- data.frame(Animal=ped.comp@label)
      ped.2$SireRenum <- ped.comp@sire
      ped.2$DamRenum  <- ped.comp@dam
      
      # get inbreeding coefs
      ped.2$Inbreeding <- round(inbreeding(ped.comp), 4)
      
      # merge to old pedigree for sires and dams
      ped.3 <- merge(ped.2, ped, by="Animal", all.x=TRUE)
      
      # order dataset
      ped.3 <- ped.3[order(ped.3$Animal), ]
      
      # Add Sire_Dam column
      ped.3$SireDam <- with(ped.3, paste(Sire, Dam, sep="_"))
      
      #----------------------------------------#
      # Get Full-Sib information
      #----------------------------------------#

      # calculate # full sibs
      full.sibs <- ped.3 %>% group_by(Sire, Dam) %>% summarise(FullSibs=n()) %>%
                        filter(!is.na(Sire)) %>%
                        filter(!is.na(Dam)) %>%
                        unite(SireDam, Sire, Dam, sep="_") %>%
                        mutate(FullSibs=FullSibs-1) %>%
                        filter(FullSibs>0) %>%
                        as.data.frame()

      # merge full sibs to pedigree
      ped.3 <- merge(ped.3, full.sibs, by="SireDam", all.x=TRUE)

      # set those NA to 0 in number of full sib column
      ped.3$FullSibs[is.na(ped.3$FullSibs)] <- 0

      #----------------------------------------#
      # Get Half-Sib information
      #----------------------------------------#

      # calc number of sire offspring
      sire.hs <- ped.3 %>% group_by(Sire) %>% summarise(nSharedSire=n()) %>%
                  filter(!is.na(Sire)) %>%
                  mutate(nSharedSire=nSharedSire-1) %>%
                  as.data.frame()

      # merge sire half sibs
      ped.3 <- merge(ped.3, sire.hs, by="Sire", all.x=TRUE)

      # calc number of sire offspring
      dam.hs <- ped.3 %>% group_by(Dam) %>% summarise(nSharedDam=n()) %>%
                  filter(!is.na(Dam)) %>%
                  mutate(nSharedDam=nSharedDam-1) %>%
                  as.data.frame()

      # merge dam half sibs
      ped.3 <- merge(ped.3, dam.hs, by="Dam", all.x=TRUE)

      # fill in 0's
      ped.3$nSharedSire[is.na(ped.3$nSharedSire)] <- 0
      ped.3$nSharedDam[is.na(ped.3$nSharedDam)]   <- 0

      ped.3$SireHS <- ped.3$nSharedSire - ped.3$FullSibs
      ped.3$DamHS  <- ped.3$nSharedDam  - ped.3$FullSibs

      ped.3 <- ped.3[, c("Animal", "Sire", "Dam", "Inbreeding",
                          "FullSibs", "SireHS", "DamHS")]
      
      # return the data
      ped.3
      
    })
    
  # output the inbreeding coefs
  output$output_data2 <- renderDataTable({
    inbred()
  })
    
    # start ped section (dataset)
    relationships <- reactive({
  
      # set data from above that was read in with csv()
      ped <- myData()
      
      # subset first three columns
      ped <- ped[, 1:3]
      
      # rename columns
      names(ped) <- c("Animal", "Sire", "Dam")
      
      # use edit ped to manage dataset
      ped.edit  <- editPed(sire  = ped[,2], 
                           dam   = ped[,3], 
                           label = ped[,1])
      
      # set pedigree
      ped.comp <- pedigree(ped.edit$sire, 
                           ped.edit$dam, 
                           ped.edit$label)
      
  
      # create A matrix (3rd ed, page 23) (uses the matrix package, thus the "."s)
      A <- getA(ped.comp)
        
      # set row and column names
      rownames(A) <- ped.comp@label
      colnames(A) <- ped.comp@label
  
      # set to data frame
      A.df <- melt(as.matrix(A))
      
      # change column names
      names(A.df) <- c("Animal1", "Animal2", "Relationship")
      
      A.df
      
    })
  
  # plot the inbreeding coefs
  output$coefs_plot <- renderPlot({
    
    # set inbreeding data
    inbred.data <- inbred()
    
    # plot a histogram of inbreeding coefficients
    ggplot(inbred.data, aes(x=Inbreeding)) +
      geom_histogram(fill="firebrick", color="grey50", binwidth=0.01) +
      xlab("Inbreeding Coefficient (F)") +
      theme(text = element_text(size=16), 
            plot.title=element_text(hjust=0.5))
      
  })
  
  # plot the inbreeding coefs
  output$n_full_sib_plot <- renderPlot({
    
    # set inbreeding data
    ped <- inbred()
    
    # plot a histogram of inbreeding coefficients
    ggplot(ped, aes(x=FullSibs)) +
      geom_histogram(fill="dodgerblue3", color="grey50", binwidth=1) +
      xlab("Number of Full Sibs") +
      theme(text = element_text(size=16), 
            plot.title=element_text(hjust=0.5))
      
  })
  
  # plot the inbreeding coefs
  output$n_PHS_plot <- renderPlot({
    
    # set inbreeding data
    ped <- inbred()
    
    # plot a histogram of inbreeding coefficients
    ggplot(ped, aes(x=SireHS)) +
      geom_histogram(fill="dodgerblue3", color="grey50", binwidth=1) +
      xlab("Number of PHS") +
      theme(text = element_text(size=16), 
            plot.title=element_text(hjust=0.5))
      
  })
  
  # plot the inbreeding coefs
  output$n_MHS_plot <- renderPlot({
    
    # set inbreeding data
    ped <- inbred()
    
    # plot a histogram of inbreeding coefficients
    ggplot(ped, aes(x=DamHS)) +
      geom_histogram(fill="dodgerblue3", color="grey50", binwidth=1) +
      xlab("Number of MHS") +
      theme(text = element_text(size=16), 
            plot.title=element_text(hjust=0.5))
      
  })
  
  output$downloadInbreeding <- downloadHandler(
    filename = function() { paste(input$file1, '.csv', sep='') },
    content = function(file) {
      write.csv(inbred(), file, na="0", row.names=FALSE)
    }
  )

  output$downloadRelationships <- downloadHandler(
    filename = function() { paste(input$file1, '.csv', sep='') },
    content = function(file) {
      write.csv(relationships(), file, row.names=FALSE)
    }
  )

  #----------------------------------------#
  # Run Offspring
  #----------------------------------------#

  # sire # offspring data
  SireOff <- reactive({
      
    data.sire <- myData()
    
    # get number of Sire offspring
    SireOff <- data.sire %>% group_by(Sire) %>% summarise(Offspring=n()) %>%
      as.data.frame()
    SireOff <- SireOff[!is.na(SireOff[, 1]), ]
    SireOff
    
  })
    
  # dam # offspring data
  DamOff <- reactive({
    
    data.dam <- myData()
    
    # get number of Sire offspring
    DamOff <- data.dam %>% group_by(Dam) %>% summarise(Offspring=n()) %>%
      as.data.frame()
    DamOff <- DamOff[!is.na(DamOff[, 1]), ]
    DamOff
    
  })
  
  # sire # offspring plot
  output$sire_off_plot <- renderPlot({
    
    # set inbreeding data
    data.sire <- SireOff()
    
    # plot a histogram of # of sire offspring
    ggplot(data.sire, aes(x=Offspring)) +
      geom_histogram(fill="dodgerblue3", color="grey50", binwidth=1) +
      xlab("Number of Sire Offspring") +
      theme(text = element_text(size=16), 
            plot.title=element_text(hjust=0.5))
      
  })
  
  # dam # offspring plot
  output$dam_off_plot <- renderPlot({

    # set inbreeding data
    data.dam <- DamOff()

    # plot a histogram of # of dam offspring
    ggplot(data.dam, aes(x=Offspring)) +
      geom_histogram(fill="magenta", color="grey50", binwidth=1) +
      xlab("Number of Dam Offspring") +
      theme(text = element_text(size=16),
            plot.title=element_text(hjust=0.5))
  
  })
  
  
  })
}



#==============================================================================#
# Output
#==============================================================================#

# Run the application 
shinyApp(ui = ui, server = server)

















