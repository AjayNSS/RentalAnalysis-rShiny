# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #TAB1 DF - BEGIN
  # Filter DF used on menuitem1
  dfRental_filtered <- reactive({
    return(dfRental %>% 
           filter (TotalBedrooms == input$ddlBedrooms, ListDateYear==input$ddlYear, ZipCode %in% input$ddlZip)
    )
  })
  #TAB2 DF - BEGIN
  
  
  #TAB2 DF - BEGIN 
  #Filter DF used on menu item2
  dfRentalMean_filtered <- reactive({
      dfT1<- dfRental %>% 
             filter (TotalBedrooms == input$m2_ddlBedrooms, ListDateYear==input$m2_ddlYear) %>% 
             group_by(ZipCode, ListDateYear) %>% 
             summarise(mean_rent=mean(LeasePerMonth),ListingCount=n(), .groups = 'drop')  %>% 
             arrange(desc(mean_rent)) 
      
      dfT2 <- merge(x=dfT1,y=dfZipCodeCityArea) %>% 
        arrange(desc(mean_rent))
      
      return( dfT2)
  })
  #TAB2 DF - END 
  
  #TAB3 DF - BEGIN
  # Filter DF used on menuitem3
  dfRental_PctIncrease_filtered <- reactive({
    
    #Get the dataFrame
    PerctIncreaseInRentByZip <- dfRental %>% 
      group_by(ZipCode, ListDateYear) %>% 
      summarise(mean_rent=mean(LeasePerMonth), .groups = 'drop')  %>% 
      group_by(ZipCode) %>% 
      mutate(lag = lag(mean_rent)) %>%
      mutate(pct_change = (mean_rent - lag) / lag * 100 )
    
    
    
    
    #Find % increase in rent by combining City
    PerctIncreaseInRentByZipSub <- merge(x=PerctIncreaseInRentByZip,y=dfZipCodeCityArea) %>% 
      arrange(desc(ListDateYear), desc(pct_change)) 
    
    
    
    
    dfT3 <- PerctIncreaseInRentByZipSub %>% 
            filter (ListDateYear==input$ddlYear_PctIncrease )
    
    
    return( dfT3)
    
  })
  #TAB3 DF - END
  
  #TAB4 DF - BEGIN
  # Filter DF used on menuitem4
  dfRental_OverAllRentComp_filtered <- reactive({
    
    #Get the dataFrame
    dfOverAllRentCompT4 <- dfRental %>% 
      filter (TotalBedrooms == 3, ListDateYear > 2016) %>% 
      group_by(ZipCode, ListDateYear) %>% 
      summarise(mean_rent=mean(LeasePerMonth), .groups = 'drop')  %>% 
      arrange(ZipCode, ListDateYear) %>% 
      pivot_wider(names_from = ZipCode, values_from = mean_rent) %>% 
      select_if(~ !any(is.na(.))) %>% 
      arrange(ListDateYear) %>% 
      pivot_longer(cols = -ListDateYear)  
      #unite(zipCode, Area, col="ZipCodeArea", sep="-")
    
    
    return( dfOverAllRentCompT4)
    
  })
  #TAB3 DF - END
  
  
  #TAB5 DF - BEGIN
  dfRental_PerctIncreaseInRentByZipSub_filtered <- reactive({
   
      dfT5 <- PerctIncreaseInRentByZipSub_Global %>% 
      filter(ListDateYear > 2016)
      selectedZip <- input$ddlZip_T5 
     
      dfT5$mean_rent <- round(dfT5$mean_rent,2)
      dfT5$lag <- round(dfT5$lag,2)
      dfT5$pct_change <- round(dfT5$pct_change,2)
      
      if (input$ddlZip_T5 != "All") {
        dfT5 <- dfT5[dfT5$ZipCode == input$ddlZip_T5,]
      }
      
      if (input$ddlYear_T5 != "All") {
        dfT5 <- dfT5[dfT5$ListDateYear == input$ddlYear_T5,]
      }
      
      if (input$ddlArea_T5 != "All") {
        dfT5 <- dfT5[dfT5$Area == input$ddlArea_T5,]
      }
        
      print(paste0("You have chosen: ", input$ddlZip_T5))
      return( dfT5)
    
  })
  #TAB5 DF - END
  
  output$value <- renderPrint({ input$ddlZip })
  
  

  
  
  #--------------------
  # Plot 1

  
  #=====================End Plot1
  
  
  #----------------------
  # BEGIN - Plot 2 - Box Plot
  
  output$zipBoxPlot <- renderPlot({
      ggplot(dfRental_filtered(), aes(x=as.factor(ZipCode), y=LeasePerMonth, fill = factor(ZipCode))) + 
      geom_boxplot( alpha=0.2, show.legend = FALSE) +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Zip Code") + 
      ylab("Rent") +
      scale_fill_discrete(name="Zip Code") +
      theme(text = element_text(size = 18))  


  } )
  #=====================End Plot2
  
  #----------------------
  # BEGIN - Plot 3 - Bar Plot
  
  output$BarPlotPctIncreaseT3 <- renderPlot({

    ggplot(dfRental_PctIncrease_filtered(), aes(x=as.factor(ZipCode), y=pct_change, fill=Area)) +   
      geom_bar(stat = "identity") +     
      geom_text(aes(label=paste(round(pct_change,2), "%" )), hjust = -0.1, vjust=0.25, size=4.5)+
      #guides(fill=guide_legend(ncol=2)) +
      theme_classic()+
      coord_flip() + 
      ylab("% Change in Rent") + 
      xlab("Zip Code") +
      scale_fill_discrete(name="County Area") + 
      theme(text = element_text(size = 18))  
     
    
      #guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
    
  } )
  
  output$BarPlotPctChangeOverYears_5Yr <- renderPlot({
    
    ggplot(PctChangeOverYears_5YrCityArea, aes(x=as.factor(ZipCode), y=pct_change, fill=Area,  show.legend=FALSE)) +  
     
      geom_bar(stat = "identity") +
      geom_text(aes(label=paste(round(pct_change,2), "%" )), hjust = -0.1, vjust=0.25, size=4.5)+
      ylab("% Change in Rent") +
      xlab("Zip Code") +
      coord_flip() +
      scale_fill_discrete(name="County Area") +
      theme(text = element_text(size = 18))  

    
  } )
  #=====================End Plot3
  
  #----------------------
  # BEGIN - T4-Plot1 - Bar Plot
  output$ptOutOverAllRentComp <- renderPlot({
    
    ggplot(dfRental_OverAllRentComp_filtered(), aes(x = ListDateYear, y = value, group = name, color = name)) +   
      geom_line(size = 1) +   
      theme_classic() + 
      xlab("List Year") + 
      ylab("Mean reant") +
      scale_color_discrete("ZipCode") + 
      theme(text = element_text(size = 18))  

    
  } )
  
  output$BarPlotMeanRentT3 <- renderPlot({
    
    ggplot(dfRental_PctIncrease_filtered(), aes(x=as.factor(ZipCode), y=mean_rent, fill=Area)) +   
      geom_bar(stat = "identity") +
      geom_text(aes(label=paste("$",round(mean_rent,0) )), hjust = -0.1, vjust=0.3, size=4.5)+
      ylab("Mean Rent") + 
      xlab("Zip Code") +
      coord_flip() +
      scale_fill_discrete(name="County Area") +
      theme(text = element_text(size = 18))  
    
    
  } )
  #=====================End T4-Plot1
  
  
  #Set up the DataTable DF sources
  
  output$DisplaydfRental <- renderDataTable(
    dfRental_filtered(),
    options = list(scrollX = TRUE, searching = TRUE),
    
  )
  
  output$DisplaydfMeanRental <- renderDataTable(
    dfRentalMean_filtered(),
    options = list(scrollX = TRUE, searching = FALSE)
  )
  
  #DisplaydfRentalPctIncrease
  output$DisplaydfRentalPctIncrease <- renderDataTable(
    PerctIncreaseInRentByZipSub_Global,
    options = list(scrollX = TRUE, searching = FALSE)
  )
  
  #DisplaydfRentalPctIncrease2022
  output$DisplaydfRentalPctIncrease2022 <- renderDataTable(
    PerctIncreaseInRentByZipSub_Global_2022,
    options = list(scrollX = TRUE, searching = FALSE)
  )
  
  #DisplaydfNashvilleAreas
  output$DisplaydfZipCodeCityArea <- renderDataTable(
    dfZipCodeCityArea,
    options = list(scrollX = TRUE, searching = FALSE)
  )
  
  #temp
  output$DisplaydfOverAllRentCompT4_global <- renderDataTable(
    dfOverAllRentCompT4_global,
    options = list(scrollX = TRUE, searching = FALSE)
  )
  
  
  #temp2

  output$DisplaydfOverAllRentCompT5_global <- renderDataTable(
    dfRental_PerctIncreaseInRentByZipSub_filtered(),
    options = list(scrollX = TRUE, searching = FALSE)
  )
  
  output$PctChangeOverYears_5YrCityArea_global_RDT <- renderDataTable(
    PctChangeOverYears_5YrCityArea,
    options = list(scrollX = TRUE, searching = FALSE)
  )
}