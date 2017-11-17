#install.packages("rpivotTable")
library(data.table)
library(DT)
library(rpivotTable)

prods <- read.csv("WA_Sales_Products_2012-14.csv", stringsAsFactors=FALSE)

countrySum <- length(unique(prods$Retailer_country))

#data is dummy table containing two values Country and Revenue from main dataset
data = data.table(Country=c(prods$Retailer_country),
                  Revenue=c(prods$Revenue))

#total revenue countries-wise
countryRevSum <- data %>%
  group_by(Country) %>%
  summarise(Revenue = sum(Revenue)) %>% arrange(desc(Revenue))

slices <- countryRevSum$Revenue
View(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(countryRevSum$Country, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels


#bottom two countries as per revenue
leastPerformanceCountries <- tail(countryRevSum,2)

#table to store country, retailertype and revenue value,
#this is will used later to create a pivot table
pivRetRev = data.table(Country=c(prods$Retailer_country),
                       RetailerType=c(prods$Retailer_type),
                       Revenue=c(prods$Revenue))
#View(pivRetRev)

#table to store country, retailertype, ordertype and revenue value,
#this is will used later to create a pivot table
pivOrderMethType = data.table(Country=c(prods$Retailer_country),
                              RetailerType=c(prods$Retailer_type),
                              OrderMethodType=c(prods$Order_method_type),
                              Revenue=c(prods$Revenue))

#table to store country and product category value,
#this is will used later to create a pivot table
#to display the performance of Product category in different countries.
pivCategoryRev = data.table(Country=c(prods$Retailer_country),
                              Category=c(prods$Category),
                              Revenue=c(prods$Revenue))

shinyServer(function(input,output){

  output$countries <- renderValueBox({
    valueBox(length(unique(prods$Retailer_country)),"Countries", icon = icon("globe"), color = "orange")
  })

  output$products <- renderValueBox({
    valueBox(length(unique(prods$Product)),"Products", icon = icon("barcode"), color = "yellow")
  })

  output$piechart <- renderPlot({
    pie3D(countryRevSum$Revenue,labels=paste(lbls,sep = "\n"),
            radius = 1.2, labelcex = 1, cex = 0.3, explode = 1)
  })

  #onhover tooltip functionality for piechart
  output$dynamic <- renderUI({
    req(input$plot_hover)
    verbatimTextOutput("vals")
  })

  output$vals <- renderPrint({
    hover <- input$plot_hover
    lbls
  })

  output$country_rev = DT::renderDataTable({
    DT::datatable(countryRevSum, options = list(pageLength = 7,
                                               lengthMenu = c(7, 14, 21), autoWidth = FALSE),
                   class = 'cell-border stripe') %>% formatStyle(
     'Country','Revenue',
     color='white',background = 'teal',target = 'row'
   )
  })

  output$rpivotRetRev <- renderRpivotTable({
    rpivotTable(pivRetRev,rows=c("Country"),
                cols=c("RetailerType"),
                inclusions = list( Country = leastPerformanceCountries$Country),
                vals = "Revenue", aggregatorName = "Sum", height = "780px")
  })

  output$rpivotOrderMethType <- renderRpivotTable({
    rpivotTable(pivOrderMethType, rows=c("Country", "RetailerType"),
                cols=c("OrderMethodType"),
                inclusions = list( Country = leastPerformanceCountries$Country),
                vals = "Revenue", aggregatorName = "Sum", height = "780px")
  })

  #Product Categorywise revenue
  output$rpivotProdPerf <- renderRpivotTable({
    rpivotTable(pivCategoryRev, rows=c("Country"),
                cols=c("Category"),
                vals = "Revenue", aggregatorName = "Sum", height = "780px")
  })

})
