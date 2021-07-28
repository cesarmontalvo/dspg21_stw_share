library(shiny)
library(forcats)
library(plotly)

#data
merged_cert_fam <- read.csv("~/STW/BGT/dashboard/merged_cert_fam.csv" ) 

ui <- fluidPage(
  
  selectInput('family', "Occupation Family",
              choices = list(
                'Management' = '11' ,
                'Business and Financial Operations' = '13' ,
                'Architecture and Engineering' = '17' ,
                'Life, Physical, and Social Science' = "19" ,
                'Arts, Design, Entertainment, Sports, and Media' = "27" ,
                'Healthcare Practitioners and Technical' = "29" ,
                'Protective Service' = "33" ,
                'Food Preparation and Serving Related' = "35" ,
                'Office and Administrative Support' = "43" ,
                'Farming, Fishing, and Forestry' = "45" ,
                'Construction and Extraction' = "47" ,
                'Installation, Maintenance, and Repair' = "49" ,
                'Production' = "51" ,
                'Transportation and Material Moving' = "53"
                             ),
              selected = "49"),
  
  #plotlyOutput("plot_20top_certificates", width = "100%")
  plotlyOutput('plot_20top_certificates' , width = "auto" ,  height = "700")
)

server <- function(input, output, session) {
  
  
  
  output$plot_20top_certificates <- renderPlotly({
    
  #plot for data
  dat <- merged_cert_fam %>% filter(fam==input$family, 
                                    certification != "Driver's License",
                                    certification != "CDL Class A",
                                    certification != "Cdl Class B",
                                    certification != "Cdl Class C",
                                    certification != "CDL Class D",
                                    certification != "Security Clearance"
  ) %>%
    arrange(desc(number)) %>%
    slice(1:30) 
  
  #plot
  plot_cert <- ggplot(data = dat, mapping = aes(x = reorder( factor(name_cert), number), number)) + 
    geom_bar(stat = "identity") + coord_flip()+
    labs(x='', y='Number',
         #title = 'Top 20 Certifications for   Occupation')
          title = paste('Top 30 Certifications for', 'Occupation') )
  
  
  plot_cert
   #ggplotly(plot_cert)   
  })
  
  
  
  
}

shinyApp(ui, server)
