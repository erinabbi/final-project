library(shiny)
library(tidyverse)
library(DT)

billionaire <- read_csv("C:/Users/westo17/OneDrive - University of Kansas/Class/824 Data Vis/final-project/Billionaires Statistics Dataset.csv")

billionaire$finalWorth <- (billionaire$finalWorth)/1000

billionaire <- billionaire %>%
  mutate(income_cat = case_when(
    finalWorth < 5 ~ "$1-$4B",
    finalWorth >= 5 & finalWorth < 10 ~ "$5-$9B",
    finalWorth >= 10 & finalWorth < 25 ~ "$10-$24B",
    finalWorth >= 25 & finalWorth < 50 ~ "$25-$49B",
    finalWorth >= 50 & finalWorth < 100 ~ "$50-$99B",
    finalWorth >= 100 ~ "$100B or More"))

billionaire$income_cat <- factor(billionaire$income_cat, levels = c("$1-$4B", "$5-$9B",
                                                                    "$10-$24B", "$25-$49B", "$50-$99B",
                                                                    "$100B or More"))

top_20 <- billionaire %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  drop_na(country) %>%
  slice_head(n = 10)

bill_data <- billionaire %>%
  semi_join(top_20, by = "country")

bill_data$country <- as.factor(bill_data$country)
countries <- levels(bill_data$country)

### PLAYING AROUND
total_worth <- bill_data %>%
  group_by(country) %>%
#  summarise("Total Billionaire Worth for Country" = sum(finalWorth))
  summarise(total_billions = sum(finalWorth)) %>%
  mutate("Total Billionaire Worth for Country" = total_billions) %>%
  select(!total_billions)

bill_data$population_country <- (bill_data$population_country)/1000000 

tax_rate <- bill_data %>%
  select(country, total_tax_rate_country) %>%
  unique() %>%
  arrange(country)
tax_rate$total_tax_rate_country[is.na(tax_rate$total_tax_rate_country)] <- "Unknown"

bill_rate <- bill_data %>%
  group_by(country, population_country) %>%
  summarise(n = n()) %>%
  mutate("Billionaires per 1M Population" = round(n/population_country, 2)) %>%
  select(country, 'Billionaires per 1M Population')
bill_rate$'Billionaires per 1M Population'[is.na(bill_rate$'Billionaires per 1M Population')] <- "Unknown"

highest_bill <- bill_data %>%
  group_by(country) %>%
  slice_max(finalWorth)

highest_bill <- highest_bill %>%
  mutate(string = paste0(personName, ", ", industries, ", $", finalWorth, "B" )) %>%
  filter(personName != "Zhang Congyuan") %>%
  select(country, string)

bill_sum <- bind_cols(total_worth, bill_rate, tax_rate, highest_bill)
bill_sum <- bill_sum %>%
  select(country...1, `Total Billionaire Worth for Country`, `Billionaires per 1M Population`,
         total_tax_rate_country, string)
colnames(bill_sum) <- c("Country", "Combined Billionaire Worth for Country (in billions of USD)", "Billionaires per 1M Population", "Total Tax Rate", "Country's Highest Worth Billionaire")

#industry_categories <- levels(billionaire$industries)

ui <- fluidPage(
  
  titlePanel("Billionaire and Wealth Data for 10 World Countries"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("countries", 
                   label = "Select Country of Interest",
                   choices = c(countries),
                   selected = "United States")
    ),
    
    mainPanel(
      
      plotOutput("hist"),
    #  tableOutput("table")
    DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    bill_data <- bill_data %>% 
      filter(country == input$countries)
    #      filter(country == input$countries
    #   if(input$countries == "All Countries") {
    #    bill_data
    # } else {
    #   bill_data <- bill_data %>% 
    #      filter(country == input$countries)
    #  }
  })
  
  #filtered_table <- reactive({
  #  
  #  bill_sum <- bill_sum %>%
  #    filter(Country == input$countries) %>%
  #    select()
  #})
  
  output$hist <- renderPlot ({
    
    filtered_data() %>%
      ggplot(aes(x = income_cat)) +
      geom_bar(color = "white", fill = "forestgreen") +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(limits = c(0, 550)) +
      labs(x = "Net Worth",
           y = "Count",
           title = paste("Distribution of Billionaire Worth for", input$countries)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20, face = "bold"),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
  })
  
  #output$table <- renderTable(filtered_table())
 output$table <- renderDT(datatable(bill_sum, rownames = FALSE))

}

shinyApp(ui, server)
