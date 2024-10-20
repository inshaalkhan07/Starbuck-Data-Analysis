# Importing all required libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(Hmisc)
library(lessR)
library(stringr)
library(lubridate)
library(plotly)
library(fmsb)
library(multipanelfigure)
library(gridExtra)
library(patchwork)
library(egg)
library(ggthemes)
library(gganimate)
library(htmlwidgets)
library(readxl)
library(shiny)
library(ggbeeswarm)
library(ggiraph)
library(rsconnect)

rsconnect::setAccountInfo(name='vinaysridhar31', token='491401209F663FDFB597663210A71529', secret='iI1UFvFVAAnOS6qFXf6tt030Qxv2LvnihOYOkBTH')
rsconnect::deployApp()



# Importing the dataset
setwd("/Users/vinaysridhar/Documents/Vinay/Northeastern University/Sem 2 Notes/Comp Viz/Project 1")
getwd()
# Importing the dataset
df <- read.csv('Locations.csv')
head(df)


df1 <- df

df1$gender[df1$gender == 'F'] <-'Female'
df1$gender[df1$gender == 'M'] <-'Male'
df1$gender[df1$gender == 'O'] <-'Other'

fig1 <- df1 %>%
  plot_ly(
    y = ~age,
    type = 'violin',
    color = ~gender,
    colors = c("#00704A","#088F8F","#5F9EA0"),
    box = list(
      visible = T
    )
  ) 
fig1 <- fig1 %>%
  layout(
    xaxis = list(
      title = "Genders"
    ),
    yaxis = list(
      title = "Ages"
    )
    )

fig1 <- fig1 %>% layout(title = "Distribution of Ages of Starbucks Customers")

fig1
htmlwidgets::saveWidget(fig1, "ViolinPlot_Project1.html")

Sys.setenv("plotly_username"= "vinaysridhar31")
Sys.setenv("plotly_api_key" = "eqZFFZRFeu0q9w1xqtla")

api_create(fig1, "Violin Plot")

df2 <- df
fig2 <- plot_ly(
  data = df2,
  y = ~income,
  nbinsy = 9,
  type = "histogram",
  marker = list(color = "#088F8F",
                line = list(color = "black",
                            width = 2.8))
) %>% 
  layout(title = "Distribution of Incomes of Starbucks Customers",
         yaxis = list(title = "Income",
                      zeroline = FALSE),
         xaxis = list(title = "Number of Customers",
                      zeroline = FALSE))


fig2

htmlwidgets::saveWidget(fig2, "ColumnChart_Project1.html")

Sys.setenv("plotly_username"= "vinaysridhar31")
Sys.setenv("plotly_api_key" = "eqZFFZRFeu0q9w1xqtla")

api_create(fig2, "Column Chart")






# Group by year and get the count of rows
yearly_counts <- aggregate(df$Year, by=list(df$Year), FUN=length)

# Rename the column with the count of rows
colnames(yearly_counts)[2] <- "count"

# Save the result as a new CSV file
write.csv(yearly_counts, "Processed_Customers.csv", row.names=FALSE)

df2 <- read_excel('Book1.xlsx')
head(df2)

# Group by year and get the count of rows
yearly_counts <- aggregate(df2$Country, by=list(df2$`Ownership Type`), FUN=length)

# Rename the column with the count of rows
colnames(yearly_counts)[2] <- "count"

# Save the result as a new CSV file
write.csv(df2, "Processed_Locations.csv", row.names=FALSE)


ownership_type = df$`Ownership.Type`
profits = df$Profits

# Define UI
ui <- fluidPage(
  headerPanel(
    tags$h2("Profits based on Ownership Type")
  ),
  mainPanel(
    plotOutput("profit_plot")
  )
)

  
  # Define server
  server <- function(input, output) {
    
    # Create a grouped bee swarm plot of profits by ownership type
    output$profit_plot <- renderPlot({
      ggplot(df, aes(x = ownership_type, y = profits, color = ownership_type)) +
        geom_quasirandom(alpha = 0.5) +
       scale_color_manual(values = c("#005C29","#228B22","#00A36C","#40826D"),name = "Ownership Type") +
        labs(x = "Ownership Type", y = "Profits") +
        theme(legend.position = "bottom",axis.title = element_text(size = 16),
              axis.text = element_text(size = 14)) + theme_bw()
    })
    
  }

# Run the app
shinyApp(ui = ui, server = server)


yearly_counts <- aggregate(df$Preferred.Drink, by=list(df$Preferred.Drink), FUN=length)

# Rename the column with the count of rows
colnames(yearly_counts)[2] <- "count"

# Save the result as a new CSV file
write.csv(yearly_counts, "Processed_Customers1.csv", row.names=FALSE)

df6 <- read_csv('Processed_Customers1.csv')
head(df6)
df6$count <- as.integer(df6$count)
df6 <- df6 %>% arrange(desc(count))
head(df6)
drink = df6$Group.1
counter = df6$count

# Define UI
ui <- fluidPage(
  headerPanel(
    tags$h2("Drinks preferred by the Starbucks Customers")
  ),
  mainPanel(
    plotOutput("line_plot")
  )
)


# Define server
server <- function(input, output) {
  
  
  # Render line plot
  output$line_plot <- renderPlot({
    ggplot(df6, aes(x = Group.1, y = counter, group = 1)) +
      geom_line(color = "#00A36C", size = 1.5) + geom_point(color = "#005C29", size = 3.5) +
      labs(x = "Preferred Drinks", y = "Count") +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
