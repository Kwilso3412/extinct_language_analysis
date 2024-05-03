#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Data
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(lessR)

# Visualization
library(shiny)
library(shinydashboard)
library(rsconnect)
library(ggplot2)
library(plotly)
library(shinyWidgets)


#### Exploring the first data set 

# Read in the files 
summary_data_set = read.csv("extinct_languages_data_set.csv")

# Find the total number of each category
endangerment_dataset = count(summary_data_set, degree_of_endangerment)

# Renames the row N to Occurrences
colnames(endangerment_dataset)[colnames(endangerment_dataset) == "n"] = "languages"

# A sum of all the language
endangerment_dataset$total_languages = sum(endangerment_dataset$languages)

# Percentage of each category by the whole 
endangerment_dataset = mutate(endangerment_dataset, percentage = round((languages / total_languages) * 100, 2))

# Create dataframe for the pie chart
pie_chart_df = data.frame(
  degree_of_endangerment = factor(c(
    rep(endangerment_dataset$degree_of_endangerment [1]),
    rep(endangerment_dataset$degree_of_endangerment [2]),
    rep(endangerment_dataset$degree_of_endangerment [3]),
    rep(endangerment_dataset$degree_of_endangerment [4]),
    rep(endangerment_dataset$degree_of_endangerment [5])
  )),
  percentage = c(
    rep(endangerment_dataset$percentage[1]),
    rep(endangerment_dataset$percentage[2]),
    rep(endangerment_dataset$percentage[3]),
    rep(endangerment_dataset$percentage[4]),
    rep(endangerment_dataset$percentage[5])
  )
)

# Stores variable as dataframe 
pie_chart_data = data.frame(pie_chart_df)


#### Exploring the second data set 

# Add Continent
add_continent_column = function(data_frame) {
  # Check if the "countries" column exists
  if ("countries" %in% colnames(data_frame)) {
    # Create a new column for continent
    data_frame$continent <- NA
    
    # Assign continents based on countries
    for (i in 1:nrow(data_frame)) {
      country <- data_frame$countries[i]
      
      if (!is.na(country)) {
        if (country %in% c(
          "Italy", "Germany", "Denmark", "Netherlands", "Poland", "Russian Federation", "Belarus", "Latvia", 
          "Lithuania", "Ukraine", "Switzerland", "Albania", "Austria", "Bosnia and Herzegovina", "Bulgaria", 
          "Croatia", "Estonia", "Finland", "France", "Greece", "Hungary", "The former Yugoslav Republic of Macedonia", 
          "Romania", "United Kingdom of Great Britain and Northern Ireland", "Slovakia", "Slovenia", "Czech Republic", 
          "Turkey", "Serbia", "Montenegro", "Republic of Moldova", "Faroe Islands", "Liechtenstein", "Greenland (Kingdom of Denmark)", "French Polynesia (France)", "Niue", "Pitcairn (U.K.)", "French Guiana (France)", "Pitcairn  (U.K.)"
        )) {
          data_frame$continent[i] = "Europe"
          
        } else if (country %in% c(
          "Algeria", "Niger", "Mali", "Nigeria", "Libya", "Chad", "Tunisia", "Sudan", "Ethiopia", "Kenya", 
          # ... (rest of the countries)
          "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan", "Sri Lanka", "Cyprus", "Palestine"
        )) {
          data_frame$continent[i] = "Africa"
          
        } else if (country %in% c(
          "Mexico", "United States of America", "Canada", "Costa Rica", "Nicaragua", "Panama", "Belize", "Guatemala", "Mexico", "United States of America", "Canada", "Dominica", "Guam (U.S.A.)"
        )) {
          data_frame$continent[i] = "North America"
          
        } else if (country %in% c(
          "Bolivia (Plurinational State of)", "Chile", "Peru", "Brazil", "Colombia", "Venezuela (Bolivarian Republic of)", 
          "Argentina", "Suriname", "Uruguay", "Guyana","Ecuador", "Paraguay"
        )) {
          data_frame$continent[i] = "South America"
          
        } else if (country %in% c(
          "Israel", "India", "Azerbaijan", "Iran (Islamic Republic of)", "Armenia", "Georgia", "Pakistan", "Afghanistan", 
          "Bangladesh", "Nepal", "Bhutan", "Myanmar", "Thailand", "Cambodia", "China", "Hong Kong, China", "Mongolia", 
          "Iraq", "Jordan", "Syrian Arab Republic", "Kuwait", "Lebanon", "Saudi Arabia", "United Arab Emirates", "Yemen", 
          "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan", "Sri Lanka", "Cyprus", "Palestine"
        )) {
          data_frame$continent[i] = "Asia"
          
        } else if (country %in% c(
          "Australia", "Fiji", "Timor-Leste", "Papua New Guinea", "Solomon Islands", "Vanuatu", "Palau", "Norfolk Island (Australia)", "Indonesia", "Japan", "Lao People's Democratic Republic", "Libyan Arab Jamahiriya", "Malaysia", "Micronesia (Federated States of)", 
          "Philippines", "Republic of Korea", "Viet Nam", "Fiji", "New Caledonia (France)", "Norfolk Island (Australia)", "Tokelau", "New Zealand"
        )) {
          data_frame$continent[i] = "Oceania"
        } else {
          # Print the country that is causing the issue
          print(paste("Unmatched country:", country))
        }
      }
    }
    
    return(data_frame)
  } else {
    # Print a message if the "countries" column is not found
    cat("Error: 'countries' column not found in the data frame.\n")
    return(NULL)
  }
}

# Read in the dataset 
cleaning_of_country_dataset = read.csv("extended_language.csv")

# Remove the columns that are needed 
unclean_country_dataset = data.frame(
  language = cleaning_of_country_dataset$name_in._english,
  countries = cleaning_of_country_dataset$countries,
  degree_of_endangerment = cleaning_of_country_dataset$degree_of_endangerment,
  number_of_speakers = cleaning_of_country_dataset$number_of_speakers
)

# Create an empty data frame to store the result
country_dataset = data.frame(country_list = character(0))

# Check if the row contains a comma before attempting to separate
if ("countries" %in% names(unclean_country_dataset) && any(grepl(",", unclean_country_dataset$countries))) {
  # Separate the countries column into individual rows
  country_dataset = separate_rows(unclean_country_dataset, countries, sep = ", ") %>%
    mutate(countries = trimws(iconv(countries, to = "UTF-8")))  # Convert to UTF-8 and then trim
}

# Remove rows with blank "countries"
country_dataset = subset(country_dataset, countries != "")

# Find the total number of languages in each country
summary_country = count(country_dataset, degree_of_endangerment, countries)

# Renames the row N to Occurrences
colnames(summary_country)[colnames(summary_country) == "n"] = "languages"

# Find the total amount of countries
summary_country$total_amount_of_languages = sum(summary_country$languages)

# Calculate the total amount of countries
summary_country$total_amount_of_countries = nrow(unique(country_dataset[c("degree_of_endangerment", "countries")]))

# Add Continent
summary_country = add_continent_column(summary_country)

## NOTE: Found that the continent of Côte d'Ivoire for some reason changing to 
## "C“te d'Ivoire" or "C?te d'Ivoire" going into the correct country will
## have to manually change the continent from NA to Africa
summary_country$continent = ifelse(is.na(summary_country$continent), "Africa", summary_country$continent)

# Totals all the languages by degree of endangerment
summary_country_grouped = summarize(
  group_by(summary_country, degree_of_endangerment, continent),
  languages = sum(languages)
)

## Max
# Find the row with the highest number of languages for each category
max_indices = tapply(seq(nrow(summary_country)), summary_country$degree_of_endangerment, function(x) x[which.max(summary_country$languages[x])])
# Puts that row into a new data frame
summary_country_max = summary_country[max_indices, ]

## Min
## NOTE: discovered there was a blank entry in the data, 
## resolved in code above to get accurate information. If in 
## the data set the column total_amount_of_countries is 3075
## go back and run line 131 - 132. Then try this code again.

# Find the row with the lowest number of languages for each category
min_indices = tapply(seq(nrow(summary_country)), summary_country$degree_of_endangerment, function(x) x[which.min(summary_country$languages[x])])
summary_country_min <- summary_country[min_indices, ]

#__________________________________________________________________________________________________________
# Shiny App Starts 
#__________________________________________________________________________________________________________

# Define UI for application that draws a histogram
ui = dashboardPage(
  dashboardHeader(title =tags$h1 ("Endangered Languages", style = "font-size: 19px;")),
  dashboardSidebar(
    br(),
    radioGroupButtons(
      inputId = "radio_btn",
      label = "Degree of Endangerment",
      choices = c("Total by Each Category","Percent of Endangerment"),
      justified = TRUE,
      direction = "vertical"),
    
    br(),
    # Widget for the continent 
    pickerInput(
      inputId = "continent",
      label = "Endangerment by Continent",
      choices = unique(summary_country$degree_of_endangerment),
    )
  ),
  dashboardBody(
    fluidRow(
      tabsetPanel(
        id = "tabs",
        # Creates the tab for degree of endangerment
        tabPanel("Degree of Endangerment",
                 plotlyOutput("degree_of_endangerment_plot")),
        # Creates the tab Endangerment by Continent 
        tabPanel("Endangerment by Continent",
                 plotlyOutput("endangerment_by_continent_plot"))
      )
    )
  ),
  skin = "black"
)

server = function(input, output) {
  # Renders plot for degree_of_endangerment tab
  output$degree_of_endangerment_plot = renderPlotly({
    if (input$radio_btn == "Total by Each Category") {
    ggplot(endangerment_dataset, aes(x = reorder(degree_of_endangerment, languages), y = languages)) +
      theme_minimal() +
      theme(
        # Centers the title 
        plot.title = element_text(hjust = 0.5),
        # Removes the background lines
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        # Removes the axis
        axis.line          = element_blank(),  
        # Removes the tick marks
        axis.ticks.x       = element_blank(), 
        axis.ticks.y       = element_blank(),
        
        # Customize the appearance of the X-axis
        # Adjust size and style
        strip.text.x = element_text(size = 10, face = "bold"),  
        # Changes the angel of the x axis labels
        axis.text.x = element_text(angle = 45, hjust = 1) 
        
      ) +
      # Formats the Y axis to count by 500
      scale_y_continuous(
        breaks = seq(0, 800, by = 50) 
      )+  # Adjust breaks as needed
      
      # Changes the labels of the bargraph 
      labs(
        title = "Number of Languages by Endangerment", 
        x = "Degree of Endangerment",
        y = "Number of Languages"
        
      ) +
      geom_col(fill = "steelblue", color = "black") 
    } else {
    
    #### Pie Chart #####
    
    # Creates the pie chart
      pie_chart <- plot_ly(
        data = pie_chart_df,
        labels = ~degree_of_endangerment,
        values = ~percentage,
        type = "pie",
        marker = list(colors = hcl.colors(length(levels(pie_chart_df$degree_of_endangerment)), "cold")),
        textinfo = "label+percent"
      )
      
      # Remove the legend
      pie_chart <- layout(pie_chart, showlegend = FALSE)
    }
  })
  
  
  
  #___________________________________________________________________
  
  # Renders plot for endangerment_by_continent tab
  output$endangerment_by_continent_plot = renderPlotly({
    
    get_dataset <- reactive({
      switch(input$continent,
             "Vulnerable" = filter(summary_country_grouped, degree_of_endangerment == "Vulnerable"),
             "Definitely endangered" = filter(summary_country_grouped, degree_of_endangerment == "Definitely endangered"),
             "Severely endangered" = filter(summary_country_grouped, degree_of_endangerment == "Severely endangered"),
             "Critically endangered" = filter(summary_country_grouped, degree_of_endangerment == "Critically endangered"),
             "Extinct" = filter(summary_country_grouped, degree_of_endangerment == "Extinct"))
    })
    
    selected_data <- get_dataset()
    ggplot(selected_data, aes(x = reorder(continent, languages), y = languages, fill = continent)) +
      # Changes the label 
      labs(title = '',
           x = 'Country',
           y = 'Language') +
      # Sets the theme to minimal
      theme_minimal() +
      # Change other aspects of the graph 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.line = element_blank(),    
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(margin = margin(b = 20)),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(
        breaks = seq(0, 200, by = 50) 
      ) + 
      # Creates the bar graph 
      geom_col()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
