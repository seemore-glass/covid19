suppressMessages(library(shiny))
suppressMessages(library(quantmod))
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(DT))


curl <- "https://covid.ourworldindata.org/data/ecdc/full_data.csv"
fdfile <- "full_data.csv"
download.file(curl,fdfile)

full_data <- fread('full_data.csv',showProgress = F)

full_data <- full_data[location != "International"]

curl <- "https://covid.ourworldindata.org/data/ecdc/locations.csv"
locfile <- "locations.csv"
download.file(curl,locfile)

locations_data <- fread('locations.csv',showProgress = F,drop=c(1,4,5))

locations_data <- locations_data[!duplicated(location) & location != "International"]

grouped_data <- locations_data[full_data,on="location"]

grouped_data[is.na(continent), continent := "World"]

#write.table(grouped_data, "grouped_data.csv",append=F, sep=",", row.names=F)


ui <- fluidPage(
  radioButtons("item", "Item:",
               c("new_cases" = "new_cases",
                 "new_deaths" = "new_deaths",
                 "total_cases" = "total_cases",
                 "total_deaths" = "total_deaths"),
               inline = TRUE,
               selected = "new_cases"),
  
  
  selectInput("con",label = "continent",
              choices = unique(grouped_data$continent),selected = "World"),
  uiOutput('x'),plotOutput("plot"),dataTableOutput("dynamic")
)


server <- function(input, output){
  output$x = renderUI({
    if (input$con !="World"){
      selectInput("loc",label = "location",  choices = c("ALL",unique(grouped_data$location[grouped_data$continent == input$con])))
    }
  })
  
  plot_data <- reactive({
    if (input$con =="World"){
        grouped_data %>% filter(continent == input$con) %>%
        group_by(date) %>%
        summarise_each(funs(sum),new_cases,new_deaths,total_cases,total_deaths) %>%  
        mutate(days = as.integer(row_number()-1))
    }
    else if (input$loc =="ALL"){
        grouped_data %>% filter(continent == input$con) %>%
        group_by(date) %>%
        summarise_each(funs(sum),new_cases,new_deaths,total_cases,total_deaths) %>%
        mutate(days = as.integer(row_number()-1))
    }
    else {
        grouped_data %>%
        filter(continent == input$con & location == input$loc) %>%
        group_by(date) %>%
        summarise_each(funs(sum),new_cases,new_deaths,total_cases,total_deaths) %>%
        mutate(days = as.integer(row_number()-1))
    }  
    })
  
  output$plot = renderPlot({
      plot_data <- plot_data() %>%
                   mutate(ma5 = SMA(get(input$item),n = min(c(5,length(get(input$item)))))) %>% 
                   mutate(ma25 = SMA(get(input$item),n = min(c(25,length(get(input$item))))))

      g = ggplot(plot_data,aes(x=days))+
          geom_line(aes(y=get(input$item),fill = input$item,colour = input$item),size=1)+
          ylab(input$item)+
          geom_line(aes(y=ma5,color = "ma5"),size=1)+
          geom_line(aes(y=ma25,color = "ma25"),size=1)+
          scale_color_manual(name = "",
                             breaks=c(input$item, "ma5", "ma25"),
                             values = c("green","red","blue")
                             )+
          ggtitle(paste0("Covid19 Situation  ",max(plot_data$date)))+
          theme(
            plot.title = element_text(size = 16,hjust = 0.5),
            legend.position = "top",
            legend.text = element_text(size = 14)
          )

      print(g)
      
  })
  output$dynamic <- renderDataTable(plot_data(),
                                    options = list(lengthMenu = c(5, 10, 25, 50),
                                                   pageLength = 5))
}

shinyApp(ui = ui, server = server)