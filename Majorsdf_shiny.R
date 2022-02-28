library(tidyverse)
library(readxl)
df <- read_excel("data/slu_graduates_17_21.xlsx")

## fixes error in the data
df <- df %>% mutate(across(everything(),
                           .fns = ~replace(., . ==  "STATS" , "STAT")))

df_long <- df %>% pivot_longer(3:8, names_to = "type", values_to = "discipline")
df_major <- df_long %>% 
  filter(type == "major1" | type == "major2" | type == "major3")

df_stat <- df_major %>% filter(discipline == "STAT" ) 
df_extra <- df_major %>% filter(discipline == "STAT" | discipline == "MATH" | discipline == "CS") %>% group_by(discipline) %>%
  summarise(nstudent = n()) %>%
  mutate(discipline = fct_reorder(discipline, nstudent))
df_gender <- df_major %>% filter(discipline == "STAT" | discipline == "MATH" | discipline == "CS") %>% group_by(discipline,sex) %>%
  summarise(nstudent = n())
df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
  filter(type == "major1" |
           type == "major2" | 
           type == "major3")

df_nostat <- df_statfull %>% filter(discipline != "STAT" &
                                      !is.na(discipline)) %>%
  group_by(discipline) %>%
  summarise(nstudent = n()) %>%
  mutate(discipline = fct_reorder(discipline, nstudent))
ggplot(data = df_nostat, aes(x = discipline, y = nstudent)) +
  geom_col() +
  coord_flip()


#add data set called df_extra so I can have radio buttons that can only pick stat math or cs as majors.

#attempting and failing to add a table that changes the output of the gender it is showing based on a persons selection.
# table added succesfully but it does not change...

library(shiny)

ui <- fluidPage(
  sidebarLayout( 
    sidebarPanel(radioButtons(inputId = "majorchoice",
                             label = "Choose a Major",
                             choices = factor(df_extra$discipline))),
    mainPanel(plotOutput(outputId = "majorplot"),
    tableOutput(outputId = "majorbreakdown"))
  ))
    


server <- function(input, output, session) { 
  
  df_update <- reactive({df_stat <- df_major %>% filter(discipline == input$majorchoice)
  df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
    filter(type == "major1" |
             type == "major2" | 
             type == "major3")
  df_nostat <- df_statfull %>% filter(discipline != input$majorchoice &
                                        !is.na(discipline)) %>%
    group_by(discipline) %>%
    summarise(nstudent = n()) %>%
    mutate(discipline = fct_reorder(discipline, nstudent))
  
  })
 
  output$majorplot <- renderPlot({
    ggplot(data = df_update(), aes(x = discipline, y = nstudent)) +
      geom_col() +
      coord_flip()
  })
  
  output$majorbreakdown <- renderTable({df_gender 
  
})
}

shinyApp(ui, server)

# go slow so that if your app breaks you know why it broke 
