#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("OSetup"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("cab",
                        "Número de cabeceiros",
                        min = 1,
                        max = 15,
                        value = 5),
            sliderInput("pe",
                        "Número de peseiros",
                        min = 1,
                        max = 15,
                        value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        cabeceiro <- paste0("Cabeceiro ",1:input$cab)
        peseiro  <- paste0("Peseiro ",1:input$pe)
        n_pe  <- length(peseiro)
        n_cab <- length(cabeceiro)
        
        # x = 3
        # i = 2
        sort_equal <- function(x){
            out <- 1:x
            for(i in 1:(x-1)){
                out <- c(out, c(lead(1:x,i),1:i))
            }
            out <- na.exclude(out)
            return(out)
        }
        
        # sort_equal(3)
        
        if(n_pe != n_cab){
            sorteio  =  rep(1:n_pe,n_cab)
        }else{
            sorteio  = sort_equal(n_pe)
        }
        
        set.seed(123);df_cab <- tibble(
            cabeca = cabeceiro,
            num_cab = sample(1:n_cab,n_cab)) 
        
        set.seed(123);df_pe <- tibble(
            pe = peseiro,
            num_pe = sample(1:n_pe,n_pe))
        
        df <-
            expand.grid(num_cab= 1:n_cab, num_rodada = 1:n_pe) %>% 
            as_tibble() %>% 
            mutate(num_pe = sorteio) %>% 
            left_join(df_pe) %>% 
            left_join(df_cab) %>% 
            mutate(pe = if_else(cabeca == pe,NA_character_,pe)) %>% 
            mutate(cabeca = fct_reorder(cabeca,num_cab)) %>% 
            mutate(pe = fct_reorder(pe,num_pe)) %>% 
            mutate(num_rodada = as.character(num_rodada)) %>% 
            mutate(num_rodada = fct_reorder(num_rodada, as.numeric(num_rodada)))
        
        plot <-
        df %>% 
            ggplot(aes(y = num_rodada,cabeca))+
            theme_bw(16)+
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  strip.text.y = element_text(angle = 180),
                  strip.placement = "outside"
            )+
            theme(panel.grid = element_blank())+
            scale_y_discrete(expand = c(0,0))+
            scale_x_discrete(expand = c(0,0))+
            geom_tile(aes(fill = pe),col = "black", show.legend = F, alpha = .7)+
            geom_text(aes(label = pe), fontface = "bold",size = 3.5)+
            scale_fill_brewer(palette = "Set1")+
            # pga_logo()+
            facet_grid(num_rodada~., scales = "free",switch = "both")+
            labs(x = "Cabeceiro",
                 y = "",
                 subtitle = "Rodada")
        print(plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
