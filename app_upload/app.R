# setup -------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(shinyTable)

# UI ----------------------------------------------------------------------


shiny::runApp(list(
    ui=pageWithSidebar(
        headerPanel('Sorteio c/ upload de .csv')
        ,
        sidebarPanel(
            fileInput('file1', 'Arquivo CSV',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
            ,
            tags$hr(),
            checkboxInput('header', 'Cabeçalho', TRUE),
            radioButtons('sep', 'Separador',
                         c(`Vírgula`=',',
                           `Ponto e vírgula`=';',
                           `Espaço em branco`='\t'),
                         'Comma'),
            tableOutput(outputId = 'table.output')
        )
        ,
        mainPanel(
            plotOutput("distPlot")
        ))
    ,
    server=function(input, output){
        output$table.output <- renderTable({
            
            inFile <- input$file1
            
            if (is.null(inFile))
                return(NULL)
            
            tbl <- read.csv2(inFile$datapath, header=input$header, sep=input$sep,encoding = "Latin1")
            names(tbl) <- stringr::str_to_title(names(tbl))
            return(tbl)
        })
        
        output$distPlot <- renderPlot({
            inFile <- input$file1
            
            # if (is.null(inFile))
            #     return(NULL)
            # tbl <- read.csv2("/home/vfx/Downloads/test.csv",sep = ",",stringsAsFactors = F,na.strings = "") %>%
                # glimpse()
            tbl <- read.csv2(inFile$datapath,
                             header=input$header,
                             sep=input$sep,
                             encoding = "Latin1",
                             na.strings = "",
                             stringsAsFactors = F)
            file_names <- names(tbl)
            cabeceiro  <- tbl %>% select(1) %>% na.omit() %>% pull(file_names[1])
            peseiro <- tbl %>% select(2) %>% na.omit() %>% pull(file_names[2])

            n_cab <- length(cabeceiro)
            n_pe  <- length(peseiro)

            sort_equal <- function(x){
                out <- 1:x

                for(i in 1:(x-1)){
                    out <- c(out, c(lead(1:x,i),1:i))
                }

                out <- na.exclude(out)

                return(out)
            }

            if( n_pe != n_cab){
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
                facet_grid(num_rodada~., scales = "free",switch = "both")+
                labs(x = stringr::str_to_title(file_names[1]),
                     y = "",
                     subtitle = "Rodada")
            print(plot)
        })
    }
))