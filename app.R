# Load necessary libraries =================================
library(shiny)
library(scholar)
library(dplyr)
library(plotly)

# Color Palette #
# https://www.pinterest.com/pin/795166877959957706/
# #18656A
# #9D5933
# #C7C0BD
# #20272D
# #39858F

# Define UI ================================================
ui <- fluidPage(
  titlePanel("Shiny Scholar"),
  ## Side bar ----------------------------------------------
  sidebarLayout(
    sidebarPanel(
		selectInput("scholar", "Select scholar:",
					c("Gabriella Costabile"="og0u3X4AAAAJ",
					  "Thomas Moore"="l5fRQdoAAAAJ")
		) # End selectInput
	), # End sidebarPanel
	## Main panel ---------------------------------------------
    mainPanel(
		textOutput("author"),
		tags$head(tags$style("#author{color: #18656A;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                         )
              ), # End author CSS
		textOutput("affiliation"),
		tags$head(tags$style("#affiliation{color: #20272D;
                                 font-size: 16px;
                                 font-style: bold;
                                 }"
                         )
              ), # End affiliation CSS
		br(),
		fluidRow(
			column(2, p("Total citations:")),
			column(3, textOutput("tot_cites"), 
				   tags$head(tags$style("#tot_cites{color: #9D5933}"))),
			column(1, p("h-index:")),
			column(3, textOutput("hindex"), 
				   tags$head(tags$style("#hindex{color: #9D5933}")))
		), # End fluidRow
		fluidRow(
			column(6, plotlyOutput("cite_hist")),
			column(6, plotlyOutput("pub_years"))	
		) # End fluidRow
	) # End mainPanel
  ) # End sidebarLayout
) # End fluidPage

server <- function(input, output, session) {
	output$author <- renderText({
		scholar::get_profile(input$scholar)$name
	})

	output$affiliation <- renderText({
		scholar::get_profile(input$scholar)$affiliation
	})

	output$tot_cites <- renderText({
		scholar::get_profile(input$scholar)$total_cites
	})

	output$hindex <- renderText({
		scholar::get_profile(input$scholar)$h_index
	})

	output$cite_hist <- renderPlotly({
		
		df <- scholar::get_citation_history(input$scholar)
		df$sumCites <- cumsum(df$cites)
		
		p <- ggplot(df, aes(year, sumCites))+
				geom_line(colour = "#18656A")+
				geom_point(shape=21, colour = "#20272D",
						   fill = "#39858F", size=2)+
				scale_x_continuous(name = "Year")+
				scale_y_continuous(name = "Cumulative no. citations")
		
		return(ggplotly(p))
	})

	output$pub_years <- renderPlotly({
		df <- scholar::get_publications(input$scholar)

		tmp <- df %>% count(year)

		p <- ggplot(tmp, aes(year, n))+
				geom_col(colour = "#20272D", fill="#39858F")+
				scale_x_continuous(name = "Year")+
				scale_y_continuous(name = "No. publications")

		return(ggplotly(p))
	})
}

shinyApp(ui, server)


