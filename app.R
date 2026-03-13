# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Load data --------------------------------------------------------------------

permits_df <- read_delim(
  "data/raw/issued-building-permits.csv",
  delim = ";",
  show_col_types = FALSE
) |> mutate(
  IssueDate = as.Date(IssueDate),
  PermitNumberCreatedDate = as.Date(PermitNumberCreatedDate),
  TypeOfWork = trimws(as.character(TypeOfWork))
)

area_choices <- c("All", sort(unique(na.omit(permits_df$GeoLocalArea))))

# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  title = "Vancouver Building Permits",
  theme = bs_theme(preset = "minty"),
  
  sidebar = sidebar(
    selectInput("area",
                "Neighbourhood",
                choices  = area_choices,
                selected = "All"),
    actionButton("reset", "Reset Filters", class = "btn-primary w-100")
  ),

  layout_columns(
    col_widths = c(6, 6),
    value_box(
      title = "Permits Issued",
      value = textOutput("permits_issued"),
      theme = "primary"
    ),
    value_box(
      title = "Avg Processing Time",
      value = textOutput("avg_processing"),
      theme = "secondary"
    )
  ),

  layout_columns(
    col_widths = 12,
    card(
      card_header("Permit Volume Over Time"),
      plotOutput("volume_chart", height = "300px")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  filtered <- reactive({
    df <- permits_df
    if (input$area != "All") {
      df <- df[df$GeoLocalArea == input$area, ]
    }
    df
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "area", selected = "All")
  })
  
  output$permits_issued <- renderText({
    formatC(nrow(filtered()), format = "d", big.mark = ",")
  })
  
  output$avg_processing <- renderText({
    df <- filtered()
    if (nrow(df) == 0) return("— Days")
    days <- as.numeric(df$IssueDate - df$PermitNumberCreatedDate)
    avg  <- mean(days, na.rm = TRUE)
    if (is.nan(avg) || is.na(avg)) return("— Days")
    paste0(round(avg, 1), " Days")
  })
  
  output$volume_chart <- renderPlot({
    df <- filtered()
    if (nrow(df) == 0) return(NULL)
    
    df <- df[!is.na(df$IssueDate), ]
    if (nrow(df) == 0) return(NULL)
    
    df$month   <- floor_date(df$IssueDate, "month")
    last_complete <- floor_date(Sys.Date(), "month") - months(1)
    monthly    <- df |> filter(month <= last_complete) |> count(month)
    
    ggplot(monthly, aes(x = month, y = n)) +
      geom_line(color = "#6c5ce7", linewidth = 1.1) +
      geom_area(fill = "#6c5ce7", alpha = 0.08) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "Year", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  }, bg = "transparent")
}

# Run --------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
