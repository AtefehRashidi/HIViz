# Libs

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "HIV_Vizualization"),
  shinydashboard::dashboardSidebar(
    shiny::radioButtons("dataSource", "Select data source:",
                        choices = c( "Use sample data" = "sample"),
                        selected = character(0)) ,
    shiny::fileInput("file", "Upload Data (.xlsx or .sav)", accept = c(".xlsx", ".sav")),
    shinyWidgets::pickerInput(
      inputId = "metrics",
      label = "Select Metrics for Time Series:",
      choices = c("hiv_prevalence", "hiv_incidence", "aids_deaths", "plhiv", "art_coverage", "testing_coverage"),
      selected = c("hiv_prevalence"),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
       
    ),
    shinyWidgets::pickerInput("sexFilter", "Select Sex:", choices = c("Male", "Female"), selected = c("Male", "Female"), multiple = TRUE),
    shinyWidgets::pickerInput("ageGroupSelect", "Select Age Group(s):", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))
  ),
  shinydashboard::dashboardBody(
    shiny::tabsetPanel(
      shiny::tabPanel("Summary Table", shiny::br(), DT::DTOutput("summary_table")),
      shiny::tabPanel("Top Countries by Year", shiny::br(), DT::DTOutput("top_countries")),
      shiny::tabPanel("Time Series by Sex", shiny::downloadButton("download_ts", "Download Plot"), shiny::br(), plotly::plotlyOutput("ts_plot")),
      shiny::tabPanel("Combined Bar: Sex × Age", shiny::downloadButton("download_bar", "Download Plot"), shiny::br(), plotly::plotlyOutput("bar_combined")),
      shiny::tabPanel("Reasons for Infection (Trends)", shiny::downloadButton("download_reason", "Download Plot"), shiny::br(), plotly::plotlyOutput("reason_ts")),
      shiny::tabPanel("Reasons (Word Cloud)", shiny::downloadButton("download_wordcloud", "Download Plot"), shiny::br(), shiny::plotOutput("reason_wordcloud"))
    )
  )
)

server <- function(input, output, session) {
  classic_pal <- as.character(paletteer::paletteer_d("ggthemes::calc"))# get the palette for all plots
  
  user_data <- reactive({
    shiny::req(input$dataSource)  # تا وقتی کاربر منبع دیتا رو مشخص نکرده، هیچی نشون نده
    
  if (input$dataSource == "sample") {
    df <- sample_data
  } else {
    shiny::req(input$file)
    ext <- tools::file_ext(input$file$name)
     df <- switch(ext,
             "xlsx" = readxl::read_excel(input$file$datapath),
              "sav" = haven::read_sav(input$file$datapath),
                {
                 shiny::showNotification("Unsupported file type", type = "error")
                  return(NULL)
               })
  }
    
# اعتبارسنجی ساده
  if (!"year" %in% names(df)) {
    shiny::showNotification("Column 'year' not found in dataset.", type = "error")
    return(NULL)
  }
    
df <- df |>
    dplyr::mutate(date = as.Date(paste0(year, "-01-01")))
    
  if ("age_group" %in% names(df)) {
     shinyWidgets::updatePickerInput(session, "ageGroupSelect",
                                 choices = unique(df$age_group),
                                selected = unique(df$age_group))
  }
    
  df
})
  
  #.........summary_table..............................  
  output$summary_table <- DT::renderDT({
    df <- user_data()
    DT::datatable(df, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), pageLength = 10)) #save tables
  })
#.........top_countries..............................    
output$top_countries <- DT::renderDT({
    df <- user_data()
    shiny::req("country" %in% names(df), "year" %in% names(df))
    metrics <- c("hiv_prevalence", "hiv_incidence", "aids_deaths")
    results <- list()
    for (metric in metrics) {
      top_df <- df |>
        dplyr::group_by(year) |>
        dplyr::filter(!is.na(.data[[metric]])) |>
        dplyr::slice_max(order_by = .data[[metric]], n = 1, with_ties = FALSE) |>
        dplyr::select(year, country, !!dplyr::sym(metric))
      names(top_df)[3] <- "value"
      top_df$metric <- metric
      results[[metric]] <- top_df
    }
    final <- dplyr::bind_rows(results) |> dplyr::arrange(year, metric)
    DT::datatable(final, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), pageLength = 10))
  })
#...........ts_plot..............................    
output$ts_plot <- plotly::renderPlotly({
  df <- user_data()
    shiny::req(length(input$metrics) > 0, input$sexFilter, input$ageGroupSelect)
    df_filtered <- df |>
      dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect)
    
df_long <- df_filtered |>
      dplyr::select(date, sex, dplyr::all_of(input$metrics)) |>
      tidyr::pivot_longer(cols = tidyr::all_of(input$metrics), names_to = "metric", values_to = "value") |>
      dplyr::group_by(date, sex, metric) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
df_all <- df_filtered |>
      dplyr::select(date, dplyr::all_of(input$metrics)) |>
      tidyr::pivot_longer(cols = tidyr::all_of(input$metrics), names_to = "metric", values_to = "value") |>
      dplyr::group_by(date, metric) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(sex = "All")
    
df_long <- dplyr::bind_rows(df_long, df_all)
    last_vals <- df_long |>
      dplyr::group_by(sex, metric) |> 
      dplyr::filter(date == max(date))
    
p <- ggplot2::ggplot(df_long, ggplot2::aes(x = date, y = value, color = sex)) +
      ggplot2::geom_line(linewidth = 1) +
      ggrepel::geom_text_repel(
        data = last_vals,
        ggplot2::aes(label = sex),
        nudge_x = 30,
        size = 3.5,
        show.legend = TRUE
      ) +
      ggplot2::facet_wrap(~metric, scales = "free_y") +
      ggplot2::theme_classic() +
      ggplot2::scale_color_manual(values = classic_pal) +
      ggplot2::labs(x = "Year", y = "Value", title = "Time Series of HIV Indicators by Sex", color = "Sex") +
      ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))
    
    plotly::ggplotly(p) |> plotly::layout(showlegend = TRUE)
  })
  
output$download_ts <- shiny::downloadHandler(
    filename = function() {
      paste0("time_series_plot_", base::Sys.Date(), ".png")
    },
  content = function(file) {
      df <- user_data()
      df_filtered <- df |> 
        dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect)
      
   df_long <- df_filtered |>
        dplyr::select(date, sex, dplyr::all_of(input$metrics)) |>
        tidyr::pivot_longer(cols = tidyr::all_of(input$metrics), names_to = "metric", values_to = "value") |>
        dplyr::group_by(date, sex, metric) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
      
  df_all <- df_filtered |>
        dplyr::select(date, dplyr::all_of(input$metrics)) |>
        tidyr::pivot_longer(cols = tidyr::all_of(input$metrics), names_to = "metric", values_to = "value") |>
        dplyr::group_by(date, metric) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(sex = "All")
      
 df_long <- dplyr::bind_rows(df_long, df_all)
      
    last_vals <- df_long |>
        dplyr::group_by(sex, metric) |>
        dplyr::filter(date == max(date))
      
p <- ggplot2::ggplot(df_long, ggplot2::aes(x = date, y = value, color = sex)) +
        ggplot2::geom_line(linewidth = 1) +
        ggrepel::geom_text_repel(
          data = last_vals, 
          ggplot2::aes(label = sex), 
          nudge_x = 30, 
          size = 3.5,
          show.legend = FALSE
        ) +
        ggplot2::facet_wrap(~metric, scales = "free_y") +
        ggplot2::theme_classic() +
        ggplot2::scale_color_manual(values = classic_pal) +
        ggplot2::labs(
          x = "Year", 
          y = "Value", 
          title = "Time Series of HIV Indicators by Sex",
          color = "Sex"
        ) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          strip.text = ggplot2::element_text(face = "bold", size = 12)
        )
      
      ggplot2::ggsave(file, plot = p, device = "png", width = 10, height = 6, dpi = 300)
    }
  )
#.........reason_ts......................................    
output$reason_ts <- plotly::renderPlotly({
  df <- user_data()
  shiny::req(all(c("year", "causes", "sex", "age_group") %in% names(df)))
  
df_reason <- df |>
      dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect) |>
      dplyr::group_by(year, causes) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
 plotly::plot_ly(df_reason, x = ~year, y = ~count, color = ~causes,
                    type = 'scatter', mode = 'lines',
                    line = list(width = 1.5),
                    hoverinfo = 'text',
                    hovertext = ~paste("Cause:", causes, "<br>Year:", year, "<br>Count:", count),
                    colors = classic_pal) |>
plotly::layout(
        title = list(
          text = "Trends in Reported Causes of HIV Infection",
          x = 0.5,
          font = list(size = 16, face = "bold")
        ),
        xaxis = list(
          title = "Year",
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          gridcolor = '#f0f0f0'
        ),
        yaxis = list(
          title = "Count",
          showgrid = FALSE,
          zeroline = FALSE,
          showline = TRUE,
          gridcolor = '#f0f0f0'
        ),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1,
          xanchor = "left"
        ),
        hovermode = 'closest',
        margin = list(t = 50, b = 80, r = 120)
      )
})
  
output$download_reason <- shiny::downloadHandler(
    filename = function() {
      paste0("reason_ts_plot_", base::Sys.Date(), ".png")
    },
    content = function(file) {
      df <- user_data()
      df_reason <- df |>
        dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect) |>
        dplyr::group_by(year, causes) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop")
      
      last_points <- df_reason |>
        dplyr::group_by(causes) |>
        dplyr::filter(year == max(year)) |>
        dplyr::ungroup()
      
  p <- ggplot2::ggplot(df_reason, ggplot2::aes(x = year, y = count, color = causes)) +
        ggplot2::geom_line(size = 1.2) +
        ggrepel::geom_text_repel(
          data = last_points,
          ggplot2::aes(label = causes),
          nudge_x = 0.5,
          size = 4,
          direction = "y",
          segment.color = "grey80",
          box.padding = 0.5,
          show.legend = FALSE
        ) +
        ggplot2::scale_color_manual(values = classic_pal) +
        ggplot2::theme_classic() +
        ggplot2::labs(
          title = "Trends in Reported Causes of HIV Infection",
          x = "Year",
          y = "Count",
          color = NULL
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none",
          axis.text = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14)
        )
      ggplot2::ggsave(file, plot = p, device = "png", width = 12, height = 7, dpi = 300)
    }
  )
# Bar plot ...................................
  
output$bar_combined <- plotly::renderPlotly({
    df <- user_data()
    shiny::req("sex" %in% names(df), "age_group" %in% names(df))
    
df_bar <- df |>
      dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect) |>
      dplyr::group_by(sex, age_group) |>
      dplyr::summarise(incidence = sum(hiv_incidence, na.rm = TRUE), .groups = "drop") |>
      dplyr::group_by(age_group) |>
      dplyr::mutate(percent = incidence / sum(incidence) * 100)
    
p <- ggplot2::ggplot(df_bar, ggplot2::aes(x = age_group, y = incidence, fill = sex)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9), alpha = 0.8) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(base::round(percent, 1), "%")), 
        position = ggplot2::position_dodge(width = 0.9), 
        vjust = -0.5, 
        size = 3.5
      ) +
      ggplot2::theme_classic() +
      ggplot2::scale_fill_manual(values = classic_pal) +
      ggplot2::labs(
        x = "Age Group", 
        y = "HIV Incidence", 
        title = "HIV Incidence by Age Group and Sex", 
        fill = "Sex"
      ) +
      ggplot2::theme(
        legend.position = "bottom", 
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold")
      )
    
 plotly::ggplotly(p) |> plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  })
  
output$download_bar <- shiny::downloadHandler(
    filename = function() {
      paste0("bar_plot_", base::Sys.Date(), ".png")
    },
    content = function(file) {
      df <- user_data()
      df_bar <- df |>
        dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect) |>
        dplyr::group_by(sex, age_group) |>
        dplyr::summarise(incidence = sum(hiv_incidence, na.rm = TRUE), .groups = "drop") |>
        dplyr::group_by(age_group) |>
        dplyr::mutate(percent = incidence / sum(incidence) * 100)
      
  p <- ggplot2::ggplot(df_bar, ggplot2::aes(x = age_group, y = incidence, fill = sex)) +
        ggplot2::geom_bar(stat = "identity", 
                          position = ggplot2::position_dodge(width = 0.9), 
                          alpha = 0.8) +
  ggplot2::geom_text(
          ggplot2::aes(label = paste0(base::round(percent, 1), "%")), 
          position = ggplot2::position_dodge(width = 0.9), 
          vjust = -0.5, 
          size = 4
        ) +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_manual(values = classic_pal) +
        ggplot2::labs(
          x = "Age Group", 
          y = "HIV Incidence", 
          title = "HIV Incidence by Age Group and Sex", 
          fill = "Sex"
        ) +
        ggplot2::theme(
          legend.position = "bottom", 
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14)
        )
      
      ggplot2::ggsave(file, plot = p, device = "png", width = 12, height = 7, dpi = 300)
    }
  )
#.......reason_wordcloud.............................
  
output$reason_wordcloud <- shiny::renderPlot({
  df <- user_data()
  shiny::req("causes" %in% names(df))
    
filtered_df <- df |> 
      dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect)
    
    word_freq <- base::sort(base::table(filtered_df$causes), decreasing = TRUE)
    base::set.seed(123)
    graphics::par(mar = base::rep(0, 4))
    wordcloud::wordcloud(
      words = base::names(word_freq),
      freq = word_freq,
      scale = c(4, 0.8),
      min.freq = 1,
      max.words = 100,
      random.order = FALSE,
      rot.per = 0.35,
      colors = classic_pal,
      fixed.asp = TRUE
    )
  })
  
output$download_wordcloud <- shiny::downloadHandler(
    filename = function() {
      paste0("wordcloud_", base::Sys.Date(), ".png")
    },
    content = function(file) {
      df <- user_data()
      filtered_df <- df |> 
        dplyr::filter(sex %in% input$sexFilter, age_group %in% input$ageGroupSelect)
      
      word_freq <- base::sort(base::table(filtered_df$causes), decreasing = TRUE)
      grDevices::png(file, width = 1200, height = 800, res = 150)
      graphics::par(mar = base::rep(0, 4))
      base::set.seed(123)
      wordcloud::wordcloud(
        base::names(word_freq), 
        word_freq,
        scale = c(4, 0.8),
        min.freq = 1,
        max.words = 100,
        random.order = FALSE,
        rot.per = 0.35,
        colors = classic_pal,
        fixed.asp = TRUE
      )
  grDevices::dev.off()
    }
  )
}

shiny::shinyApp(ui = ui, server = server)