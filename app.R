
# app.R

library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(ggpmisc)

df_raw <- read_excel("apple_health_daily_summary.xlsx", col_types = "text")

df_raw <- df_raw %>%
  mutate(date_num = as.numeric(date),
         Date = as.Date(date_num, origin = "1899-12-30"),
         weight_clean = str_replace_all(str_trim(weight_lbs), "[^0-9.]", ""),
         weight_lbs = as.numeric(weight_clean))

eight_months_ago <- floor_date(Sys.Date(), "month") %m-% months(8)

df_filtered <- df_raw %>%
  filter(!is.na(Date), Date >= eight_months_ago)

numeric_cols <- c("weight_lbs", "ActiveEnergy", "BasalEnergy", "ExerciseMinutes", "Steps",
                  "FlightsClimbed", "WalkRunDistance", "BikeDistance", "StandHours",
                  "BMI", "Subcutaneous_fat_pct", "Visceral_fat", "Skeletal_muscle_lbs",
                  "Metabolic_age", "Body_fat_lbs", "Fat_free_mass_lbs", "Body_water_lbs",
                  "Muscle_mass_lbs", "Bone_mass_lbs", "Protein_lbs", "BMR_kcal")

for (col in numeric_cols) {
  if (col %in% colnames(df_filtered)) {
    df_filtered[[col]] <- as.numeric(df_filtered[[col]])
  }
}

df_filtered <- df_filtered %>%
  mutate(
    Month = factor(month(Date, label = TRUE, abbr = FALSE), levels = month.name),
    Weekday = factor(weekdays(Date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  )

monthly_avg_all <- df_filtered %>%
  group_by(Month) %>%
  summarise(across(all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

month_boundaries <- seq(floor_date(min(df_filtered$Date), "month"),
                        ceiling_date(max(df_filtered$Date), "month"), by = "month")

month_rects <- data.frame(
  x_min = head(month_boundaries, -1),
  x_max = tail(month_boundaries, -1),
  month = month.name[month(month_boundaries[-length(month_boundaries)])]
)

month_colors <- c(
  "January" = "#1f77b4", "February" = "#ff7f0e", "March" = "#2ca02c", 
  "April" = "#d62728", "May" = "#9467bd", "June" = "#8c564b",
  "July" = "#e377c2", "August" = "#7f7f7f", "September" = "#bcbd22", 
  "October" = "#17becf", "November" = "#c7c7c7", "December" = "#ff9896"
)

weekday_shapes <- c("Monday" = 16, "Tuesday" = 17, "Wednesday" = 15,
                    "Thursday" = 18, "Friday" = 8, "Saturday" = 3, "Sunday" = 4)

# SHINY UI ----
ui <- fluidPage(
  titlePanel("Apple Health Trends"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Choose a metric to visualize:", choices = intersect(numeric_cols, colnames(df_filtered)))
    ),
    mainPanel(
      plotOutput("healthPlot", height = "700px")
    )
  )
)

# SHINY SERVER ----
server <- function(input, output) {
  output$healthPlot <- renderPlot({
    metric <- input$metric

    month_labels_df <- monthly_avg_all %>%
      select(Month, avg_value = !!sym(metric)) %>%
      left_join(month_rects, by = c("Month" = "month")) %>%
      filter(!is.na(avg_value)) %>%
      mutate(
        x_pos = x_min + (x_max - x_min) / 2,
        label = paste0(substr(Month, 1, 3), " — Avg: ", round(avg_value, 1))
      )

    ggplot(df_filtered, aes(x = Date, y = .data[[metric]])) +
      geom_rect(data = month_rects,
                aes(xmin = x_min, xmax = x_max, ymin = -Inf, ymax = Inf, fill = month),
                alpha = 0.1, color = "black", inherit.aes = FALSE) +
      geom_point(aes(color = Month, shape = Weekday), size = 3) +
      scale_shape_manual(values = weekday_shapes) +
      geom_smooth(aes(group = Month, color = Month), method = "lm", se = FALSE, size = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
      scale_color_manual(values = month_colors) +
      scale_fill_manual(values = month_colors, guide = "none") +
      scale_x_date(
        breaks = month_labels_df$x_pos,
        labels = month_labels_df$label,
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "right",
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_line(color = "gray"),
        plot.margin = margin(1, 1, 2, 1, "cm")
      ) +
      labs(
        title = paste("Scatter plot of", metric),
        x = NULL,
        y = gsub("\\.", " ", metric),
        shape = "Weekday",
        color = "Month"
      ) +
      geom_text_repel(aes(label = format(Date, "%b %d"), color = Month),
                      box.padding = 0.5, max.overlaps = 50, size = 3, show.legend = FALSE) +
      stat_poly_eq(
        aes(label = paste(stat(eq.label), "*','~~", stat(rr.label))),
        formula = y ~ x,
        parse = TRUE,
        size = 4,
        color = "black",
        output.type = "expression"
      )
  })
}

# RUN APP ----
shinyApp(ui = ui, server = server)
