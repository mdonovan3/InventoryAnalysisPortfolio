library(shiny)
library(shinydashboard)
library(fresh)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(duckdb)
library(DBI)

source("functions.R")

# ── Load data at startup (static — no live DB connection) ─────────────────────
con <- dbConnect(duckdb(), dbdir = "data/bones_demo.duckdb", read_only = TRUE)
inventories   <- dbGetQuery(con, "SELECT * FROM inventories ORDER BY inventory_date")
inv_details   <- dbGetQuery(con, "SELECT * FROM inv_details")
all_sales     <- dbGetQuery(con, "SELECT * FROM sales")
all_purchases <- dbGetQuery(con, "SELECT * FROM purchases")
inv_totals    <- dbGetQuery(con, "SELECT * FROM inv_totals ORDER BY inventory_date")
dbDisconnect(con)

inv_details$inventory_date   <- as.Date(inv_details$inventory_date)
inventories$inventory_date   <- as.Date(inventories$inventory_date)
all_sales$start_date         <- as.Date(all_sales$start_date)
all_sales$end_date           <- as.Date(all_sales$end_date)
all_purchases$start_date     <- as.Date(all_purchases$start_date)
all_purchases$end_date       <- as.Date(all_purchases$end_date)
all_purchases$invoice_date   <- as.Date(all_purchases$invoice_date)
all_purchases$payment_date   <- as.Date(all_purchases$payment_date)
inv_totals$inventory_date    <- as.Date(inv_totals$inventory_date)

inv_dates <- sort(unique(inventories$inventory_date))
rooms     <- sort(unique(inv_details$room_name))
vendors   <- sort(unique(all_purchases$company_name))

date_choices <- setNames(
  as.character(inv_dates),
  format(inv_dates, "%b %d, %Y")
)

# ── Fresh dark theme ──────────────────────────────────────────────────────────
app_theme <- create_theme(
  adminlte_color(
    light_blue = "#1f6feb",
    aqua       = "#1f6feb",
    green      = "#3fb950",
    yellow     = "#d29922",
    red        = "#f85149",
    navy       = "#0d1117"
  ),
  adminlte_sidebar(
    dark_bg       = "#0d1117",
    dark_hover_bg = "#161b22",
    dark_color    = "#8b949e",
    dark_submenu_bg    = "#0d1117",
    dark_submenu_color = "#8b949e"
  ),
  adminlte_global(
    content_bg  = "#0d1117",
    box_bg      = "#161b22",
    info_box_bg = "#161b22"
  )
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin  = "black",
  title = "Bones Wine — Inventory Analysis",

  dashboardHeader(
    title = tags$span(
      style = "font-weight: 600; letter-spacing: 0.3px;",
      "Bones Wine Analytics"
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Overview",   tabName = "overview",   icon = icon("chart-line")),
      menuItem("Inventory",  tabName = "inventory",  icon = icon("warehouse")),
      menuItem("Sales",      tabName = "sales",      icon = icon("chart-bar")),
      menuItem("Purchases",  tabName = "purchases",  icon = icon("truck")),
      menuItem("Variance",   tabName = "variance",   icon = icon("balance-scale"))
    ),
    tags$hr(style = "border-color: #21262d; margin: 12px 0;"),
    tags$div(
      style = "padding: 0 12px;",
      selectInput(
        "start_date", "Beginning Inventory",
        choices  = date_choices,
        selected = as.character(inv_dates[length(inv_dates) - 1])
      ),
      selectInput(
        "end_date", "Ending Inventory",
        choices  = date_choices,
        selected = as.character(inv_dates[length(inv_dates)])
      ),
      tags$hr(style = "border-color: #21262d; margin: 10px 0;"),
      selectizeInput(
        "room_filter", "Room Filter (blank = all)",
        choices  = rooms,
        selected = NULL,
        multiple = TRUE,
        options  = list(placeholder = "All rooms")
      )
    )
  ),

  dashboardBody(
    use_theme(app_theme),
    tags$head(tags$style(HTML(dark_dt_css()))),
    tags$head(tags$style(HTML("
      .value-box-icon { font-size: 42px; }
      .small-box { border-radius: 6px; }
      .small-box .inner h3 { font-size: 22px; font-weight: 700; }
      .small-box .inner p { font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; }
      .box { border-radius: 6px; border-top: none; }
      .box-header { border-bottom: 1px solid #21262d; }
      .tab-content { padding-top: 10px; }
      .nav-tabs > li > a { color: #8b949e; }
      .nav-tabs > li.active > a { color: #c9d1d9; background-color: #161b22; border-color: #30363d; }
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
    "))),

    tabItems(

      # ── Overview ────────────────────────────────────────────────────────────
      tabItem("overview",
        fluidRow(
          valueBoxOutput("vb_beg_skus",  width = 3),
          valueBoxOutput("vb_beg_val",   width = 3),
          valueBoxOutput("vb_end_skus",  width = 3),
          valueBoxOutput("vb_end_val",   width = 3)
        ),
        fluidRow(
          valueBoxOutput("vb_sales_btl",  width = 3),
          valueBoxOutput("vb_sales_gl",   width = 3),
          valueBoxOutput("vb_sales_cogs", width = 3),
          valueBoxOutput("vb_purch_val",  width = 3)
        ),
        fluidRow(
          box(
            title = "Inventory Value Trend (2023 – Present)",
            width = 8, status = "primary", solidHeader = TRUE,
            plotOutput("trend_chart", height = "280px")
          ),
          box(
            title = "Period Summary",
            width = 4, status = "primary", solidHeader = TRUE,
            tags$table(
              style = "width:100%; color:#c9d1d9; font-size:13px; line-height:2;",
              tags$tr(
                tags$td(style = "color:#8b949e;", "Period"),
                tags$td(style = "text-align:right; font-weight:600;", textOutput("period_label", inline = TRUE))
              ),
              tags$tr(
                tags$td(style = "color:#8b949e;", "Beg. Variance"),
                tags$td(style = "text-align:right;", textOutput("beg_var_label", inline = TRUE))
              ),
              tags$tr(
                tags$td(style = "color:#8b949e;", "End. Variance"),
                tags$td(style = "text-align:right;", textOutput("end_var_label", inline = TRUE))
              ),
              tags$tr(
                tags$td(style = "color:#8b949e;", "Sales (bottles)"),
                tags$td(style = "text-align:right;", textOutput("total_btl_label", inline = TRUE))
              ),
              tags$tr(
                tags$td(style = "color:#8b949e;", "Sales (glasses)"),
                tags$td(style = "text-align:right;", textOutput("total_gl_label", inline = TRUE))
              ),
              tags$tr(
                tags$td(style = "color:#8b949e;", "Avg. Cost %"),
                tags$td(style = "text-align:right;", textOutput("avg_cost_pct_label", inline = TRUE))
              ),
              tags$tr(
                tags$td(style = "color:#8b949e;", "Purchases"),
                tags$td(style = "text-align:right;", textOutput("purch_label", inline = TRUE))
              )
            )
          )
        )
      ),

      # ── Inventory ───────────────────────────────────────────────────────────
      tabItem("inventory",
        tabBox(
          width = 12,
          tabPanel(
            "Beginning Inventory",
            DTOutput("beg_inv_table")
          ),
          tabPanel(
            "Ending Inventory",
            DTOutput("end_inv_table")
          )
        )
      ),

      # ── Sales ───────────────────────────────────────────────────────────────
      tabItem("sales",
        box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Sales by SKU — Period Totals",
          DTOutput("sales_table")
        )
      ),

      # ── Purchases ───────────────────────────────────────────────────────────
      tabItem("purchases",
        fluidRow(
          box(
            width = 4, status = "primary",
            title = "Filter Vendor",
            solidHeader = FALSE,
            selectizeInput(
              "vendor_filter", NULL,
              choices  = vendors,
              selected = NULL,
              multiple = TRUE,
              options  = list(placeholder = "All vendors")
            )
          ),
          box(
            width = 8, status = "primary",
            title = "Purchases by Vendor",
            solidHeader = FALSE,
            plotOutput("vendor_bar", height = "180px")
          )
        ),
        box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Purchase Detail",
          DTOutput("purch_table")
        )
      ),

      # ── Variance ────────────────────────────────────────────────────────────
      tabItem("variance",
        fluidRow(
          box(
            width = 6, status = "danger", solidHeader = TRUE,
            title = "Beginning Inventory — Top Variance by SKU",
            plotOutput("beg_var_chart", height = "260px")
          ),
          box(
            width = 6, status = "danger", solidHeader = TRUE,
            title = "Ending Inventory — Top Variance by SKU",
            plotOutput("end_var_chart", height = "260px")
          )
        ),
        tabBox(
          width = 12,
          tabPanel(
            "Beginning Variance",
            DTOutput("beg_var_table")
          ),
          tabPanel(
            "Ending Variance",
            DTOutput("end_var_table")
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  start_dt <- reactive(as.Date(input$start_date))
  end_dt   <- reactive(as.Date(input$end_date))

  # ── Filtered reactive datasets ─────────────────────────────────────────────
  beg_inv <- reactive({
    df <- inv_details %>% filter(inventory_date == start_dt())
    if (length(input$room_filter) > 0)
      df <- df %>% filter(room_name %in% input$room_filter)
    df
  })

  end_inv <- reactive({
    df <- inv_details %>% filter(inventory_date == end_dt())
    if (length(input$room_filter) > 0)
      df <- df %>% filter(room_name %in% input$room_filter)
    df
  })

  period_sales <- reactive({
    all_sales %>%
      filter(end_date > start_dt(), end_date <= end_dt())
  })

  period_purch <- reactive({
    df <- all_purchases %>%
      filter(end_date > start_dt(), end_date <= end_dt())
    if (length(input$vendor_filter) > 0)
      df <- df %>% filter(company_name %in% input$vendor_filter)
    df
  })

  beg_var <- reactive({
    beg_inv() %>%
      mutate(
        variance_qty = quantity_counted - theoretical_quantity,
        variance_ext = variance_qty * product_cost
      )
  })

  end_var <- reactive({
    end_inv() %>%
      mutate(
        variance_qty = quantity_counted - theoretical_quantity,
        variance_ext = variance_qty * product_cost
      )
  })

  # ── Value boxes ────────────────────────────────────────────────────────────
  output$vb_beg_skus <- renderValueBox({
    valueBox(
      fmt_number(nrow(beg_inv())),
      paste("Beg. Inventory —", format(start_dt(), "%b %d, %Y")),
      icon = icon("cubes"), color = "blue"
    )
  })

  output$vb_beg_val <- renderValueBox({
    valueBox(
      fmt_dollars(sum(beg_inv()$ext, na.rm = TRUE)),
      "Beginning Value",
      icon = icon("dollar-sign"), color = "blue"
    )
  })

  output$vb_end_skus <- renderValueBox({
    valueBox(
      fmt_number(nrow(end_inv())),
      paste("End. Inventory —", format(end_dt(), "%b %d, %Y")),
      icon = icon("cubes"), color = "navy"
    )
  })

  output$vb_end_val <- renderValueBox({
    valueBox(
      fmt_dollars(sum(end_inv()$ext, na.rm = TRUE)),
      "Ending Value",
      icon = icon("dollar-sign"), color = "navy"
    )
  })

  output$vb_sales_btl <- renderValueBox({
    valueBox(
      fmt_number(sum(period_sales()$btl_sales, na.rm = TRUE)),
      "Bottles Sold",
      icon = icon("wine-glass-alt"), color = "green"
    )
  })

  output$vb_sales_gl <- renderValueBox({
    valueBox(
      fmt_number(sum(period_sales()$gl_sales, na.rm = TRUE)),
      "Glasses Sold",
      icon = icon("cocktail"), color = "green"
    )
  })

  output$vb_sales_cogs <- renderValueBox({
    valueBox(
      fmt_dollars(sum(period_sales()$total_sales_ext, na.rm = TRUE)),
      "Sales COGS",
      icon = icon("chart-line"), color = "teal"
    )
  })

  output$vb_purch_val <- renderValueBox({
    total_purch <- all_purchases %>%
      filter(end_date > start_dt(), end_date <= end_dt()) %>%
      summarise(total = sum(ext, na.rm = TRUE)) %>%
      pull(total)
    valueBox(
      fmt_dollars(total_purch),
      "Purchases (all vendors)",
      icon = icon("truck"), color = "yellow"
    )
  })

  # ── Period summary labels ──────────────────────────────────────────────────
  output$period_label <- renderText({
    paste(format(start_dt(), "%b %d"), "→", format(end_dt(), "%b %d, %Y"))
  })

  output$beg_var_label <- renderText({
    v <- sum(beg_var()$variance_ext, na.rm = TRUE)
    fmt_dollars(v)
  })

  output$end_var_label <- renderText({
    v <- sum(end_var()$variance_ext, na.rm = TRUE)
    fmt_dollars(v)
  })

  output$total_btl_label <- renderText({
    fmt_number(sum(period_sales()$btl_sales, na.rm = TRUE))
  })

  output$total_gl_label <- renderText({
    fmt_number(sum(period_sales()$gl_sales, na.rm = TRUE))
  })

  output$avg_cost_pct_label <- renderText({
    s <- period_sales()
    cogs  <- sum(s$total_sales_ext, na.rm = TRUE)
    sales <- sum(s$total_sales_ext / ifelse(is.na(s$cost_percent) | s$cost_percent == 0, NA, s$cost_percent / 100),
                 na.rm = TRUE)
    if (is.na(sales) || sales == 0) return("—")
    scales::percent(cogs / sales, accuracy = 0.1)
  })

  output$purch_label <- renderText({
    total_purch <- all_purchases %>%
      filter(end_date > start_dt(), end_date <= end_dt()) %>%
      summarise(total = sum(ext, na.rm = TRUE)) %>%
      pull(total)
    fmt_dollars(total_purch)
  })

  # ── Trend chart ────────────────────────────────────────────────────────────
  output$trend_chart <- renderPlot({
    bg <- "#161b22"
    accent <- "#1f6feb"
    highlight <- "#3fb950"

    ggplot(inv_totals, aes(x = inventory_date, y = inv_total)) +
      annotate("rect",
        xmin = start_dt(), xmax = end_dt(),
        ymin = -Inf, ymax = Inf,
        fill = "#1f6feb", alpha = 0.08
      ) +
      geom_line(color = accent, linewidth = 1.1) +
      geom_point(
        data = inv_totals %>%
          filter(inventory_date %in% c(start_dt(), end_dt())),
        color = highlight, size = 3.5
      ) +
      geom_point(color = accent, size = 1.8) +
      scale_y_continuous(
        labels = scales::dollar_format(scale = 1e-3, suffix = "K"),
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      scale_x_date(date_labels = "%b '%y", date_breaks = "3 months") +
      labs(x = NULL, y = "Inventory Value") +
      gg_dark_theme()
  }, bg = "#161b22")

  # ── Inventory tables ───────────────────────────────────────────────────────
  inv_dt <- function(df) {
    df %>%
      select(
        Room = room_name,
        SKU  = inventory_name,
        Counted = quantity_counted,
        Theoretical = theoretical_quantity,
        `Unit Cost` = product_cost,
        `Ext. Value` = ext
      ) %>%
      arrange(Room, SKU) %>%
      datatable(
        extensions = "Buttons",
        options = list(
          dom         = "Bfrtip",
          buttons     = list("csv", "excel"),
          pageLength  = 25,
          scrollX     = TRUE,
          order       = list(list(0, "asc"))
        ),
        rownames = FALSE,
        class    = "compact"
      ) %>%
      formatCurrency(c("Unit Cost", "Ext. Value"), digits = 2) %>%
      formatRound(c("Counted", "Theoretical"), digits = 0)
  }

  output$beg_inv_table <- DT::renderDT(inv_dt(beg_inv()))
  output$end_inv_table <- DT::renderDT(inv_dt(end_inv()))

  # ── Sales table ────────────────────────────────────────────────────────────
  output$sales_table <- DT::renderDT({
    period_sales() %>%
      group_by(SKU = inventory_name) %>%
      summarise(
        `Btl. Sales`  = sum(btl_sales,      na.rm = TRUE),
        `Btl. COGS`   = sum(btl_sales_ext,  na.rm = TRUE),
        `Gl. Sales`   = sum(gl_sales,       na.rm = TRUE),
        `Gl. COGS`    = sum(gl_sales_ext,   na.rm = TRUE),
        `Total Units` = sum(total_sales,    na.rm = TRUE),
        `Total COGS`  = sum(total_sales_ext, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(`Total Units` > 0) %>%
      arrange(desc(`Total COGS`)) %>%
      datatable(
        extensions = "Buttons",
        options = list(
          dom        = "Bfrtip",
          buttons    = list("csv", "excel"),
          pageLength = 25,
          scrollX    = TRUE
        ),
        rownames = FALSE,
        class    = "compact"
      ) %>%
      formatCurrency(c("Btl. COGS", "Gl. COGS", "Total COGS"), digits = 2) %>%
      formatRound(c("Btl. Sales", "Gl. Sales", "Total Units"), digits = 0)
  })

  # ── Purchases chart ────────────────────────────────────────────────────────
  output$vendor_bar <- renderPlot({
    df <- all_purchases %>%
      filter(end_date > start_dt(), end_date <= end_dt()) %>%
      group_by(Vendor = company_name) %>%
      summarise(Total = sum(ext, na.rm = TRUE), .groups = "drop") %>%
      arrange(Total) %>%
      mutate(Vendor = factor(Vendor, levels = Vendor))

    ggplot(df, aes(x = Total, y = Vendor)) +
      geom_col(fill = "#1f6feb", width = 0.7) +
      geom_text(aes(label = scales::dollar(Total, accuracy = 1)),
                hjust = -0.08, size = 3, color = "#8b949e") +
      scale_x_continuous(
        labels = scales::dollar_format(scale = 1e-3, suffix = "K"),
        expand = expansion(mult = c(0, 0.2))
      ) +
      labs(x = NULL, y = NULL) +
      gg_dark_theme() +
      theme(panel.grid.major.y = element_blank())
  }, bg = "#161b22")

  # ── Purchases table ────────────────────────────────────────────────────────
  output$purch_table <- DT::renderDT({
    period_purch() %>%
      select(
        Vendor        = company_name,
        Invoice       = vendor_invoice_id,
        `Invoice Date` = invoice_date,
        `Pay Date`    = payment_date,
        SKU           = inventory_name,
        `Qty Rec.`    = number_received,
        `Unit Price`  = price_per_unit,
        `Ext. Cost`   = ext
      ) %>%
      arrange(Vendor, `Invoice Date`, SKU) %>%
      datatable(
        extensions = "Buttons",
        options = list(
          dom        = "Bfrtip",
          buttons    = list("csv", "excel"),
          pageLength = 25,
          scrollX    = TRUE
        ),
        rownames = FALSE,
        class    = "compact"
      ) %>%
      formatCurrency(c("Unit Price", "Ext. Cost"), digits = 2)
  })

  # ── Variance charts ────────────────────────────────────────────────────────
  var_bar_chart <- function(df_var, n = 15) {
    df <- df_var %>%
      arrange(variance_ext) %>%
      slice_head(n = n) %>%
      mutate(
        SKU   = ifelse(nchar(inventory_name) > 35, paste0(substr(inventory_name, 1, 32), "..."), inventory_name),
        SKU   = factor(SKU, levels = SKU),
        color = ifelse(variance_ext < 0, "#f85149", "#3fb950")
      )

    ggplot(df, aes(x = variance_ext, y = SKU, fill = color)) +
      geom_col(width = 0.75, show.legend = FALSE) +
      geom_vline(xintercept = 0, color = "#30363d", linewidth = 0.5) +
      geom_text(aes(
        label = scales::dollar(variance_ext, accuracy = 1),
        hjust = ifelse(variance_ext < 0, 1.08, -0.08)
      ), size = 2.8, color = "#8b949e") +
      scale_fill_identity() +
      scale_x_continuous(
        labels = scales::dollar,
        expand = expansion(mult = c(0.15, 0.2))
      ) +
      labs(x = "Variance ($)", y = NULL) +
      gg_dark_theme() +
      theme(
        axis.text.y       = element_text(size = 8),
        panel.grid.major.y = element_blank()
      )
  }

  output$beg_var_chart <- renderPlot(
    var_bar_chart(beg_var()), bg = "#161b22"
  )

  output$end_var_chart <- renderPlot(
    var_bar_chart(end_var()), bg = "#161b22"
  )

  # ── Variance tables ────────────────────────────────────────────────────────
  var_dt <- function(df_var) {
    df_var %>%
      select(
        Room = room_name,
        SKU  = inventory_name,
        Counted     = quantity_counted,
        Theoretical = theoretical_quantity,
        `Var. Qty`  = variance_qty,
        `Unit Cost` = product_cost,
        `Var. $`    = variance_ext
      ) %>%
      arrange(`Var. $`) %>%
      datatable(
        extensions = "Buttons",
        options = list(
          dom        = "Bfrtip",
          buttons    = list("csv", "excel"),
          pageLength = 25,
          scrollX    = TRUE
        ),
        rownames = FALSE,
        class    = "compact"
      ) %>%
      formatCurrency(c("Unit Cost", "Var. $"), digits = 2) %>%
      formatRound(c("Counted", "Theoretical", "Var. Qty"), digits = 0) %>%
      formatStyle(
        "Var. $",
        color = styleInterval(c(-0.01, 0.01), c("#f85149", "#8b949e", "#3fb950"))
      )
  }

  output$beg_var_table <- DT::renderDT(var_dt(beg_var()))
  output$end_var_table <- DT::renderDT(var_dt(end_var()))
}

shinyApp(ui, server)
