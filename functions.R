library(ggplot2)
library(scales)

fmt_dollars <- function(x, digits = 0) {
  ifelse(is.na(x) | is.nan(x), "—",
    scales::dollar(as.numeric(x), accuracy = if (digits == 0) 1 else 0.01))
}

fmt_number <- function(x) {
  ifelse(is.na(x), "—", scales::comma(as.numeric(x), accuracy = 1))
}

dark_dt_css <- function() {
  paste0(
    "table.dataTable tbody tr { background-color: #161b22 !important; color: #c9d1d9; }",
    "table.dataTable tbody tr:hover { background-color: #1c2128 !important; }",
    "table.dataTable tbody tr.odd { background-color: #161b22 !important; }",
    "table.dataTable tbody tr.even { background-color: #0d1117 !important; }",
    "table.dataTable thead th {",
    "  background-color: #0d1117 !important; color: #8b949e;",
    "  border-bottom: 1px solid #30363d !important;",
    "  border-top: 1px solid #30363d !important; }",
    "table.dataTable thead tr { background-color: #0d1117 !important; }",
    "table.dataTable.no-footer { border-bottom: 1px solid #30363d; }",
    ".dataTables_wrapper { color: #8b949e; }",
    ".dataTables_wrapper .dataTables_info,",
    ".dataTables_wrapper .dataTables_length,",
    ".dataTables_wrapper .dataTables_filter { color: #8b949e !important; }",
    ".dataTables_wrapper .dataTables_filter input {",
    "  background-color: #0d1117; color: #c9d1d9;",
    "  border: 1px solid #30363d; border-radius: 4px; padding: 2px 6px; }",
    ".dataTables_wrapper .dataTables_length select {",
    "  background-color: #0d1117; color: #c9d1d9; border: 1px solid #30363d; }",
    ".dataTables_paginate .paginate_button { color: #8b949e !important; }",
    ".dataTables_paginate .paginate_button.current {",
    "  background: #1f6feb !important; color: #fff !important;",
    "  border-color: #1f6feb !important; }",
    ".dataTables_paginate .paginate_button:hover {",
    "  background: #21262d !important; color: #c9d1d9 !important; }",
    ".dt-buttons .dt-button {",
    "  background-color: #21262d !important; color: #c9d1d9 !important;",
    "  border: 1px solid #30363d !important; border-radius: 4px !important; }",
    ".dt-buttons .dt-button:hover { background-color: #30363d !important; }",
    ".box-body { padding-top: 8px; }"
  )
}

gg_dark_theme <- function() {
  theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "#161b22", color = NA),
    panel.background  = element_rect(fill = "#161b22", color = NA),
    panel.grid.major  = element_line(color = "#21262d", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    text              = element_text(color = "#c9d1d9"),
    axis.text         = element_text(color = "#8b949e", size = 9),
    axis.title        = element_text(color = "#8b949e", size = 10),
    plot.title        = element_text(color = "#e6edf3", size = 13, face = "bold"),
    plot.subtitle     = element_text(color = "#8b949e", size = 10),
    plot.margin       = margin(12, 16, 8, 12),
    legend.background = element_rect(fill = "#161b22", color = NA),
    legend.text       = element_text(color = "#c9d1d9"),
    strip.text        = element_text(color = "#c9d1d9")
  )
}
