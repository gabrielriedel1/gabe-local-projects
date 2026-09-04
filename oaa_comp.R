library(tidyverse)
library(readxl)
library(RODBC)
library(ggplot2)
library(plotly)


##########################################################
## Actually Gabe Practicing Queries + Data Manipulation ##
##########################################################


# Local path to savant defense excel file
savant_data_path <- "data/outs_above_average_savant.xlsx"


# Joins Savant (s) DRS with Mariners (m) DRS aggregated from DB
join_comp_drs <- function(filepath) {
  
  savant_drs_df <- read_excel(filepath)

  dw01 <- odbcDriverConnect(
    connection = paste0(
    "Driver={SQL Server};",
    "server=Baseball-DW01;",
    "trusted_connection=yes"
  )
)

  query_text <- paste0("
  SELECT d.[fielder_id]
      ,d.[fielder_name]
      ,d.[year]
      ,ROUND(SUM(d.[total_runs_saved]), 2) AS m_drs
  FROM [BBOps].[dbo].[defense] d
      INNER JOIN (SELECT [play_id], [game_type]
                  FROM [BBOps].[dbo].[pitches]
                  WHERE [year] = 2026
                  ) AS t1
      ON t1.[play_id] = d.[play_id]
  WHERE d.[year] = 2026
      AND t1.[game_type] = 'R'
  GROUP BY d.[fielder_id], d.[fielder_name], d.[year]
  ORDER BY 4 DESC
  ")

  df <- sqlQuery(dw01, query_text, stringsAsFactors = FALSE)
  on.exit(odbcClose(dw01))

  joined_df <- savant_drs_df |>
    left_join(df, by = c("player_id" = "fielder_id", "year" = "year"))

  joined_df
}

# Create joined m and s DRS data frame
m_s_df <- join_comp_drs(savant_data_path)

# Create columns to quantify differences in DRS between m and s
find_delta_drs <- function(df) {
  mutated_df <- df |>
    rename(s_drs = fielding_runs_prevented, player_name = `last_name, first_name`) |>
    mutate(delta_drs = m_drs - s_drs) |>
    mutate(
      pos_to_neg = if_else(m_drs >= 0 & s_drs < 0, 1, 0),
      neg_to_pos = if_else(m_drs <= 0 & s_drs > 0, 1, 0)
    ) |>
    arrange(desc(delta_drs))
  mutated_df
}

# Update data with new columns
mutated_df <- find_delta_drs(m_s_df)



mutated_df |>
  group_by(primary_pos_formatted) |>
  summarize(pos_count = n())

mutated_df |>
  mutate(position_type = factor(if_else(
    primary_pos_formatted %in% c('CF', 'LF', 'RF'),
    'OF',
    'IF'
  ))) |>
  group_by(position_type) |>
  summarize(pos_count = n(),
            pos_prop = n() / nrow(mutated_df))

##############################
## CLAUDE GENERATED VISUALS ##
##############################

# Shared house-style histogram for a delta_drs distribution
plot_drs_delta_hist <- function(df, title, subtitle) {
  ink    <- "#0C2C56"
  muted  <- "#666666"
  faint  <- "#bbbbbb"
  accent <- "#007A7A"

  delta_range <- range(df$delta_drs, na.rm = TRUE)
  zero_in_range <- delta_range[1] <= 0 && delta_range[2] >= 0

  p <- df |>
    mutate(large_gap = abs(delta_drs) > 10) |>
    ggplot(aes(x = delta_drs, fill = large_gap)) +
    geom_histogram(binwidth = 2, center = 0, color = "white", linewidth = 0.15)

  if (zero_in_range) {
    p <- p + geom_vline(xintercept = 0, color = faint, linewidth = 0.6, linetype = "dashed")
  }

  p +
    scale_fill_manual(values = c(`FALSE` = muted, `TRUE` = accent), guide = "none") +
    scale_x_continuous(
      expand = expansion(mult = 0.04),
      breaks = scales::breaks_pretty(n = 6)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Delta DRS (runs)",
      y = NULL,
      caption = "Source: BBOps.dbo.defense (Mariners internal) vs. Baseball Savant outs-above-average export"
    ) +
    theme_minimal(base_family = "serif") +
    theme(
      panel.grid    = element_blank(),
      axis.text.y   = element_blank(),
      axis.ticks.y  = element_blank(),
      axis.line.x   = element_line(color = ink),
      axis.ticks.x  = element_line(color = ink),
      axis.text.x   = element_text(color = ink),
      axis.title.x  = element_text(color = muted),
      plot.title    = element_text(color = ink, face = "bold", size = 14),
      plot.subtitle = element_text(color = muted, size = 10.5),
      plot.caption  = element_text(color = muted, size = 8, hjust = 0)
    )
}

build_drs_delta_hist <- function(df) {
  plot_drs_delta_hist(
    df,
    title = "How Mariners and Savant DRS estimates compare",
    subtitle = "Delta DRS = Mariners internal DRS - Savant DRS, 2026 season"
  )
}

# Interactive dot-histogram: same buckets as plot_drs_delta_hist, but each
# player is its own hoverable point (stacked within its bucket) instead of
# a bar, so individual players stay visible and inspectable.
plot_drs_delta_dots <- function(df, title, subtitle, binwidth = 2) {
  ink    <- "#0C2C56"
  muted  <- "#666666"
  faint  <- "#bbbbbb"
  accent <- "#007A7A"

  dot_df <- df |>
    mutate(
      bin_center = binwidth * round(delta_drs / binwidth),
      large_gap  = abs(delta_drs) > 10,
      tooltip = paste0(
        player_name,
        "<br>Mariners DRS: ", m_drs,
        "<br>Savant DRS: ", s_drs,
        "<br>Delta DRS: ", delta_drs
      )
    ) |>
    arrange(bin_center, delta_drs) |>
    group_by(bin_center) |>
    mutate(stack_pos = row_number()) |>
    ungroup()

  delta_range <- range(dot_df$delta_drs, na.rm = TRUE)
  zero_in_range <- delta_range[1] <= 0 && delta_range[2] >= 0

  p <- ggplot(dot_df, aes(x = bin_center, y = stack_pos, text = tooltip))

  if (zero_in_range) {
    p <- p + geom_vline(xintercept = 0, color = faint, linewidth = 0.6, linetype = "dashed")
  }

  p <- p +
    geom_point(aes(fill = large_gap), shape = 21, color = "white", size = 4.2, stroke = 0.3) +
    scale_fill_manual(values = c(`FALSE` = muted, `TRUE` = accent), guide = "none") +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 6),
      expand = expansion(mult = 0.06)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
    labs(title = title, subtitle = subtitle, x = "Delta DRS (runs)", y = NULL) +
    theme_minimal(base_family = "serif") +
    theme(
      panel.grid    = element_blank(),
      axis.text.y   = element_blank(),
      axis.ticks.y  = element_blank(),
      axis.line.x   = element_line(color = ink),
      axis.ticks.x  = element_line(color = ink),
      axis.text.x   = element_text(color = ink),
      axis.title.x  = element_text(color = muted),
      plot.title    = element_text(color = ink, face = "bold", size = 14),
      plot.subtitle = element_text(color = muted, size = 10.5)
    )

  ggplotly(p, tooltip = "text") |>
    layout(
      font = list(family = "Georgia, serif", color = ink),
      hoverlabel = list(bgcolor = ink, font = list(color = "white", family = "Georgia, serif"))
    )
}

build_drs_delta_hist_pos_to_neg <- function(df) {
  plot_drs_delta_dots(
    df |> filter(pos_to_neg == 1),
    title = "DRS discrepancies where Mariners rate a player positive and Savant rates them negative",
    subtitle = "Delta DRS = Mariners internal DRS - Savant DRS, 2026 season"
  )
}

build_drs_delta_hist_neg_to_pos <- function(df) {
  plot_drs_delta_dots(
    df |> filter(neg_to_pos == 1),
    title = "DRS discrepancies where Mariners rate a player negative and Savant rates them positive",
    subtitle = "Delta DRS = Mariners internal DRS - Savant DRS, 2026 season"
  )
}

build_drs_delta_hist(mutated_df)
build_drs_delta_hist_pos_to_neg(mutated_df)
build_drs_delta_hist_neg_to_pos(mutated_df)

mutated_df |>
  summarize(tot_pos_to_neg = sum(pos_to_neg, na.rm = TRUE),
            tot_neg_to_pos = sum(neg_to_pos, na.rm = TRUE))

##############################
## CLAUDE GENERATED EXPORT  ##
##############################

library(openxlsx)

# Maps a numeric vector to hex colors on a diverging red-white-green scale,
# scaled independently to that vector's own min/max, anchored at mid_val
# (each half of the scale stretches to its own extreme, so a mostly-positive
# column still shows full-strength red at its one negative outlier)
value_to_hex <- function(x, min_val, max_val, mid_val = 0,
                          neg = "#C00000", mid = "#FFFFFF", pos = "#00B050") {
  x <- pmin(pmax(x, min_val), max_val)
  ramp_neg <- colorRampPalette(c(neg, mid))(101)
  ramp_pos <- colorRampPalette(c(mid, pos))(101)

  out <- rep(NA_character_, length(x))
  is_neg <- !is.na(x) & x <= mid_val
  is_pos <- !is.na(x) & x > mid_val

  if (any(is_neg)) {
    frac <- if (mid_val > min_val) (x[is_neg] - min_val) / (mid_val - min_val) else 1
    out[is_neg] <- ramp_neg[round(frac * 100) + 1]
  }
  if (any(is_pos)) {
    frac <- if (max_val > mid_val) (x[is_pos] - mid_val) / (max_val - mid_val) else 1
    out[is_pos] <- ramp_pos[round(frac * 100) + 1]
  }
  out
}

# Writes mutated_df to a colored .xlsx: s_drs, m_drs, delta_drs are each
# shaded on their own red-white-green scale (white = 0), everything else
# stays plain
export_colored_drs_excel <- function(df, path) {
  export_df <- df |> select(-fielder_name, -outs_above_average)

  wb <- createWorkbook()
  addWorksheet(wb, "DRS Comparison")
  writeData(wb, "DRS Comparison", export_df)

  color_cols <- c("s_drs", "m_drs", "delta_drs")
  for (col in color_cols) {
    col_idx <- which(names(export_df) == col)
    vals <- export_df[[col]]
    hex_colors <- value_to_hex(
      vals,
      min_val = min(vals, na.rm = TRUE),
      max_val = max(vals, na.rm = TRUE),
      mid_val = 0
    )
    for (row in seq_along(vals)) {
      if (!is.na(hex_colors[row])) {
        addStyle(
          wb, "DRS Comparison",
          style = createStyle(fgFill = hex_colors[row]),
          rows = row + 1, cols = col_idx, gridExpand = FALSE
        )
      }
    }
  }

  saveWorkbook(wb, path, overwrite = TRUE)
}

export_colored_drs_excel(mutated_df, "drs_comparison_colored.xlsx")
