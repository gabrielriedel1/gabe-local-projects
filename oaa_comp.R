library(tidyverse)
library(readxl)
library(RODBC)
library(ggplot2)

savant_data_path <- "data/outs_above_average_savant.xlsx"

join_comp_drs <- function(filepath) {
  savant_oaa_df <- read_excel(filepath)

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

  joined_df <- savant_oaa_df |>
    left_join(df, by = c("player_id" = "fielder_id", "year" = "year"))

  joined_df
}


final_df <- join_comp_drs(savant_data_path)
View(final_df)

find_delta_drs <- function(df) {
  mutated_df <- df |>
    rename(s_drs = fielding_runs_prevented) |>
    mutate(delta_drs = m_drs - s_drs) |>
    mutate(
      pos_to_neg = if_else(m_drs >= 0 & s_drs < 0, 1, 0),
      neg_to_pos = if_else(m_drs <= 0 & s_drs > 0, 1, 0)
    ) |>
    arrange(desc(delta_drs))
  mutated_df
}

mutated <- find_delta_drs(final_df)


build_drs_delta_hist <- function(df) {
  df |>
    ggplot(aes(x=delta_drs)) +
    geom_histogram()
}

build_drs_delta_hist(mutated)
