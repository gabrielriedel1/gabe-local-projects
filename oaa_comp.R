library(tidyverse)
library(readxl)
library(RODBC)

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
      ,ROUND(SUM(d.[total_runs_saved]), 2) AS mariners_drs
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

find_delta_drs <- function(joined_df) {
  mutated_df <- joined_df |>
    mutate(delta_drs = fielding_runs_prevented - mariners_drs)
  
  mutated_df
}

mutated <- find_delta_drs(final_df)
