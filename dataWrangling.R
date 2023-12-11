fan_df <- read.csv("2022-2023-Fan-Attendance.csv")
win_df <- read.csv("2022-2023-Win-Percentage-Actual.csv")

df <- merge(x = fan_df, y = win_df, by.x = "TEAM", by.y = "Team", all.x = TRUE)

get_home_attendance_avg <- function(df, team_name) {
  get_home_attendance_avg_df <- select(filter(df, TEAM == team_name), Home_AVG)
  result = get_home_attendance_avg_df[1,1]
  result = paste("Home Attendance Average:", result)
  result = h5(result)
  return(result)
}
get_home_win_percentange <- function(df, team_name) {
  get_home_win_percentange_df <- select(filter(df, TEAM == team_name), Home)
  result = get_home_win_percentange_df[1,1]
  result = paste("Home Win Percent:", result)
  result = h5(result)
  return(result)
}

get_away_attendance_avg <- function(df, team_name) {
  get_away_attendance_avg_df <- select(filter(df, TEAM == team_name), Road_AVG)
  result = get_away_attendance_avg_df[1,1]
  result = paste("Away Attendance Average:", result)
  result = h5(result)
  return(result)
}
get_away_win_percentange <- function(df, team_name) {
  get_away_win_percentange_df <- select(filter(df, TEAM == team_name), Away)
  result = get_away_win_percentange_df[1,1]
  result = paste("Away Win Percent:", result)
  result = h5(result)
  return(result)
}

get_team_name <- function(df, rank) {
  get_team_name_df <- select(filter(df, Rank == rank), TEAM)
  result = get_team_name_df[1,1]
  result = paste("Team name:", result)
  result = h5(result)
  return(result)
}

get_win_percent_overall <- function(df, rank) {
  get_win_percent_overall_df <- select(filter(df, Rank == rank), X2022)
  number = get_win_percent_overall_df[1,1]
  result = paste("Overall Win Percentange:", number)
  result = h5(result)
  return(result)
}

bucks = get_win_percent_overall(df, 1)
