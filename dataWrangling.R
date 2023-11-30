fan_df <- read.csv("2022-2023-Fan-Attendance.csv")
win_df <- read.csv("2022-2023-Win-Percentage-Actual.csv")

df <- merge(x = fan_df, y = win_df, by.x = "TEAM", by.y = "Team", all.x = TRUE)
