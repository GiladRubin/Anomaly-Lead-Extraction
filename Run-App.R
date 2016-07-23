if (!"shiny" %in% installed.packages()[, "Package"])
  install.packages("shiny", dependencies = TRUE)
library("shiny")
runGitHub(repo = "GiladRubin/Anomaly-Lead-Extraction")