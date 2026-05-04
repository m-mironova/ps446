#change tab-separated Harvard Dataverse files to comma-separated
experimental_data <- read.delim("experimental_data.tab")
write.csv(experimental_data, "experimental_data.csv", row.names = FALSE)

pretest_data <- read.delim("pretest_data.tab")
write.csv(pretest_data, "pretest_data.csv", row.names = FALSE)

state_population <- read.delim("state_population.tab")
write.csv(state_population, "state_population.csv", row.names = FALSE)