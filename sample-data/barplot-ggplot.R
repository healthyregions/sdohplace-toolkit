chart_data <- nyc_data[nyc_data$NTAName]

# Lose the spatial component for the chart data
racial_data <- st_drop_geometry(nyc_data)

# Subselect to only variables we're interested in
racial_data <- racial_data[, c("pctblack", "pcthisp", "pctwhite", "pctapi", "pctother")]

# Light cleaning 
racial_data <- t(racial_data)
racial_data <- as.data.frame(racial_data)
racial_data <- cbind(Race = rownames(racial_data), Percentage = racial_data[, 1])
rownames(racial_data) <- NULL

ggplot(racial_data, aes(x=Race, y=Percentage)) + 
  geom_bar(stat = "identity")
