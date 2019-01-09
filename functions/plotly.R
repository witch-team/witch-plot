#Plotly graphs
library(plotly)



#Create plotly Panel
# Create a shareable link to your chart
Sys.setenv("plotly_username"="johannes.emm")
Sys.setenv("plotly_api_key"="Gn54gpjB6p4p5wJEO9O7")
#plotly()





#get dynamic ggplotly plots
ggplotly() #just execute after each plot












p <- ggplot2::last_plot()
p_plotly <- ggplotly(p)
# Set up API credentials: https://plot.ly/r/getting-started
plot_for_web = api_create(p_plotly, filename="plotly_test", fileopt = "overwrite")
plot_for_web


