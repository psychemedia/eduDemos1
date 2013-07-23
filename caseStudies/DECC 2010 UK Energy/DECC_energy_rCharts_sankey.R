#Inspired by @timelyportfolio - All My Roads Lead Back to Finance–PIMCO Sankey
#http://timelyportfolio.blogspot.co.uk/2013/07/all-my-roads-lead-back-to-financepimco.html

colnames(DECC.overall.energy)=c('Sector','source','target','value')

require(rCharts)

#Download and unzip @timelyportfolio's Sankey/rCharts package
#https://github.com/timelyportfolio/rCharts_d3_sankey

sankeyPlot <- rCharts$new()

#We need to tell R where the Sankey library is.
#I put it as a subdirectory to my current working directory (.)
sankeyPlot$setLib('./rCharts_d3_sankey-gh-pages/')

#We also need to point to an HTML template page
sankeyPlot$setTemplate(script = "./rCharts_d3_sankey-gh-pages/layouts/chart.html")

sankeyPlot$set(
  data = DECC.overall.energy,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ".1%"
)

sankeyPlot

#If we want to add in a further layer, showing how each Sector contributes
#to the End-use energy usage, we need to additionally treat the Sector as
#a source and the sum of that secotr's energy use by End Use
#Recall, the source column in the data file corresponds to End use.
sectorEnergy=aggregate(value ~ Sector + source, DECC.overall.energy, sum)
colnames(sectorEnergy)=c('source','target','value')

#We can now generate a single data file combing all source and target data
energyfull=subset(DECC.overall.energy,select=c('source','target','value'))
energyfull=rbind(energyfull,sectorEnergy)

#Let's plot the new chart
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('./rCharts_d3_sankey-gh-pages/')
sankeyPlot$setTemplate(script = "./rCharts_d3_sankey-gh-pages/layouts/chart.html")

sankeyPlot$set(
  data = energyfull,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ".1%"
)

sankeyPlot