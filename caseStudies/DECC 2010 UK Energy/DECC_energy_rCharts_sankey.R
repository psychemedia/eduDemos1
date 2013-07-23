#Inspired by @timelyportfolio - All My Roads Lead Back to Finance–PIMCO Sankey
#http://timelyportfolio.blogspot.co.uk/2013/07/all-my-roads-lead-back-to-financepimco.html


#Here's the baseline column naming for the dataset
colnames(DECC.overall.energy)=c('Sector','Enduse','EnergyType','value')

#The plotting routines require column names to be specified as:
##source, target, value
#to show what connects to what and by what thickness line

#If we want to plot from enduse to energytype we need this relabelling
workingdata=DECC.overall.energy
colnames(workingdata)=c('Sector','source','target','value')

#Now let's create a Sankey diagram
##http://ramnathv.github.io/rCharts/
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
  data = workingdata,
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
#a source and the sum of that sector's energy use by End Use
#Recover the colnames so we can see what's going on
sectorEnergy=aggregate(value ~ Sector + Enduse, DECC.overall.energy, sum)
colnames(sectorEnergy)=c('source','target','value')

#We can now generate a single data file combing all source and target data
energyfull=subset(workingdata,select=c('source','target','value'))
energyfull=rbind(energyfull,sectorEnergy)

#Let's plot the new chart
#To make thinks easier, let's abstract a little more...
sankeyPlot=function(df){
  sankeyPlot <- rCharts$new()
  
  #We need to tell R where the Sankey library is.
  #I put it as a subdirectory to my current working directory (.)
  sankeyPlot$setLib('./rCharts_d3_sankey-gh-pages/')
  
  #We also need to point to an HTML template page
  sankeyPlot$setTemplate(script = "./rCharts_d3_sankey-gh-pages/layouts/chart.html")
  
  sankeyPlot$set(
    data = df,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = 750,
    height = 500,
    labelFormat = ".1%"
  )
  
  sankeyPlot
}

sankeyPlot(energyfull)


#Let's do some more digging...

#How much of each energy type does each sector use
enduseBySector=aggregate(value ~ Sector + Enduse, DECC.overall.energy, sum)
colnames(enduseBySector)=c('source','target','value')
sankeyPlot(enduseBySector)

colnames(enduseBySector)=c('target','source','value')
sankeyPlot(enduseBySector)

#How much of each energy type is associated with each enduse
energyByEnduse=aggregate(value ~ EnergyType + Enduse, DECC.overall.energy, sum)
colnames(energyByEnduse)=c('source','target','value')

sankeyPlot(energyByEnduse)

