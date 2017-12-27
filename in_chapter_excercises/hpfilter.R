#################################################
#                                               #    
#    Excerice on Hodrick–Prescott (HP) filter   #
#    pag. 252                                   #
#                                               #
#################################################

# Load libraries
library(forecast)
library(FinTS)
library(haven)
library(gdata)
library(lmtest)
library(qcc)
library(urca)
library(tseries)
library(plm)
library(quantmod)
library(mFilter)
library(ggplot2)
library(gridExtra)
library(reshape2)

# Import data and organize it
dataset <- read_sas("/your_path/panel.sas7bdat")
time = dataset[,1]

# USA data
USA = dataset[,9]
USA = ts(USA)
HPUSA = hpfilter(USA, type="lambda")
HPUSA
objectList = list(HPUSA$x,HPUSA$trend,HPUSA$cycle)
names(objectList) = c("HPUSA","trend","cycle")
sapply(objectList,class)

#Conversion from ts to data.frame
fn_ts_to_DF = function(x)  {
  DF = data.frame(date=zoo::as.Date(time(objectList[[x]])),tseries=as.matrix(objectList[[x]])) 
  colnames(DF)[2]=names(objectList)[x]
  return(DF)
}
DFList = lapply(seq_along(objectList),fn_ts_to_DF)
names(DFList) = c("HPUSA","trend","cycle")
seriesTrend = merge(DFList$HPUSA,DFList$trend,by="date")
cycleSeries = DFList$cycle

# Plots
gSeries = ggplot(melt(seriesTrend,"date"),aes(x=date,y=value,color=variable)) + geom_line() +
          ggtitle('Hodrick-Prescot Filter for HPUSA') + 
          theme(legend.title = element_blank(),legend.justification = c(0.1, 0.8), legend.position = c(0, 1),
          legend.direction = "horizontal",legend.background = element_rect(fill="transparent",size=.5, linetype="dotted"))
gCycle =  ggplot(cycleSeries,aes(x=date,y=cycle)) + geom_line(color="#619CFF") + ggtitle("Cyclical component (deviations from trend)")

gComb = grid.arrange(gSeries,gCycle,nrow=2)
