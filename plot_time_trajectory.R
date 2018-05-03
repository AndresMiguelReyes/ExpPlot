library(ggplot2)

### FUNCTIONIZE AND FIX DATA FRAME STRUCT ###

buildCat = function()
{
  values = vector()
  states_cat = vector()
  for (i in 1:8)
  {
    states_cat = c(states_cat,rep(paste0("category", i), each = 4+(i-1)))
    end_val = 4+(i-1)
    values = c(values,seq(1,end_val))
  }
  for (i in 9:37)
  {
    states_cat = c(states_cat,rep(paste0("category", i), each = 11))
    start_val = i-7
    end_val = i+3
    values = c(values,seq(start_val,end_val))
  }
  for (i in 38:40)
  {
    states_cat = c(states_cat,rep(paste0("category", i), each = 8+(40-i)))
    start_val = i-7
    end_val = 40
    values = c(values,seq(start_val,end_val))
  }
  
  return(list(states_cat,values))
}

fillTimes = function(df,expTimeMat)
{
  for(i in 1:nrow(df))
  {
    cat = as.character(df[i,3])
    startState = as.integer(substr(cat,9,nchar(cat)))
    endState = df[i,1]
    df[i,2] = expTimeMat[startState,endState]
    #print(i)
  }
  
  return(df)
}

dispStart = 30
dispEnd = 32

xx = buildCat()
expTimeMat = read.csv("ET_neweps.csv")
expTimeMat = expTimeMat[2:nrow(expTimeMat),2:ncol(expTimeMat)]
df <- data.frame(x=xx[[2]], val=numeric(406), variable=xx[[1]] )
df = fillTimes(df,expTimeMat)
dispList = paste0("category", dispStart:dispEnd)
df = df[df[,3] %in% dispList,]
#colnames(df)[3] = "Starting State"

cat = as.character(df[,3])
startState = substr(cat,9,nchar(cat))
df[,3] = startState
expPlot = ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=variable))+ geom_point(aes(colour=variable))
expPlot + labs(x = "Ending State",y = "Expected Time to State Transfer\n(days)",title = "Expected Path of ALS",colour = "Starting State")
