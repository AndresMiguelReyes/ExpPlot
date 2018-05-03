### INPUT: data - the dataframe that contains the data
###        time_col - name of the column that contains the time data
###        state_col - name of the column that contains the state data
###        ID_col - name of the column that contains the unique subject IDs of each patient
###
### OUTPUT: no direct output, writes two .csv files to your current working directory
###         named "times" and states". Before running the program again, you must close
###         ALL instances of times.csv and states.csv.


extractData = function(data, time_col, state_col, ID_col)
{
  ### remove any rows with NA entries in either the state or the time cols ###
  desiredCols = c(time_col, state_col)
  completeVec = complete.cases(data[, desiredCols])
  completeData = data[completeVec, ]
  
  time_list = list()
  state_list = list()
  it = 1
  row_ind = 1
  
  for(i in 1:nrow(data))
  {
    if (i ==  nrow(data))
    {
      time_list[[row_ind]] = data[(i-(it-1)):i,time_col]
      state_list[[row_ind]] = data[(i-(it-1)):i,state_col]
    }
    
    else if(data[i,ID_col] != data[i+1,ID_col])
    {
      time_list[[row_ind]] = data[(i-(it-1)):i,time_col]
      state_list[[row_ind]] = data[(i-(it-1)):i,state_col]
      
      it = 0
      row_ind =  row_ind + 1
    }
    it = it + 1
  }
  
  #write.table(time_col, sep=",", file="times.csv",  row.names=FALSE,col.names = FALSE)
  #write.table(state_col, sep=",", file="states.csv",  row.names=FALSE, col.names = FALSE)
  
  return(list(time_list,state_list))
}