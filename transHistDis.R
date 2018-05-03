states = read.csv('alsfrs.csv',header=T, stringsAsFactors=T)
states[is.na(states$ALSFRS_Total),"ALSFRS_Total"] = states[is.na(states$ALSFRS_Total),"ALSFRS_R_Total"] 

states = states[!is.na(states$ALSFRS_Total),]
states[!is.na(states$ALSFRS_Delta),]

states$ALSFRS_Delta = floor(states$ALSFRS_Delta)
states$ALSFRS_Total = floor(states$ALSFRS_Total)

time_series_list = extractData(states, 13, 14, 1)
states_list = time_series_list[[2]]
state_change_vec = sapply(states_list,diff)
state_change_vec = unlist(state_change_vec)

hist(state_change_vec, xlab = "size of state jump", main="frequency of state jumps")

freq_table = table(state_change_vec)
freq_table = freq_table/sum(freq_table)
freq_table = as.data.frame(freq_table)[order(-freq_table),]
freq_table_g1 = freq_table[freq_table[,2] > .01,]
freq_table_g1 = freq_table_g1[order(freq_table_g1[,1]),]
barplot(freq_table_g1[,2],names = freq_table_g1[,1],ylab="frequency",xlab="size of state jump",main="frequency of state jumps with greater than 1 percent likelihood")
