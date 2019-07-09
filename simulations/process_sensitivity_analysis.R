all = c()
for (j in 0:99){
	filename = sprintf('../results_0-100/sensitivity.DAHVage_subtype.optimx.%s.rds', j)
	load(filename)
	H1 = result$H1m
	H3 = result$H3m

	all = rbind(all, c(H1, H3))
}
output = data.frame(all)
colnames(output) = c('H1m', 'H3m')
write.csv(output, '../results_0-100/sensitivity_summary.csv', row.names=FALSE)