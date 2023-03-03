#--------------------------------------------------------
# CLASS PERFORMANCE
# Stores performance metrics
#--------------------------------------------------------

library(BBmisc)
library(testthat)

# Performance object.
# Result of an experiment with the following members:
#		perf - the performance values for each run (data.frame - columns are the methods, rows are the individual runs)
#		aggr - the aggregated performance value 

new_performance = function(size) {
	makeS3Obj(
		"Performance",
		perf = data.frame(row.names = 1:size),
		aggr = data.frame(row.names = 1)
	)
}

add_performance = function(perf, new_perf) {
  perf$perf = cbind(perf$perf, new_perf$perf)
  perf$aggr = cbind(perf$aggr, new_perf$aggr)
  return(perf)
}

save_performance = function(perf, method, res, task_id = NULL) {
	if (inherits(res, "ResampleResult")) {
		perf$perf[[method]] = res$measures.test[['cindex']]
		perf$aggr[[method]] = res$aggr['cindex.test.mean_narm']
	} else if (inherits(res, "BenchmarkResult")) {
		raw = getBMRPerformances(res, learner.ids = c(method))
		perf$perf[[method]] = raw[[task_id]][[method]][['cindex']]	
		aggr <- getBMRAggrPerformances(res, learner.ids = c(method))
		perf$aggr[[method]] = aggr[[1]][[1]][['cindex.test.mean_narm']]
	}
	return(perf)
}

write_performance = function(perf, result_file, suffix)
{
	write.csv(perf$perf, paste(result_file, "_perf", "_", suffix, ".csv", sep=""), row.names=TRUE)
	write.csv(perf$aggr, paste(result_file, "_aggr", "_", suffix, ".csv", sep=""), row.names=TRUE)
}

write_raw_perf = function(filename, suffix, perf) {
	ordered = scores[order(names(scores))]
	full_fname = paste(filename, "_", suffix, ".csv", sep="")
	write.table(data.frame(t(ordered)), full_fname, sep=",", append=TRUE, row.names=FALSE, col.names=!file.exists(full_fname))
}