#--------------------------------------------------------
# CLASS FEATURE SELECTION
# Stores feature selections
#--------------------------------------------------------

library(BBmisc)
library(testthat)

# Featsel object.
# Result of an experiment with the following members:
#		feats - the features selected in each run (list of character vectors)

new_features = function(task) {
	makeS3Obj(
		"Features",
		featsel      = data.frame(row.names = getTaskFeatureNames(task)),	
		featsel_aggr = data.frame(row.names = getTaskFeatureNames(task)),
		featnames    = list()
	)
}

add_features = function(feats, new_feats) {
  feats$featsel = cbind(feats$featsel, new_feats$featsel)
  feats$featsel_aggr = cbind(feats$featsel_aggr, new_feats$featsel_aggr)
  return(feats)
}

save_features = function(feats, method, names, scores, num_models) {
	for (i in 1:num_models) {
		col_name = paste0(method, "-", i)
		feats$featsel[, col_name] = 0
		if (i <= length(names)) {
			x = which(!is.na(names[[i]]))
			labels = names[[i]][x]
			if (is.null(scores))
				feats$featsel[labels, col_name] = 1
			else
				feats$featsel[labels, col_name] = scores[[i]][x]
			feats$featnames[[i]] = labels
		}
	}
	cols = paste0(method, "-", 1:num_models)
	feats$featsel_aggr[, method] = apply(feats$featsel[,cols], 1, function(x) mean(x, na.rm=TRUE))
	return(feats)
}
	
write_features = function(feats, result_file, suffix) {
	feats$featsel[["Total"]] = apply(feats$featsel, 1, function(x) sum(x, na.rm=TRUE))
	write.csv(feats$featsel, paste0(result_file, "_featsel", "_", suffix, ".csv"), row.names=TRUE)
	write.csv(feats$featsel_aggr, paste0(result_file, "_featsel_aggr", "_", suffix, ".csv"), row.names=TRUE)
}

# scores is a named vector
write_raw_features = function(filename, suffix, scores) {
	ordered = scores[order(names(scores))]
	full_fname = paste(filename, "_", suffix, ".csv", sep="")
	incl_names = (file.size(full_fname) == 0)
	write.table(data.frame(t(ordered)), full_fname, sep=",", append=TRUE, row.names=FALSE, col.names=incl_names)
}
