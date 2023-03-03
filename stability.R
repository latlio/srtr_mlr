#--------------------------------------------------------
# CLASS STABILITY
# Stores and calculates stability metrics
#--------------------------------------------------------

library(BBmisc)

# Stability object.
# Result of a stability experiment with the following members:
#		stab - the stability values

new_stability = function() { 
	makeS3Obj(
		"Stability",
		stab = data.frame(row.names = c("Jaccard", "Dice", "Kuncheva", "Lustgarten", "Consistency"))
	)
}

add_stability = function(stab, new_stab)  {
  stab$stab = cbind(stab$stab, new_stab$stab)
  return(stab)
}
	
calc_jaccard = function(set1, set2)
{
	jaccard = 0
	numerator = length(intersect(set1, set2))
	denominator = length(union(set1, set2))
	if (denominator != 0) {
		jaccard = numerator / denominator
	}
	return(jaccard)
}

calc_dice = function(set1, set2)
{
	return(2 * length(intersect(set1, set2)) / (length(set1) + length(set2)))
}

calc_kuncheva = function(set1, set2, total)
{
	kuncheva = 0
	r = length(intersect(set1, set2))
	k = min(length(set1), length(set2))
	numerator = (r * total) - (k * k)
	denominator = k * (total - k)
	if (denominator != 0) {
		kuncheva = numerator / denominator
	}
	return(kuncheva)
}
	
calc_lustgarten = function(set1, set2, total)
{
	lustgarten = 0
#	print(paste0("Length set 1: ", length(set1)))
#	print(paste0("Length set 2: ", length(set2)))
#	print(paste0("Length intersection: ", length(intersect(set1, set2))))
	numerator = length(intersect(set1, set2)) - (length(set1) * length(set2) / total)
	denominator = min(length(set1), length(set2)) - max(0, length(set1) + length(set2) - total)
#	print(paste0("numerator = ", numerator))
#	print(paste0("denominator = ", denominator))
	if (denominator != 0) {
		lustgarten = numerator / denominator
	}
	return(lustgarten)
}

# Relative weighted consistency index - Song et al 2019, Petr Somol and Jana Novovicova 2010
calc_consistency = function(sets, num_sets, num_feats)
{
	N =  sum(lengths(sets))
	D = N %% num_feats
	H = N %% num_sets
	counts <- table(unlist(lapply(sets, unique)))
	prods = lapply(counts, function(x) x * (x-1))
	sum_prods = sum(unlist(prods))
	return((num_feats * (N - D + sum_prods) - N^2 + D^2) / (num_feats * (H^2 + num_sets * (N - H) - D) - N^2 + D^2))
}

# Use the first num_sets sets in the calculation.
# That way we can run it once with a large number of repeats and 
# do the calculation multiple times on different numbers of runs.
#
calc_stability = function(sets, num_sets, num_feats)
{
#	print("In calc_stability")
#	print(paste0("num_feats = ", num_feats))
#	print(sets)
	jaccard = 0
	dice = 0
	kuncheva = 0
	lustgarten = 0
	if (num_sets > 1) {
		for (i in 1:(num_sets - 1)) {
			for (j in (i+1):num_sets) {
				jaccard = jaccard + calc_jaccard(sets[[i]], sets[[j]])
				dice = dice + calc_dice(sets[[i]], sets[[j]])
				kuncheva = kuncheva + calc_kuncheva(sets[[i]], sets[[j]], num_feats)
				lustgarten = lustgarten + calc_lustgarten(sets[[i]], sets[[j]], num_feats)
			}
		}
		jaccard = jaccard * 2 / (num_sets * (num_sets - 1))
		dice = dice * 2 / (num_sets * (num_sets - 1))
		kuncheva = kuncheva * 2 / (num_sets * (num_sets - 1))
		lustgarten = lustgarten * 2 / (num_sets * (num_sets - 1))
		consistency = calc_consistency(sets, length(sets), num_feats)
	}
	return( c(jaccard, dice, kuncheva, lustgarten, consistency) )
}

# Calculate stability incrementally on an increasing number of features in each set
# Returns the number of features in the optimally stable set
#
calc_inc_stability = function(sets, num_sets, num_feats, stab_metric)
{
	incstab = list()
	for (i in seq(5, num_feats, by = 5)) {
		subsets = sapply(sets, function(x) {x[1:i, "name"]})		
		incstab[[as.integer(i/5)]] = calc_stability(subsets, num_sets, num_feats)
	}
	stab_vec = unlist(incstab)
	vec = stab_vec[seq(stab_metric, length(stab_vec), stab_metric)]
	return(which.max(vec) * 5)
}

save_stability = function(stab, method, sets, num_feats) {
#	if (length(sets) >= 20) {
#		for (size in seq(10, length(sets), by = 10)) {
#			stab$stab[[paste0(method, size)]] = calc_stability(sets, size, num_feats)
#		}
#	} else {
		stab$stab[method] = calc_stability(sets, length(sets), num_feats)
#	}
	return(stab)
}

write_stability = function(stab, result_file, suffix)
{
	write.csv(stab$stab, paste(result_file, "_stab", "_", suffix, ".csv", sep=""), row.names=TRUE)
}