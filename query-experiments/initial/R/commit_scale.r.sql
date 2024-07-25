CREATE AGGREGATE commit_scale(size_of_commit INTEGER, insertions INTEGER, deletions INTEGER, number_of_files INTEGER, number_of_lines INTEGER) RETURNS INTEGER LANGUAGE R {
	sizes <- aggregate(size_of_commit, by=list(aggr_group), FUN=median)$x
	
};
