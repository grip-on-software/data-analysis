CREATE AGGREGATE max_changelog(changelog_id INTEGER) RETURNS INTEGER LANGUAGE R {
	aggregate(changelog_id, by=list(aggr_group), FUN=max)$x
};

-- SELECT issue_id, max_changelog(changelog_id) FROM gros.issue GROUP BY issue_id;
