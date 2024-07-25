SELECT story_points, cast(coalesce(notdone.notdone_num, 0) as float)/(coalesce(notdone.notdone_num,0)+coalesce(done.done_num,0)) AS ratio
FROM (SELECT issue.story_points, COUNT(*) AS notdone_num FROM gros.issue,
	(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata
	WHERE issue.issue_id = maxdata.issue_id
	AND issue.changelog_id = maxdata.max_changelog_id
	AND issue.resolution <> 1 AND issue.status <> 6
	AND issue.story_points <> 0
	GROUP BY issue.story_points) AS notdone NATURAL FULL OUTER JOIN
(SELECT issue.story_points, COUNT(*) AS done_num FROM gros.issue,
	(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata
	WHERE issue.issue_id = maxdata.issue_id
	AND issue.changelog_id = maxdata.max_changelog_id
	AND (issue.resolution = 1 OR issue.status = 6)
	AND issue.story_points <> 0
	GROUP BY issue.story_points) AS done
ORDER BY story_points;
