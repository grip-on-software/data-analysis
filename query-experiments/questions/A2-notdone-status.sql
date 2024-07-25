SELECT issue.story_points, COUNT(*) FROM gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata,
(SELECT DISTINCT(issue.issue_id) FROM gros.issue, gros.sprint
	WHERE issue.sprint_id = sprint.sprint_id
	AND issue.resolution <> 1 AND issue.status <> 6
	AND issue.updated < sprint.end_date) AS issuedata
WHERE issue.issue_id = maxdata.issue_id
AND issue.changelog_id = maxdata.max_changelog_id
AND issue.issue_id = issuedata.issue_id
AND issue.story_points <> 0
GROUP BY issue.story_points
ORDER BY issue.story_points;
