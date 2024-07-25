-- No update during sprint
SELECT issue.story_points, COUNT(*) AS frequency FROM gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata
WHERE issue.issue_id = maxdata.issue_id
AND issue.changelog_id = maxdata.max_changelog_id
AND issue.story_points <> 0
AND NOT EXISTS (SELECT DISTINCT(issue.issue_id) FROM gros.issue AS issue2, gros.sprint
	WHERE issue.issue_id = issue2.issue_id AND issue2.sprint_id = sprint.sprint_id
	AND issue2.status <> 0
	AND issue2.updated < sprint.end_date)
GROUP BY issue.story_points
ORDER BY issue.story_points;
