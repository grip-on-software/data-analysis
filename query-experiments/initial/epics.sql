SELECT issue.project_id, issue.key, COUNT(*) FROM gros.issue
JOIN (SELECT issue.key, MAX(issue.changelog_id) AS changelog_id FROM gros.issue GROUP BY issue.key) AS issue2
ON issue.key = issue2.key AND issue.changelog_id = issue2.changelog_id
RIGHT OUTER JOIN (
	SELECT issue_id, epic, MAX(story_points) AS story_points, MAX(changelog_id) AS changelog_id FROM gros.issue
	WHERE epic IS NOT NULL
	GROUP BY issue_id, epic
) AS subtask
ON subtask.epic = issue.key WHERE issue.type = 6
GROUP BY issue.project_id, issue.key;