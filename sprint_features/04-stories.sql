SELECT project_id, sprint_id, COUNT(*) AS num_stories
FROM (
	SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id
	FROM gros.issue
	WHERE issue.type = 7
	AND issue.sprint_id <> 0
) AS stories
GROUP BY project_id, sprint_id
