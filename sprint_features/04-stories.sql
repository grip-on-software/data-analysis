SELECT project_id, sprint_id, COUNT(*) AS num_stories
FROM (
	SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id
	FROM gros.issue
	JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
	WHERE issue.type = 7
	AND issue.updated > ${sprint_open}
	AND issue.sprint_id <> 0
) AS stories
GROUP BY project_id, sprint_id
