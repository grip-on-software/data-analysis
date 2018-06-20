SELECT project_id, sprint_id, COUNT(*) AS backlog_size FROM (
	SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id
	FROM gros.issue
	JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
	AND sprint.start_date > issue.updated
) AS backlog_sprint
GROUP BY project_id, sprint_id
