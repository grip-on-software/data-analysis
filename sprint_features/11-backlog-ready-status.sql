SELECT project_id, sprint_id, COUNT(*) AS backlog_ready_status FROM (
	SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id
	FROM gros.issue
	JOIN gros.ready_status ON issue.ready_status = ready_status.id
	JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
	WHERE issue.ready_status IS NOT NULL
	AND issue.updated < ${planned_end}
) AS backlog_ready_status
GROUP BY project_id, sprint_id
