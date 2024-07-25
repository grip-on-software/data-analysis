SELECT issue.project_id, issue.updated_by, MIN(issue.updated) AS first_update, MIN(sprint.start_date) AS first_sprint FROM gros.issue, gros.sprint
WHERE issue.project_id = sprint.project_id
GROUP BY issue.project_id, issue.updated_by
ORDER BY issue.project_id, first_update;

