SELECT commits.project_id, commits.developer_id, MIN(commits.commit_date) AS first_update, MIN(sprint.start_date) AS first_sprint FROM gros.commits, gros.sprint
WHERE commits.project_id = sprint.project_id
GROUP BY commits.project_id, commits.developer_id
ORDER BY commits.project_id, first_update;

