SELECT commits.project_id, COUNT(*) AS num_commits FROM gros.commits
GROUP BY commits.project_id HAVING commits.project_id IS NOT NULL;
