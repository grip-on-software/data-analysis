SELECT project_id, CAST(commit_date AS DATE) AS commit_day, COUNT(*) AS value
FROM gros.commits
GROUP BY project_id, commit_day
