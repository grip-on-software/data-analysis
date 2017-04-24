SELECT project_id, CAST(commit_date AS DATE) AS commit_day, COUNT(*) AS value
FROM gros.commits
JOIN gros.vcs_developer ON commits.developer_id = vcs_developer.alias_id
WHERE vcs_developer.jira_dev_id <> -1
GROUP BY project_id, commit_day
