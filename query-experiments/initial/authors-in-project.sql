SELECT commitdata.project_id, jiradata.jira_developer_id, MIN(commitdata.min_date) AS commit_min, MAX(commitdata.max_date) AS commit_max, MIN(jiradata.min_date) AS jira_min, MAX(jiradata.max_date) AS jira_max
FROM
(SELECT project_id, developer_id, MIN(commit_date) AS min_date, MAX(commit_date) AS max_date
FROM gros.commits
GROUP BY project_id, developer_id) AS commitdata
LEFT JOIN
((SELECT project_id, updated_by AS jira_developer_id, MIN(updated) AS min_date, MAX(updated) AS max_date
FROM gros.issue
GROUP BY project_id, jira_developer_id) AS jiradata
JOIN
(gros.vcs_developer JOIN gros.developer ON vcs_developer.jira_dev_id = developer.id)
ON jiradata.jira_developer_id = developer.name)
ON commitdata.project_id = jiradata.project_id
AND commitdata.developer_id = vcs_developer.alias_id
GROUP BY commitdata.project_id, jiradata.jira_developer_id
HAVING jiradata.jira_developer_id IS NOT NULL
ORDER BY commitdata.project_id, jiradata.jira_developer_id;
