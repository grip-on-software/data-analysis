SELECT project_id, updated_by, developer.id, vcs_developer.display_name, COUNT(*) AS updates
FROM gros.issue
LEFT OUTER JOIN gros.developer ON updated_by = developer.name
LEFT OUTER JOIN gros.vcs_developer ON developer.id = git_developer.jira_dev_id
WHERE rank_change is not NULL AND sprint_id <> 0
GROUP BY project_id, updated_by, developer.id, git_developer.display_name
ORDER BY project_id, updates DESC;
