SELECT issue.project_id, developer.id, developer."name", MIN(issue.updated) FROM
gros.developer --(gros.developer FULL OUTER JOIN gros.git_developer ON developer.id = git_developer.jira_dev_id)
LEFT OUTER JOIN gros.issue ON developer."name" = issue.updated_by
GROUP BY issue.project_id, developer.id, developer."name";--, git_developer.display_name;
