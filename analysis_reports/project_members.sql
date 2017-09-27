SELECT
  developer.display_name AS source,
  ${s(project_name)} AS target,
  developer.local_domain AS internal,
  project.is_support_team AS support,
  COALESCE(SUM(commits_count.commits), 0) AS num_commits,
  SUM(issue_update.issues) AS num_issues
FROM gros.project_developer
  JOIN gros.developer ON project_developer.developer_id = developer.id
  JOIN gros.project ON project_developer.project_id = project.project_id
  LEFT JOIN (
         SELECT project_id, assignee, COUNT(DISTINCT issue_id) as issues
         FROM gros.issue ${s(interval_condition, field='updated')}
		 GROUP BY project_id, assignee
       ) AS issue_update ON project_developer.project_id = issue_update.project_id
    AND developer.name = issue_update.assignee
  LEFT JOIN gros.vcs_developer ON developer.id = vcs_developer.jira_dev_id
  LEFT JOIN (
         SELECT project_id, developer_id, COUNT(DISTINCT version_id) as commits
         FROM gros.commits ${s(interval_condition, field='commit_date')}
		 GROUP BY project_id, developer_id
       ) AS commits_count
    ON project_developer.project_id = commits_count.project_id
    AND vcs_developer.alias_id = commits_count.developer_id
  GROUP BY developer.display_name, project.project_id, project.name, project.is_support_team, developer.local_domain
  ORDER BY developer.display_name, project.name, project.project_id;
