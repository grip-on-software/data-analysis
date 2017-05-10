SELECT
  developer.display_name AS source,
  project.name AS target,
  developer.local_domain AS internal,
  COALESCE(SUM(commits_count.commits), 0) AS num_commits,
  SUM(issue_update.issues) AS num_issues
FROM gros.project_developer
  JOIN gros.developer ON project_developer.developer_id = developer.id
  JOIN gros.project ON project_developer.project_id = project.project_id
  JOIN (
         SELECT project_id, updated_by, COUNT(*) as issues
         FROM gros.issue GROUP BY project_id, updated_by
       ) AS issue_update ON project_developer.project_id = issue_update.project_id
    AND developer.name = issue_update.updated_by
  LEFT JOIN gros.vcs_developer ON developer.id = vcs_developer.jira_dev_id
  LEFT JOIN (
         SELECT project_id, developer_id, COUNT(DISTINCT version_id) as commits
         FROM gros.commits GROUP BY project_id, developer_id
       ) AS commits_count
    ON project_developer.project_id = commits_count.project_id
    AND vcs_developer.alias_id = commits_count.developer_id
  GROUP BY developer.display_name, project.name, developer.local_domain
  ORDER BY developer.display_name, project.name;
