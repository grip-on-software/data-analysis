SELECT developer.display_name, project_developer.project_id, issue_update.first_date, issue_update.last_date
FROM gros.project_developer
  JOIN gros.developer ON project_developer.developer_id = developer.id
  JOIN (
         SELECT project_id, updated_by, MIN(issue.updated) AS first_date, MAX(issue.updated) AS last_date
         FROM gros.issue GROUP BY project_id, updated_by
       ) AS issue_update ON project_developer.project_id = issue_update.project_id
    AND developer.name = issue_update.updated_by
  ORDER BY project_developer.project_id, developer.display_name;
