SELECT developer.display_name AS source, project.name AS target, issue_update.start_date, issue_update.end_date
FROM gros.project_developer
  JOIN gros.developer ON project_developer.developer_id = developer.id
  JOIN gros.project ON project_developer.project_id = project.project_id
  JOIN (
         SELECT project_id, updated_by, MIN(issue.updated) AS start_date, MAX(issue.updated) AS end_date
         FROM gros.issue GROUP BY project_id, updated_by
       ) AS issue_update ON project_developer.project_id = issue_update.project_id
    AND developer.name = issue_update.updated_by
  ORDER BY developer.display_name, project.name;
