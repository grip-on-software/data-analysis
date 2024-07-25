-- Earlier timestamp, but should have no 'extra' issue versions...
SELECT project_id, MAX(updated) AS date FROM gros.issue GROUP BY project_id;

-- union with:
SELECT project.project_id, MAX(date) AS date FROM gros.comment JOIN gros.issue ON issue.issue_id = comment.issue_id JOIN gros.project ON issue.project_id = project.project_id GROUP BY project.project_id;

-- union with:
SELECT project.project_id, MAX(updated_date) AS date FROM gros.comment JOIN gros.issue ON issue.issue_id = comment.issue_id JOIN gros.project ON issue.project_id = project.project_id GROUP BY project.project_id;

-- and take the mamximum (group by project id)
