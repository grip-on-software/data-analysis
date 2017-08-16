SELECT project_id, COUNT(DISTINCT issue_id) AS num_issues FROM gros.issue GROUP BY project_id;
