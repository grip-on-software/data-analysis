SELECT project_id, COUNT(DISTINCT issue_id) AS num_tests FROM gros.issue
WHERE type IN (9,10,10301) GROUP BY project_id;
