-- Number of issues with distinct titles (to avoid automated subtasks)
SELECT project_id, COUNT(DISTINCT title) AS num_issues FROM gros.issue
GROUP BY project_id;
