-- Number of stories in total
SELECT project_id, COUNT(DISTINCT issue_id) AS num_stories FROM gros.issue
WHERE type = 7
GROUP BY project_id;
