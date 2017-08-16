SELECT project_id, COUNT(*) AS num_merges FROM gros.commits
WHERE type = 'merge' GROUP BY project_id
