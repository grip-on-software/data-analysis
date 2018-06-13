SELECT project_id, SUM(jobs) AS num_jobs FROM gros.jenkins GROUP BY project_id;
