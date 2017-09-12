SELECT project_id, COUNT(*) AS num_versions FROM gros.fixversion
GROUP BY project_id;
