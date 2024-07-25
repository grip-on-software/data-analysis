SELECT project_id, metric_id, type, COUNT(*), MIN(version_id), MAX(version_id) FROM gros.metric_target GROUP BY project_id, metric_id, type;
