SELECT project_id, metric.metric_id, name, COUNT(DISTINCT(since_date)) AS changes
FROM gros.metric_value, gros.metric
WHERE metric_value.metric_id = metric.metric_id
GROUP BY project_id, metric.metric_id, name HAVING COUNT(DISTINCT(since_date)) > 1
ORDER BY project_id, metric.metric_id;
