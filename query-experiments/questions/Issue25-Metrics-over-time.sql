SELECT project."name", first_date AS metric_date, COUNT(*) FROM gros.project,
(SELECT metric_value.project_id, metric."name", MIN(metric_value.since_date) AS first_date FROM gros.metric, gros.metric_value
	WHERE metric.metric_id = metric_value.metric_id
	GROUP BY metric_value.project_id, metric."name") AS metricdata
WHERE project.project_id = metricdata.project_id
GROUP BY project.name, first_date
ORDER BY project.name, metric_date;
