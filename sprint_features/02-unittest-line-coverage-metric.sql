-- Unittest coverage metric
SELECT project_id, sprint_id, AVG(value) AS unittest_line_coverage FROM (
	SELECT project_id, sprint_id, domain_name, value, end_date,
		ROW_NUMBER() OVER (
			PARTITION BY project_id, sprint_id
			ORDER BY project_id, sprint_id, end_date DESC
		) AS rev_row
	FROM (
		SELECT metric_value.project_id, metric_value.sprint_id,
			metric_value.value, metric.domain_name,
			MAX(metric_value.date) AS end_date
		FROM gros.metric_value
		JOIN gros.metric ON metric_value.metric_id = metric.metric_id
		WHERE metric_value.value <> -1 AND metric.base_name = 'UnittestLineCoverage'
		GROUP BY metric_value.project_id, metric_value.sprint_id,
			metric_value.value, metric.domain_name
	) AS unittest_rows)
AS unittest_metric
WHERE rev_row = 1
GROUP BY project_id, sprint_id
