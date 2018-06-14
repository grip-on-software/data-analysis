SELECT project_id, sprint_id, value AS lines_of_code FROM (
	SELECT project_id, sprint_id, value, end_date,
		ROW_NUMBER() OVER (
			PARTITION BY project_id, sprint_id
			ORDER BY project_id, sprint_id, end_date DESC
		) AS rev_row
	FROM (
		SELECT metric_value.project_id, metric_value.sprint_id,
			metric_value.value, metric_value.date AS end_date
		FROM gros.metric_value
		JOIN gros.metric ON metric_value.metric_id = metric.metric_id
		AND metric_value.value <> -1 AND metric.base_name = 'TotalLOC'
	) AS loc_rows)
AS lines_of_code_metric
WHERE rev_row = 1
