SELECT ${f(join_cols, "sprint_metrics")}, COUNT(*) AS num_missing_metrics
FROM (
    SELECT DISTINCT ${f(join_cols, "metric_value")}, metric_value.metric_id
	FROM gros.metric_value
    WHERE metric_value.value <= -1 AND metric_value.sprint_id <> 0
	${s(project_condition, project="metric_value")}
) AS sprint_metrics
${g(join_cols, "sprint_metrics")}
