SELECT sprint_metrics.project_id, sprint_metrics.sprint_id, COUNT(*) AS num_missing_metrics
FROM (
    SELECT DISTINCT metric_value.project_id, sprint.sprint_id, metric_value.metric_id FROM gros.metric_value, gros.sprint
    WHERE metric_value."date" BETWEEN sprint.start_date AND sprint.end_date
    AND metric_value.value = -1
) AS sprint_metrics
GROUP BY sprint_metrics.project_id, sprint_metrics.sprint_id
