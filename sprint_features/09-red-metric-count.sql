SELECT sprint_metrics.project_id, sprint_metrics.sprint_id, COUNT(*) AS num_red_metrics
FROM (
    SELECT DISTINCT
        metric_value.project_id, metric_value.sprint_id, metric_value.metric_id
    FROM gros.metric_value
    JOIN gros.sprint
    ON metric_value.project_id = sprint.project_id
    AND metric_value.sprint_id = sprint.sprint_id
    WHERE metric_value.category = 'red'
) AS sprint_metrics
GROUP BY sprint_metrics.project_id, sprint_metrics.sprint_id
