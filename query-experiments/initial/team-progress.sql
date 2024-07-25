SELECT team_progress.project_id, team_progress.sprint_id, AVG(metric_value."value") AS team_progress
FROM gros.metric_value, (
    SELECT metric_value.project_id, sprint.sprint_id, metric_value.metric_id, MAX(metric_value."date") AS max_date
    FROM gros.metric_value
    JOIN gros.metric ON metric_value.metric_id = metric.metric_id
    AND metric."name" LIKE 'TeamProgress%'
    JOIN gros.sprint ON sprint.project_id = metric_value.project_id AND metric_value."date" BETWEEN sprint.start_date AND sprint.end_date
    WHERE metric_value."value" <> -1
    GROUP BY metric_value.project_id, sprint.sprint_id, metric_value.metric_id
) AS team_progress
WHERE metric_value."date" = team_progress.max_date AND metric_value.metric_id = team_progress.metric_id
GROUP BY team_progress.project_id, team_progress.sprint_id
ORDER BY team_progress.project_id, team_progress.sprint_id;
