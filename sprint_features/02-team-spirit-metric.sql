-- Team spirit metric
SELECT team_spirit.project_id, team_spirit.sprint_id, AVG(metric_value."value") AS team_spirit
FROM gros.metric_value, (
    SELECT metric_value.project_id, sprint.sprint_id, metric_value.metric_id, MAX(metric_value."date") AS max_date
    FROM gros.metric_value
    JOIN gros.metric
    ON metric_value.metric_id = metric.metric_id
    JOIN gros.sprint
    ON metric_value.project_id = sprint.project_id
    AND metric_value.sprint_id = sprint.sprint_id
    WHERE metric_value."value" <> -1
    AND metric.base_name = 'TeamSpirit'
    GROUP BY metric_value.project_id, sprint.sprint_id, metric_value.metric_id
) AS team_spirit
WHERE metric_value."date" = team_spirit.max_date AND metric_value.metric_id = team_spirit.metric_id
GROUP BY team_spirit.project_id, team_spirit.sprint_id
