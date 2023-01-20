-- Team spirit metric
SELECT ${f(join_cols, "team_spirit")}, AVG(metric_value."value") AS team_spirit
FROM gros.metric_value, (
    SELECT ${f(join_cols, "metric_value")}, metric_value.metric_id, MAX(metric_value."date") AS max_date
    FROM gros.metric_value
    JOIN gros.metric
    ON metric_value.metric_id = metric.metric_id
    JOIN gros.${t("sprint")}
    ON ${j(join_cols, "metric_value", "sprint")}
    WHERE metric_value."value" <> -1
    AND metric.base_name = 'TeamSpirit'
    ${g(join_cols, "metric_value")}, metric_value.metric_id
) AS team_spirit
WHERE metric_value."date" = team_spirit.max_date AND metric_value.metric_id = team_spirit.metric_id
${g(join_cols, "team_spirit")}
