-- This query is too slow
SELECT sprint_id, COUNT(*) FROM
    (SELECT measurementdata.sprint_id, measurementdata.metric_id, measurementdata.date, measurementdata.value, metric_value.date AS earlier_date FROM
        (SELECT sprint_id, metric_id, date, value, start_date FROM gros.metric_value, gros.sprint
            WHERE date BETWEEN start_date and end_date
            AND metric_value.project_id = sprint.project_id
        ) AS measurementdata, gros.metric_value
        -- Join
        WHERE measurementdata.metric_id = metric_value.metric_id
        -- Determine earlier metric (no metrics in between)
        AND metric_value.date < measurementdata.date
        AND metric_value.date >= start_date
        AND NOT EXISTS (
            SELECT * FROM gros.metric_value AS metric_value2
            WHERE measurementdata.metric_id = metric_value2.metric_id
            AND metric_value2.date BETWEEN metric_value.date AND measurementdata.date
        )
        -- Determine changed value
        AND metric_value.value <> measurementdata.value
    ) AS alldata
GROUP BY sprint_id;
