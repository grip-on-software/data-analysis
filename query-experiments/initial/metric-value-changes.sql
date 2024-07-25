SELECT metric_id, MIN(since_date), MAX(since_date), COUNT(*) FROM 
(SELECT metric_id, since_date, MIN(value) FROM gros.metric_value GROUP BY metric_id, since_date) AS md
GROUP BY metric_id ORDER BY COUNT(*) DESC;
