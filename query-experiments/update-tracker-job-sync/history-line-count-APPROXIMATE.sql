-- Not exact!
SELECT project_id, COUNT(*) FROM (SELECT DISTINCT project_id, date FROM gros.metric_value) AS dates GROUP BY project_id;
