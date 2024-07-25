SELECT project_id, MIN(start_date) AS min_start_date FROM gros.sprint GROUP BY project_id;
