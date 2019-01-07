SELECT project_id, EXTRACT(day FROM MAX(${s(sprint_close)})-MIN(start_date)) AS lifespan
FROM gros.sprint GROUP BY project_id
