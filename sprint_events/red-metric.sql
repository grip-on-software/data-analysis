SELECT DISTINCT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, since_date AS date
FROM gros.metric_value
JOIN gros.project ON metric_value.project_id = project.project_id
LEFT OUTER JOIN gros.sprint ON metric_value.project_id = sprint.project_id AND metric_value.since_date BETWEEN sprint.start_date AND sprint.end_date
WHERE category = 'red' AND EXTRACT(day FROM date - since_date) > 7
