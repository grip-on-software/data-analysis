SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, COALESCE(sprint.complete_date, sprint.end_date) AS date
FROM gros.sprint
JOIN gros.project ON sprint.project_id = project.project_id
