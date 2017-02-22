SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.start_date AS date, sprint.end_date AS end_date 
FROM gros.sprint
JOIN gros.project ON sprint.project_id = project.project_id
