SELECT ${s(project_name)} AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, ${sprint_close} AS date
FROM gros.sprint
JOIN gros.project ON sprint.project_id = project.project_id
