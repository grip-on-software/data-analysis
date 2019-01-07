SELECT ${s(project_name)} AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.start_date AS date, ${s(sprint_close)} AS end_date, board_id
FROM gros.sprint
JOIN gros.project ON sprint.project_id = project.project_id
