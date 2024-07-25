SELECT project."name" AS project_name, real_sprint.sprint_id AS real_sprint_id, real_sprint."name" AS real_sprint_name, real_sprint.start_date AS real_start_date, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.start_date AS start_date, COUNT(*)
FROM gros.commits
JOIN gros.sprint ON commits.commit_date BETWEEN sprint.start_date AND sprint.end_date AND sprint.project_id = commits.project_id
JOIN gros.project ON commits.project_id = project.project_id
JOIN gros.sprint AS real_sprint ON commits.project_id = real_sprint.project_id AND commits.sprint_id = real_sprint.sprint_id
WHERE commits.sprint_id <> sprint.sprint_id
GROUP BY project_name, real_sprint_id, real_sprint_name, real_start_date, sprint_id, sprint_name, start_date
