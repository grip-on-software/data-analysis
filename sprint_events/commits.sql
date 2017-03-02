SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, commits.commit_date AS date
FROM gros.commits
JOIN gros.sprint ON commits.sprint_id = sprint.sprint_id
JOIN gros.project ON commits.project_id = project.project_id
