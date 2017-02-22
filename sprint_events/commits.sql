SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, commits.commit_date AS date, 'commits' AS type
FROM gros.commits
JOIN gros.sprint ON commits.commit_date BETWEEN sprint.start_date AND sprint.end_date
JOIN gros.project ON commits.project_id = project.project_id
