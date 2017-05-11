SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date
FROM gros.issue
LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
JOIN gros.project ON issue.project_id = project.project_id
WHERE issue.rank_change IS NOT NULL AND updated < ${sprint_close}
ORDER BY project.project_id, sprint.start_date, issue.updated
