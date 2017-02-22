-- Sprint sequence number within project
SELECT sprint.project_id, sprint.sprint_id, COUNT(*) AS sprint_num
FROM gros.sprint, gros.sprint AS sprint2
WHERE sprint.project_id = sprint2.project_id
AND sprint.start_date >= sprint2.start_date
GROUP BY sprint.project_id, sprint.sprint_id
