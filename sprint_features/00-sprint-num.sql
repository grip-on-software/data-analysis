-- Sprint sequence number within project
SELECT sprint.project_id, sprint.sprint_id, COUNT(*) AS sprint_num
FROM (gros.sprint
	JOIN gros.project ON sprint.project_id = project.project_id
), gros.sprint AS sprint2
WHERE sprint.project_id = sprint2.project_id
--AND ${sprint_open} >= sprint2.start_date
AND sprint.sprint_id >= sprint2.sprint_id
${s(sprint_conditions)}
${s(sprint_conditions, sprint='sprint2')}
GROUP BY sprint.project_id, sprint.sprint_id
