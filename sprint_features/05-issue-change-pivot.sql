SELECT issue.project_id, sprint.sprint_id, AVG(EXTRACT(day FROM issue.updated - sprint.start_date)) AS avg_change_day
FROM gros.issue, gros.sprint
WHERE issue.sprint_id = sprint.sprint_id
GROUP BY issue.project_id, sprint.sprint_id
