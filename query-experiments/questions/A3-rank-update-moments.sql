SELECT issue.project_id, EXTRACT(day FROM updated - sprint.start_date) AS sprint_day, EXTRACT(hour FROM updated) AS sprint_hour, COUNT(*) AS frequency
FROM gros.issue, gros.sprint
WHERE issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
AND rank_change IS NOT NULL AND updated < sprint.end_date
GROUP BY issue.project_id, sprint_day, sprint_hour HAVING COUNT(*) > 5
ORDER BY issue.project_id, sprint_day, sprint_hour;
