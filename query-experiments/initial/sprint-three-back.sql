SELECT sprint.sprint_id, sprint.project_id, sprint.name, sprint.start_date, sprint.end_date, prevsprints.start_date AS third_sprint_start, COUNT(*)
FROM gros.sprint, gros.sprint AS prevsprints, gros.sprint AS sprint3
WHERE prevsprints.project_id = sprint.project_id AND sprint3.project_id = sprint.project_id
AND prevsprints.start_date < sprint.start_date
AND sprint3.start_date BETWEEN prevsprints.start_date AND sprint.start_date
GROUP BY sprint.sprint_id, sprint.project_id, sprint.name, sprint.start_date, sprint.end_date, prevsprints.start_date
HAVING COUNT(*) = 3
ORDER BY sprint.project_id, sprint.start_date;
