SELECT sprint.project_id, sprint.sprint_id,
	EXTRACT(day FROM sprint.end_date - sprint.start_date) AS sprint_days
FROM gros.sprint;
