SELECT sprint.project_id, sprint."name", EXTRACT(day FROM sprint.end_date - sprint2.start_date) AS overlap, sprint.start_date, sprint.end_date, sprint2."name", sprint2.start_date, sprint2.end_date
FROM gros.sprint, gros.sprint AS sprint2
WHERE sprint.project_id = sprint2.project_id AND sprint.sprint_id <> sprint2.sprint_id
AND (sprint.end_date > sprint2.start_date AND sprint.start_date < sprint2.start_date) AND EXTRACT(day FROM sprint.end_date - sprint2.start_date) > 0;
