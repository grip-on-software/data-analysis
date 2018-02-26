SELECT sprint.project_id, sprint.sprint_id, CAST(MIN(update_tracker.update_date) > ${sprint_close} AS INT) AS sprint_id_complete
FROM gros.update_tracker
JOIN gros.sprint ON update_tracker.project_id = sprint.project_id
GROUP BY sprint.project_id, sprint.sprint_id, sprint.end_date, sprint.complete_date
