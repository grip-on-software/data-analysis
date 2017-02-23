SELECT issue.project_id, issue.sprint_id, COUNT(*) AS num_late_rank_changes
FROM gros.issue JOIN gros.sprint ON issue.sprint_id = sprint.sprint_id
WHERE issue.sprint_id <> 0 AND issue.rank_change IS NOT NULL
AND issue.updated > sprint.start_date
GROUP BY issue.project_id, issue.sprint_id
