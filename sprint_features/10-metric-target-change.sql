SELECT metric_target.project_id, sprint.sprint_id, COUNT(*) AS num_metric_target_changes
FROM gros.metric_target, gros.metric_version, gros.sprint
WHERE metric_target.project_id = metric_version.project_id AND metric_target.version_id = metric_version.version_id
AND metric_version.commit_date BETWEEN sprint.start_date AND sprint.end_date
GROUP BY metric_target.project_id, sprint.sprint_id
