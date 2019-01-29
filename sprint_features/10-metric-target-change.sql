SELECT ${f(join_cols, "sprint")}, COUNT(*) AS num_metric_target_changes
FROM gros.metric_target, gros.metric_version, gros.${t("sprint")}
WHERE ${j(join_cols, "metric_target", "metric_version", 1)}
AND metric_target.version_id = metric_version.version_id
AND ${j(join_cols, "metric_version", "sprint", 1)}
AND metric_version.commit_date BETWEEN ${t("sprint")}.start_date AND ${s(sprint_close)}
${s(project_condition, project="metric_target")}
${g(join_cols, "sprint")}
