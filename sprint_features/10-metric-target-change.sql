SELECT ${f(join_cols, "sprint")}, COUNT(*) AS num_metric_target_changes
FROM gros.metric_target, gros.metric_version, gros.${t("sprint")}
WHERE ${j(join_cols[1], "metric_target", "metric_version")}
AND metric_target.version_id = metric_version.version_id
AND ${j(join_cols[1], "metric_version", "sprint")}
AND metric_version.commit_date BETWEEN ${t("sprint")}.start_date AND ${t("sprint")}.end_date
GROUP BY ${f(join_cols, "sprint")}
